package WWW::PAUSE::Simple;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(
                       upload_file
                       list_files
                       delete_files
                       undelete_files
                       reindex
                       set_password
                       set_account_info
               );

use Perinci::Object;

our %SPEC;

our %common_args = (
    username => {
        summary => 'PAUSE ID',
        schema  => ['str*', match=>'\A\w{2,9}\z', max_len=>9],
        req     => 1,
        tags    => ['common'],
    },
    password => {
        summary => 'PAUSE password',
        schema  => 'str*',
        is_password => 1,
        req     => 1,
        tags    => ['common'],
    },
);

our %detail_arg = (
    detail => {
        summary => 'Whether to return detailed records',
        schema  => 'bool',
    },
);

our %file_arg = (
    file => {
        summary => 'File name',
        schema  => ['array*', of=>'str*'],
        req => 1,
        pos => 0,
        greedy => 1,
    },
);

our %file_opt_arg = (
    file => {
        summary => 'File name',
        schema  => ['array*', of=>'str*'],
        pos => 0,
        greedy => 1,
    },
);

our %all_arg = (
    all => {
        summary => 'Apply to all files',
        schema  => 'bool',
    },
);

$SPEC{':package'} = {
    v => 1.1,
    summary => 'An API for PAUSE',
};

sub _request {
    require HTTP::Request::Common;

    my %args = @_;

    state $ua = do {
        require LWP::UserAgent;
        LWP::UserAgent->new;
    };
    my $req = HTTP::Request::Common::POST(
        "https://pause.perl.org/pause/authenquery",
        @{ $args{post_data} });
    $req->authorization_basic($args{username}, $args{password});

    $ua->request($req);
}

sub _htres2envres {
    my $res = shift;
    [$res->code, $res->message, $res->content];
}

$SPEC{upload_file} = {
    v => 1.1,
    summary => 'Upload file(s) to your PAUSE account',
    args => {
        %common_args,
        %file_arg,
        subdir => {
            summary => 'Subdirectory to put the file(s) into',
            schema  => 'str*',
            default => '',
        },
    },
};
sub upload_file {
    require File::Basename;

    my %args = @_;
    my $files  = $args{file}
        or return [400, "Please specify at least one file"];
    my $subdir = $args{subdir} // '';

    my $envres = envresmulti();

    for my $file (@$files) {
        my $res;
        {
            unless (-f $file) {
                $res = [404, "No such file"];
                last;
            }

            my $upload_res = _request(
                %args,
                post_data => [
                    Content_Type => 'form-data',
                    Content => {
                        HIDDENNAME                        => $args{username},
                        CAN_MULTIPART                     => 0,
                        pause99_add_uri_upload            => File::Basename::basename($file),
                        SUBMIT_pause99_add_uri_httpupload => " Upload this file from my disk ",
                        pause99_add_uri_uri               => "",
                        pause99_add_uri_httpupload        => [$file],
                        (length($subdir) ? (pause99_add_uri_subdirtext => $subdir) : ()),
                    },
                ]
            );
            if (!$upload_res->is_success) {
                $res = _htres2envres($upload_res);
                last;
            }
            if ($res->content !~ m!<h3>Submitting query</h3>\s*<p>(.+?)</p>!s) {
                $res = [543, "Can't scrape upload status from response", $upload_res->content];
                last;
            }
            my $str = $1;
            if ($str =~ /Query succeeded/) {
                $res = [200, "OK", undef, {"func.raw_status" => $str}];
            } else {
                $res = [500, "Failed: $str"];
            }
        }
        $res->[3] //= {};
        $res->[3]{item_id} = $file;
        $envres->add_result($res->[0], $res->[1], $res->[3]);
    }
    $envres->as_struct;
}

$SPEC{list_files} = {
    v => 1.1,
    summary => 'List files on your PAUSE account',
    args => {
        %common_args,
        %detail_arg,
        query => {
            summary => 'Filter result by keywords/string',
            schema => ['array*', of=>'str*'],
            pos => 0,
            greedy => 1,
            tags => ['category:filtering'],
        },
        del => {
            summary => 'Only list files which are scheduled for deletion',
            'summary.alt.bool.not' => 'Only list files which are not scheduled for deletion',
            schema => 'bool',
            tags => ['category:filtering'],
        },
    },
};
sub list_files {
    require DateTime::Format::DateParse; # XXX any better module?

    my %args = @_;
    my $q   = $args{query} // [];
    my $del = $args{del};

    my $res = _request(
        %args,
        post_data => [{ACTION=>'show_files'}],
    );
    return _htres2envres($res) unless $res->is_success;
    return [543, "Can't scrape list of files from response",
            $res->content]
        unless $res->content =~ m!<h3>Files in directory.+</h3><pre>(.+)</pre>!s;
    my $str = $1;
    my @files;
  REC:
    while ($str =~ m!(?:\A |<br/> )(.+?)\s+(\d+)\s+(Scheduled for deletion \(due at )?(\w+, \d\d \w+ \d{4} \d\d:\d\d:\d\d GMT)!g) {

        my $time = DateTime::Format::DateParse->parse_datetime($4);
        if ($time) {
            $time = $time->epoch;
        } else {
            $time = 0;
        }

        my $rec = {
            name  => $1,
            size  => $2,
            scheduled_for_deletion => $3 ? 1:0,
        };
        if ($3) {
            $rec->{deletion_time} = $time;
        } else {
            $rec->{mtime} = $time;
        }

        # filter
        if (@$q) {
            for (@$q) {
                next REC unless index(lc($rec->{name}), lc($_)) >= 0;
            }
        }
        if (defined $del) {
            next if $del xor $rec->{scheduled_for_deletion};
        }

        push @files, $args{detail} ? $rec : $rec->{name};

    }
    [200, "OK", \@files];
}

sub _delete_or_undelete_or_reindex_files {
    my $which = shift;
    my %args = @_;

    my $files = $args{file} // [];
    my $all   = $args{all};
    if (!$all && !@$files) { return [400, "Please specify at least one file (or specify --all)"] }
    if ( $all &&  @$files) { return [400, "Please don't specify any file if you use --all"] }

    if ($all) {
        my $listres = list_files(%args);
        return [500, "Can't list files: $listres->[0] - $listres->[1]"]
            unless $listres->[0] == 200;
        $files = [grep {
            !/CHECKSUMS$/ && ($which ne 'reindex' || !/\.readme$/)
                          } @{ $listres->[2] }];
    }

    my $res = _request(
        %args,
        post_data => [
            [
                HIDDENNAME                => $args{username},
                ($which eq 'delete'   ? (SUBMIT_pause99_delete_files_delete   => "Delete"  ) : ()),
                ($which eq 'undelete' ? (SUBMIT_pause99_delete_files_undelete => "Undelete") : ()),
                ($which eq 'reindex'  ? (SUBMIT_pause99_reindex_delete        => "Reindex" ) : ()),
                ($which =~ /delete/   ? (pause99_delete_files_FILE => $files) : ()),
                ($which eq 'reindex'  ? (pause99_reindex_FILE => $files) : ()),
            ],
        ],
    );
    return _htres2envres($res) unless $res->is_success;
    return [543, "Can't scrape $which status from response", $res->content]
        unless $res->content =~ m!<h3>Files in directory!s;
    [200,"OK"];
}

$SPEC{delete_files} = {
    v => 1.1,
    summary => 'Delete files',
    description => <<'_',

When a file is deleted, it is not immediately deleted but has
scheduled_for_deletion status for 72 hours, then deleted. During that time, the
file can be undeleted.

_
    args => {
        %common_args,
        %file_opt_arg,
        %all_arg,
    },
};
sub delete_files {
    my %args = @_; # only for DZP::Rinci::Wrap
    _delete_or_undelete_or_reindex_files('delete', @_);
}

$SPEC{undelete_files} = {
    v => 1.1,
    summary => 'Undelete files',
    description => <<'_',

When a file is deleted, it is not immediately deleted but has
scheduled_for_deletion status for 72 hours, then deleted. During that time, the
file can be undeleted.

_
    args => {
        %common_args,
        %file_opt_arg,
        %all_arg,
    },
};
sub undelete_files {
    my %args = @_; # only for DZP::Rinci::Wrap
    _delete_or_undelete_or_reindex_files('undelete', @_);
}

$SPEC{reindex_files} = {
    v => 1.1,
    summary => 'Force reindexing',
    args => {
        %common_args,
        %file_opt_arg,
        %all_arg,
    },
};
sub reindex_files {
    my %args = @_; # only for DZP::Rinci::Wrap
    _delete_or_undelete_or_reindex_files('reindex', @_);
}

$SPEC{set_password} = {
    v => 1.1,
    args => {
        %common_args,
    },
};
sub set_password {
    my %args = @_;
    [501, "Not yet implemented"];
}

$SPEC{set_account_info} = {
    v => 1.1,
    args => {
        %common_args,
    },
};
sub set_account_info {
    my %args = @_;
    [501, "Not yet implemented"];
}


1;
# ABSTRACT:

=head1 SYNOPSIS


=head1 DESCRIPTION

B<STATUS: Experimental.>

This module provides several API functions for performing common tasks on PAUSE.
It comes with a CLI script L<pause>.


=head1 SEE ALSO

L<CPAN::Uploader> which also does uploading from CLI.

L<WWW::PAUSE::CleanUpHomeDir> which can clean old releases from your PAUSE
account (but no CLI). A similar subcommand will be added to C<pause> (e.g.
C<pause clean>), which will provide an option to specify the number of old
releases to keep.

=cut

package WWW::PAUSE::Simple;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';
use Exporter qw(import);
our @EXPORT_OK = qw(
                       upload_file
                       list_files
                       delete_files
                       undelete_files
                       reindex_files
                       list_dists
                       delete_old_releases
                       set_password
                       set_account_info
               );

use Perinci::Object;

our %SPEC;

our $re_archive_ext = qr/(?:tar|tar\.(?:Z|gz|bz2|xz)|zip|rar)/;

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

our %detail_l_arg = (
    detail => {
        summary => 'Whether to return detailed records',
        schema  => 'bool',
        cmdline_aliases => {l=>{}},
    },
);

our %files_arg = (
    files => {
        summary => 'File names/wildcard patterns',
        'summary.alt.plurality.singular' => 'File name/wildcard pattern',
        schema  => ['array*', of=>'str*', min_len=>1],
        'x.name.is_plural' => 1,
        req => 1,
        pos => 0,
        greedy => 1,
    },
);

our %file_opt_arg = (
    files => {
        summary => 'File names/wildcard patterns',
        'summary.alt.plurality.singular' => 'File name/wildcard pattern',
        schema  => ['array*', of=>'str*'],
        'x.name.is_plural' => 1,
        pos => 0,
        greedy => 1,
    },
);

$SPEC{':package'} = {
    v => 1.1,
    summary => 'An API for PAUSE',
};

sub _common_args {
    my $args = shift;
    (username=>$args->{username}, password=>$args->{password});
}

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
        %files_arg,
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
    my $files  = $args{files}
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

            $log->tracef("Uploading %s ...", $file);
            my $httpres = _request(
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
            if (!$httpres->is_success) {
                $res = _htres2envres($httpres);
                last;
            }
            if ($httpres->content !~ m!<h3>Submitting query</h3>\s*<p>(.+?)</p>!s) {
                $res = [543, "Can't scrape upload status from response", $httpres->content];
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
        $log->tracef("Result of upload: %s", $res);
        $envres->add_result($res->[0], $res->[1], $res->[3]);
    }
    $envres->as_struct;
}

$SPEC{list_files} = {
    v => 1.1,
    summary => 'List files on your PAUSE account',
    args => {
        %common_args,
        %detail_l_arg,
        %file_opt_arg,
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
    require Regexp::Wildcards;
    require String::Wildcard::Bash;

    my %args  = @_;
    my $q   = $args{files} // [];
    my $del = $args{del};

    my $httpres = _request(
        %args,
        post_data => [{ACTION=>'show_files'}],
    );

    # convert wildcard patterns in arguments to regexp
    $q = [@$q];
    for (@$q) {
        next unless String::Wildcard::Bash::contains_wildcard($_);
        my $re = Regexp::Wildcards->new(type=>'unix')->convert($_);
        $re = qr/\A($re)\z/;
        $_ = $re;
    }

    return _htres2envres($httpres) unless $httpres->is_success;
    return [543, "Can't scrape list of files from response",
            $httpres->content]
        unless $httpres->content =~ m!<h3>Files in directory.+</h3><pre>(.+)</pre>!s;
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
            is_scheduled_for_deletion => $3 ? 1:0,
        };
        if ($3) {
            $rec->{deletion_time} = $time;
        } else {
            $rec->{mtime} = $time;
        }

        # filter by requested file/wildcard
      FILTER_QUERY:
        {
            last unless @$q;
            for (@$q) {
                if (ref($_) eq 'Regexp') {
                    last FILTER_QUERY if $rec->{name} =~ $_;
                } else {
                    last FILTER_QUERY if $rec->{name} eq $_;
                }
            }
            # nothing matches
            next REC;
        }
        if (defined $del) {
            next REC if $del xor $rec->{is_scheduled_for_deletion};
        }

        push @files, $args{detail} ? $rec : $rec->{name};

    }
    my %resmeta;
    if ($args{detail}) {
        $resmeta{format_options} = {
            any => {
                table_column_orders => [[qw/name size mtime is_scheduled_for_deletion deletion_time/]],
            },
        };
    }
    [200, "OK", \@files, \%resmeta];
}

$SPEC{list_dists} = {
    v => 1.1,
    summary => 'List distributions on your PAUSE account',
    description => <<'_',

Distribution names will be extracted from tarball/zip filenames.

Unknown/unparseable filenames will be skipped.

_
    args => {
        %common_args,
        %detail_l_arg,
        newest => {
            schema => 'bool',
            summary => 'Only show newest non-dev version',
            description => <<'_',

Dev versions will be skipped.

_
        },
        newest_n => {
            schema => ['int*', min=>1],
            summary => 'Only show this number of newest non-dev versions',
            description => <<'_',

Dev versions will be skipped.

_
        },
    },
};
sub list_dists {
    require List::MoreUtils;
    require Version::Util;
    use experimental 'smartmatch';

    my %args  = @_;

    my $res = list_files(_common_args(\%args), del=>0);
    return [500, "Can't list files: $res->[0] - $res->[1]"] if $res->[0] != 200;

    my $newest_n;
    if ($args{newest_n}) {
        $newest_n = $args{newest_n};
    } elsif ($args{newest}) {
        $newest_n = 1;
    }

    my @dists;
    for my $file (@{$res->[2]}) {
        if ($file =~ m!/!) {
            $log->debugf("Skipping %s: under a subdirectory", $file);
            next;
        }
        unless ($file =~ /\A
                          (\w+(?:-\w+)*)
                          -v?(\d+(?:\.\d+){0,2}(_\d+|-TRIAL)?)
                          \.$re_archive_ext
                          \z/ix) {
            $log->debugf("Skipping %s: doesn't match release regex", $file);
            next;
        }
        my ($dist, $version, $is_dev) = ($1, $2, $3);
        next if $is_dev && $newest_n;
        push @dists, {
            name => $dist,
            file => $file,
            version => $version,
            is_dev_version => $is_dev ? 1:0,
        };
    }

    my @old_files;
    if ($newest_n) {
        my %dist_versions;
        for my $dist (@dists) {
            push @{ $dist_versions{$dist->{name}} }, $dist->{version};
        }
        for my $dist (keys %dist_versions) {
            $dist_versions{$dist} = [
                sort { -Version::Util::cmp_version($a, $b) }
                    @{ $dist_versions{$dist} }];
            if (@{ $dist_versions{$dist} } > $newest_n) {
                $dist_versions{$dist} = [splice(
                    @{ $dist_versions{$dist} }, 0, $newest_n)];
            }
        }
        my @old_dists = @dists;
        @dists = ();
        for my $dist (@old_dists) {
            if ($dist->{version} ~~ @{ $dist_versions{$dist->{name}} }) {
                push @dists, $dist;
            } else {
                push @old_files, $dist->{file};
            }
        }
    }

    unless ($args{detail}) {
        @dists = List::MoreUtils::uniq(map { $_->{name} } @dists);
    }

    my %resmeta;
    if ($newest_n) {
        $resmeta{"func.old_files"} = \@old_files;
    }
    if ($args{detail}) {
        $resmeta{format_options} = {
            any => {
                table_column_orders => [[qw/name version is_dev_version file/]],
            },
        };
    }
    [200, "OK", \@dists, \%resmeta];
}

$SPEC{delete_old_releases} = {
    v => 1.1,
    summary => 'Delete older versions of distributions on your PAUSE account',
    description => <<'_',

Developer releases will not be deleted.

To delete developer releases, you can use `delete_files` (rm), e.g. from the
command line:

    % pause rm 'My-Module-*TRIAL*'; # delete a dist's trial releases
    % pause rm '*TRIAL*' '*_*'; # delete all files containing TRIAL or underscore

_
    args => {
        %common_args,
        %detail_l_arg,
        num_keep => {
            schema => ['int*', min=>1],
            default => 1,
            summary => 'Number of new versions (including newest) to keep',
            cmdline_aliases => { n=>{} },
            description => <<'_',

1 means to only keep the newest version, 2 means to keep the newest and the
second newest, and so on.

_
        },
    },
    features => {dry_run=>1},
};
sub delete_old_releases {
    my %args = @_;

    my $res = list_dists(_common_args(\%args), newest_n=>$args{num_keep}//1);
    return [500, "Can't list dists: $res->[0] - $res->[1]"] if $res->[0] != 200;
    my $old_files = $res->[3]{'func.old_files'};

    return [304, "No older releases", undef,
            {'cmdline.result'=>'There are no older releases to delete'}]
        unless @$old_files;
    my @to_delete;
    for my $file (@$old_files) {
        $file =~ s/\.$re_archive_ext\z//;
        push @to_delete, "$file.*";
    }
    $res = delete_files(_common_args(\%args),
                        files=>\@to_delete, -dry_run=>$args{-dry_run});
    return $res if $res->[0] != 200 || $args{-dry_run};
    my $deleted_files = $res->[3]{'func.files'} // [];
    if (@$deleted_files) {
        $res->[3]{'cmdline.result'} = $deleted_files;
    } else {
        $res->[3]{'cmdline.result'} = 'Deleted 0 files';
    }
    $res;
}

sub _delete_or_undelete_or_reindex_files {
    use experimental 'smartmatch';
    require Regexp::Wildcards;
    require String::Wildcard::Bash;

    my $which = shift;
    my %args = @_;

    my $files0 = $args{files} // [];
    return [400, "Please specify at least one file"] unless @$files0;

    my @files;
    {
        my $listres;
        for my $file (@$files0) {
            if (String::Wildcard::Bash::contains_wildcard($file)) {
                unless ($listres) {
                    $listres = list_files(_common_args(\%args));
                    return [500, "Can't list files: $listres->[0] - $listres->[1]"]
                        unless $listres->[0] == 200;
                }
                my $re = Regexp::Wildcards->new(type=>'unix')->convert($file);
                $re = qr/\A($re)\z/;
                for my $f (@{$listres->[2]}) {
                    push @files, $f if $f =~ $re && !($f ~~ @files);
                }
            } else {
                push @files, $file;
            }
        }
    }

    unless (@files) {
        return [304, "No files to process"];
    }

    if ($args{-dry_run}) {
        $log->warnf("[dry-run] %s %s", $which, \@files);
        return [200, "OK (dry-run)"];
    } else {
        $log->infof("%s %s ...", $which, \@files);
    }

    my $httpres = _request(
        %args,
        post_data => [
            [
                HIDDENNAME                => $args{username},
                ($which eq 'delete'   ? (SUBMIT_pause99_delete_files_delete   => "Delete"  ) : ()),
                ($which eq 'undelete' ? (SUBMIT_pause99_delete_files_undelete => "Undelete") : ()),
                ($which eq 'reindex'  ? (SUBMIT_pause99_reindex_delete        => "Reindex" ) : ()),
                ($which =~ /delete/   ? (pause99_delete_files_FILE => \@files) : ()),
                ($which eq 'reindex'  ? (pause99_reindex_FILE => \@files) : ()),
            ],
        ],
    );
    return _htres2envres($httpres) unless $httpres->is_success;
    return [543, "Can't scrape $which status from response", $httpres->content]
        unless $httpres->content =~ m!<h3>Files in directory!s;
    [200,"OK", undef, {'func.files'=>\@files}];
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
        %files_arg,
    },
    features => {dry_run=>1},
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
        %files_arg,
    },
    features => {dry_run=>1},
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
        %files_arg,
    },
    features => {dry_run=>1},
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

This module provides several API functions for performing common tasks on PAUSE.
There is also a CLI script L<pause> distributed separately in L<App::pause>.


=head1 SEE ALSO

L<CPAN::Uploader> which also does uploading from CLI.

L<WWW::PAUSE::CleanUpHomeDir> which can clean old releases from your PAUSE
account (CLI example is provided script).

L<https://perlancar.wordpress.com/2015/03/25/interacting-with-pause-using-cli/>

=cut

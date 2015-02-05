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

our %SPEC;

our %common_args = (
    username => {
        summary => 'PAUSE ID',
        schema  => 'str*',
        req     => 1,
    },
    password => {
        summary => 'PAUSE password',
        schema  => 'str*',
        is_password => 1,
        req     => 1,
    },
);

our %detail_arg = (
    detail => {
        summary => 'Whether to return detailed records',
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
    summary => 'Upload a file to your PAUSE account',
    args => {
        %common_args,
        file => {
            summary => 'Path to file to upload',
            schema => 'str*',
            req => 1,
            pos => 0,
        },
        subdir => {
            summary => 'Subdirectory to put the file into',
            schema  => 'str*',
            default => '',
        },
    },
};
sub upload_file {
    require File::Basename;

    my %args = @_;
    my $file   = $args{file};
    my $subdir = $args{subdir} // '';

    (-f $file) or return [500, "No such file: $file"];

    my $res = _request(
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
    return _htres2envres($res) unless $res->is_success;
    return [543, "Can't scrape upload status from response"]
        unless $res->content =~ m!<h3>Submitting query</h3>\s*<p>(.+?)</p>!s;
    my $str = $1;
    if ($str =~ /Query succeeded/) {
        return [200, "OK", undef, {"func.raw_status" => $str}];
    } else {
        return [500, "Failed: $str"];
    }
}

$SPEC{list_files} = {
    v => 1.1,
    summary => 'List files on your PAUSE account',
    args => {
        %common_args,
        %detail_arg,
    },
};
sub list_files {
    require DateTime::Format::DateParse; # XXX any better module?

    my %args = @_;
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
        push @files, $args{detail} ? $rec : $rec->{name};

    }
    [200, "OK", \@files];
}

$SPEC{delete_files} = {
    %common_args,
};
sub delete_files {
    my %args = @_;
}

$SPEC{undelete_files} = {
    %common_args,
};
sub undelete_files {
    my %args = @_;
    [501, "Not yet implemented"];
}

$SPEC{reindex} = {
    %common_args,
};
sub reindex {
    my %args = @_;
    [501, "Not yet implemented"];
}

$SPEC{set_password} = {
    %common_args,
};
sub set_password {
    my %args = @_;
    [501, "Not yet implemented"];
}

$SPEC{set_account_info} = {
    %common_args,
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

=cut

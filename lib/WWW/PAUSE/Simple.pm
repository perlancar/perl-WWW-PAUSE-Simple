package WWW::PAUSE::Simple;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any '$log';
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
    # 2016-07-13 - for a few months now, PAUSE has been giving random 500 errors
    # when uploading. i'm defaulting to a retries=3
    retries => {
        summary => 'Number of retries when received 5xx HTTP error from server',
        schema  => 'int*',
        default => 2,
        tags    => ['common'],
    },
    retry_delay => {
        summary => 'How long to wait before retrying',
        schema  => 'duration*',
        default => "3s",
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
        tags => ['category:filtering'],
    },
);

our %mod_opt_arg = (
    modules => {
        summary => 'Module names/wildcard patterns',
        'summary.alt.plurality.singular' => 'Module name/wildcard pattern',
        schema  => ['array*', of=>'str*'],
        'x.name.is_plural' => 1,
        pos => 0,
        greedy => 1,
        tags => ['category:filtering'],
    },
);

our %protect_files_arg = (
    protect_files => {
        summary => 'Protect some files/wildcard patterns from delete/cleanup',
        schema  => ['array*', of=>'str*'],
        'x.name.is_plural' => 1,
        tags => ['category:filtering'],
    },
);

$SPEC{':package'} = {
    v => 1.1,
    summary => 'An API for PAUSE',
};

sub _parse_release_filename {
    my $filename = shift;
    return undef unless
        $filename =~ /\A
                      (\w+(?:-\w+)*)
                      -v?(\d+(?:\.\d+){0,2}(_\d+|-TRIAL)?)
                      \.$re_archive_ext
                      \z/ix;
    return ($1, $2, $3); # (dist, version, is_dev)
}

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

    my $tries = 0;
    my $resp;
  RETRY:
    while (1) {
        $resp = $ua->request($req);
        if ($resp->code =~ /^[5]/ && $args{retries} >= ++$tries) {
            $log->warnf("Got error %s (%s) from server, retrying (%d/%d) ...",
                        $resp->code, $resp->message, $tries, $args{retries});
            sleep $args{retry_delay};
            next;
        }
        last;
    }
    $resp;
}

sub _htres2envres {
    my $res = shift;
    [$res->code, $res->message, $res->content];
}

$SPEC{upload_files} = {
    v => 1.1,
    summary => 'Upload file(s)',
    args_rels => {
        choose_one => [qw/delay group_delay/],
    },
    args => {
        %common_args,
        %files_arg,
        subdir => {
            summary => 'Subdirectory to put the file(s) into',
            schema  => 'str*',
            default => '',
        },
        delay => {
            summary => 'Pause a number of seconds between files',
            schema => ['duration*'],
            description => <<'_',

If you upload a lot of files (e.g. 7-10 or more) at a time, the PAUSE indexer
currently might choke with SQLite database locking problem and thus fail to
index your releases. Giving a delay of say 2-3 minutes (120-180 seconds) between
files will alleviate this problem.

_
        },
        group_delay => {
            summary => 'Pause a number of seconds between groups of files',
            schema => 'duration*',
            description => <<'_',

As an alternative to the `delay` option, you can also use this option. This will
group the files to be uploaded by versions and prevent files of the same dist
and different versions to be uploaded too close to one another, as this might
cause indexing problem too. For example, suppose you're uploading Foo-1.zip
Foo-2.zip Foo-3.zip Bar-1.zip Baz-1.zip Baz-2.zip. The files will be uploaded in
this order:

    upload: Foo-1.zip Bar-1.zip Baz-1.zip
    group delay
    upload: Foo-2.zip Baz-2.zip
    group delay
    upload: Foo-3.zip

As with the `delay` option, it is recommended that if you upload several files
at once, you set this option to 2-3 minutes (120-180 seconds).

_
        },
    },
    features => {dry_run=>1},
};
sub upload_files {
    require File::Basename;

    my %args = @_;
    my $files0 = $args{files}
        or return [400, "Please specify at least one file"];
    my $subdir = $args{subdir} // '';

    # probably group it into versions
    my @files;
    if ($args{group_delay}) {
        my %files_by_dist; # key=distname, val=[file, ...]
        my $max_num_vers = 0;
        for my $file (sort @$files0) {
            my ($dist, $ver, $is_dev) = _parse_release_filename($file);
            if (!$dist) {
                ($dist, $ver) = ("", 0);
            }
            $files_by_dist{$dist} //= [];
            push @{ $files_by_dist{$dist} }, $file;
            $max_num_vers = @{ $files_by_dist{$dist} }
                if $max_num_vers < @{ $files_by_dist{$dist} };
        }
        for my $i (1..$max_num_vers) {
            for my $dist (keys %files_by_dist) {
                next unless @{ $files_by_dist{$dist} } >= $i;
                push @files, {
                    group => $i,
                    file => $files_by_dist{$dist}[$i-1],
                };
            }
        }
    } else {
        @files = map { +{group=>1, file=>$_} } @$files0;
    }

    my $envres = envresmulti();

    my $i = 0;
    my $prev_group = 0;
    for my $ent (@files) {
        my $file = $ent->{file};
        my $res;
        {
            unless (-f $file) {
                $res = [404, "No such file"];
                last;
            }

            if ($args{-dry_run}) {
                $log->tracef("[dry-run] Uploading %s ...", $file);
                goto DELAY;
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
        $log->warnf("Upload of %s failed: %s - %s", $file, $res->[0], $res->[1])
            if $res->[0] !~ /^2/;
        $envres->add_result($res->[0], $res->[1], $res->[3]);

      DELAY:
        {
            # it's the last file, no point in delaying, just exit
            last if ++$i >= @files;
            if ($args{delay}) {
                $log->tracef("Sleeping between files for %d second(s) ...", $args{delay});
                sleep $args{delay};
                last;
            }
            if ($args{group_delay} &&
                    $ent->{group} > 1 && $prev_group != $ent->{group}) {
                $log->tracef("Sleeping between group for %d second(s) ...", $args{group_delay});
                sleep $args{group_delay};
                $prev_group = $ent->{group};
            }
        }
    }
    $envres->as_struct;
}

# old name, deprecated
{
    no strict 'refs';
    no warnings 'once';
    *upload_file = \&upload_files;
    $SPEC{upload_file} = { %{ $SPEC{upload_files} } }; # shallow clone
    $SPEC{upload_file}{'x.no_index'} = 1;
}

$SPEC{list_files} = {
    v => 1.1,
    summary => 'List files',
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
    require Date::Parse;
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

        my $time = Date::Parse::str2time($4, "UTC");

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
        $resmeta{'table.fields'} =
            [qw/name size mtime is_scheduled_for_deletion deletion_time/];
        $resmeta{'table.field_formats'} =
            [undef, undef, 'iso8601_datetime', undef, 'iso8601_date'];
    }
    [200, "OK", \@files, \%resmeta];
}

$SPEC{list_dists} = {
    v => 1.1,
    summary => 'List distributions',
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
        my ($dist, $version, $is_dev) = _parse_release_filename($file);
        unless (defined $dist) {
            $log->debugf("Skipping %s: doesn't match release regex", $file);
            next;
        }
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
                sort { version->parse($b) <=> version->parse($a) }
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
        $resmeta{'table.fields'} = [qw/name version is_dev_version file/];
    }
    [200, "OK", \@dists, \%resmeta];
}

$SPEC{delete_old_releases} = {
    v => 1.1,
    summary => 'Delete older versions of distributions',
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
        %protect_files_arg,
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
    $res = delete_files(
        _common_args(\%args),
        protect_files => $args{protect_files},
        files=>\@to_delete,
        -dry_run=>$args{-dry_run},
    );
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

    my $protect_files = $args{protect_files} // [];

    my @files;
    {
        my $listres;
        if (grep {String::Wildcard::Bash::contains_wildcard($_)}
                (@$files0, @$protect_files)) {
            $listres = list_files(_common_args(\%args));
            return [500, "Can't list files: $listres->[0] - $listres->[1]"]
                unless $listres->[0] == 200;
        }

        for my $file (@$files0) {
            if (String::Wildcard::Bash::contains_wildcard($file)) {
                my $re = Regexp::Wildcards->new(type=>'unix')->convert($file);
                $re = qr/\A($re)\z/;
                for my $f (@{$listres->[2]}) {
                    push @files, $f if $f =~ $re && !($f ~~ @files);
                }
            } else {
                push @files, $file;
            }
        }

        for my $protect_file (@$protect_files) {
            if (String::Wildcard::Bash::contains_wildcard($protect_file)) {
                my $re = Regexp::Wildcards->new(type=>'unix')->convert(
                    $protect_file);
                $re = qr/\A($re)\z/;
                @files = grep {
                    if ($_ =~ $re) {
                        $log->debugf("Excluding %s (protected, wildcard %s)",
                                     $_, $protect_file);
                        0;
                    } else {
                        1;
                    }
                } @files;
            } else {
                @files = grep {
                    if ($_ eq $protect_file) {
                        $log->debugf("Excluding %s (protected)", $_);
                        0;
                    } else {
                        1;
                    }
                } @files;
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
        %protect_files_arg,
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
    'x.no_index' => 1,
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
    'x.no_index' => 1,
};
sub set_account_info {
    my %args = @_;
    [501, "Not yet implemented"];
}

$SPEC{list_modules} = {
    v => 1.1,
    summary => 'List modules (permissions)',
    args => {
        %common_args,
        %detail_l_arg,
        %mod_opt_arg,
        type => {
            summary => 'Only list modules matching certain type',
            schema => 'str*',
            tags => ['category:filtering'],
        },
    },
};
sub list_modules {
    my %args  = @_;
    require Regexp::Wildcards;
    require String::Wildcard::Bash;

    my $q = $args{modules} // [];

    my %post_data = (ACTION=>'peek_perms');

    # optimize: the PAUSE server can do SQL LIKE, if there is only a single
    # module argument we pass it to server to reduce traffic
    if (@$q == 1) {
        $post_data{pause99_peek_perms_by} = 'ml';
        $post_data{pause99_peek_perms_query} =
            String::Wildcard::Bash::convert_wildcard_to_sql($q->[0]);
        $post_data{pause99_peek_perms_sub} = 'Submit';
    }

    my $httpres = _request(
        %args,
        post_data => [\%post_data],
    );

    return _htres2envres($httpres) unless $httpres->is_success;

    # convert wildcard patterns in arguments to regexp
    for (@$q) {
        next unless String::Wildcard::Bash::contains_wildcard($_);
        my $re = Regexp::Wildcards->new(type=>'unix')->convert($_);
        $re = qr/\A($re)\z/;
        $_ = $re;
    }

    my @mods;
    goto NO_MODS if $httpres->content =~ /No records found/;
    return [543, "Can't scrape list of modules from response",
            $httpres->content]
        unless $httpres->content =~ m!<tr><td><b>module</b></td>.+?</tr>(.+?)</table>!s;
    my $str = $1;

  REC:
    while ($str =~ m!<tr>
                     <td><a[^>]+>(.+?)</a></td>\s*
                     <td><a[^>]+>(.+?)</a></td>\s*
                     <td>(.+?)</td>\s*
                     <td>(.+?)</td>\s*
                     </tr>!gsx) {
        my $rec = {module=>$1, userid=>$2, type=>$3, owner=>$4};

        # filter by requested file/wildcard
      FILTER_QUERY:
        {
            last unless @$q > 1;
            for (@$q) {
                if (ref($_) eq 'Regexp') {
                    last FILTER_QUERY if $rec->{module} =~ $_;
                } else {
                    last FILTER_QUERY if $rec->{module} eq $_;
                }
            }
            # nothing matches
            next REC;
        }

      FILTER_TYPE:
        if ($args{type}) {
            next REC unless $rec->{type} eq $args{type};
        }

        push @mods, $args{detail} ? $rec : $rec->{module};
    }

  NO_MODS:

    my %resmeta;
    if ($args{detail}) {
        $resmeta{'table.fields'} =[qw/module userid type owner/];
    }
    [200, "OK", \@mods, \%resmeta];
}

1;
# ABSTRACT:

=for Pod::Coverage ^(upload_file|set_account_info|set_password)$

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

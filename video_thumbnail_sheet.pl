#!/usr/bin/perl
# Create a description sheet with thumbnails for videos
# Written by Miklós Máté, 2017
# Distributed under WTFPL
use strict;
use warnings;

use Image::Magick;
use Data::Dumper;
use File::Path;

## constants
use constant DEBUG => 0;
use constant FONT => 'Liberation-Sans';
use constant FONT_FIXED => 'Liberation-Mono';

## debug printing
sub debug {
    warn @_ if DEBUG; # to STDERR
}

## working temp directory
my $WORKDIR = "/tmp/vts_tmp$$";
debug "$WORKDIR\n";

## add '\' before shell meta characters
sub escape_name {
    my $input = shift;
    my $output = $input;
    $output =~ s/([;<>\*\|`&\$!#\(\)\[\]\{\}:'" ])/\\$1/g;
    #debug "escaped: '$input' -> '$output'";
    return $output
}

## parse time into float, seconds and milliseconds are optional in the input
sub parse_time {
    my $time = shift;
    # hours and fractional seconds are optional
    my ($hhh, $hours, $minutes, $seconds, $sss) = $time =~ /^((\d\d):)?(\d\d):(\d\d(\.\d\d)?)$/;
    #debug "parse time '$time' -> '$hhh' '$hours' '$minutes' '$seconds' '$sss'\n";
    die "cannot parse time '$time'\n" unless defined $minutes;
    $hours = 0 unless defined $hours;
    return $hours*60*60 + $minutes*60 + $seconds;
}

#debug parse_time('03:02:01.12');
#debug parse_time('02:01.12');
#debug parse_time('03:02:01');
#debug parse_time('02:01');
#exit 0;

## return formatted time string from the given seconds
## format is automatic if show_hours or show_decimals is not set
sub format_time {
    my ($val, $show_hours, $show_decimals) = @_;
    my $hours = int($val / (60*60));
    my $minutes = int(($val - $hours*60*60) / 60);
    my $seconds = $val - $hours*60*60 - $minutes*60;
    my $frac = '05.2';
    unless ($show_decimals) {
        $frac = '02.0' if $seconds == int($seconds)
    }
    if ($show_hours || $hours) {
        return sprintf("%02d:%02d:%${frac}f", $hours, $minutes, $seconds);
    } else {
        return sprintf("%02d:%${frac}f", $minutes, $seconds);
    }
}

#debug format_time(1, 1);
#debug format_time(1.23, 1);
#debug format_time(1.23);
#debug format_time(123.4);
#debug format_time(123);
#debug format_time(123, 0, 1);
#exit 0;

sub min {
    my $min = shift;
    $min = $min<$_ ? $min : $_ for @_;
    return $min
}

sub max {
    my $max = shift;
    $max = $max>$_ ? $max : $_ for @_;
    return $max
}

# remove the elements from the first array that are also present in any subsequent array
sub array_substract {
    my $a1 = shift;
    my %a1;
    @a1{@$a1} = ();
    while (my $a2 = shift) {
        for my $i (@$a2) {
            delete $a1{$i}
        }
    }
    return [keys %a1]
}


##################################
## Configuration
#TODO remember window size? conf_windowsize?

my $sheetconf_blackoutlines;
my $sheetconf_columns;
my $sheetconf_fontsize;
my $sheetconf_format;
my $sheetconf_rows;
my $sheetconf_shotwidth;
my $sheetconf_showhours;
my $sheetconf_timestamps;
####################################
{
    my $config_dir = "$ENV{HOME}/.config/";
    my $config_file_name = 'video_thumbnail_sheet.conf';

    # 's' string, 'b' bool', 'i' int, 'f' float, 'e' enum
    my %config_items = (
        sheetconf_blackoutlines=>['b', \$sheetconf_blackoutlines, 'Black outlines around snapshots', 1],
        sheetconf_columns=>['i', \$sheetconf_columns, 'Number of snapshots in a row', 4],
        sheetconf_fontsize=>['i', \$sheetconf_fontsize, 'Text font size', 14],
        sheetconf_format=>['e jpg png', \$sheetconf_format, 'Image format', 'jpg'],
        sheetconf_rows=>['i', \$sheetconf_rows, 'Number of rows', 4],
        sheetconf_shotwidth=>['i', \$sheetconf_shotwidth, 'Width of a snapshot in pixels', 200],
        sheetconf_showhours=>['b', \$sheetconf_showhours, 'Always show hours in timestamps (off=auto)', 0],
        sheetconf_timestamps=>['b', \$sheetconf_timestamps, 'Timestamps for snapshots', 1],
    );

    sub load_config {
        return unless -d $config_dir;
        return unless -r "$config_dir$config_file_name";

        my %legal_bool_values;
        @legal_bool_values{qw(true false yes no 1 0)} = (1, 0, 1, 0, 1, 0);

        open (my $CONF, "$config_dir$config_file_name") or die "Cannot read config file: $!\n";
        while (my $cline = <$CONF>) {
            $cline =~ s/#.*//;#TODO can valid config values contain '#'?
            next if $cline =~ /^\s*$/;

            if ($cline =~ /^(\w+)\s*=\s*(.*)/) {
                my ($key, $val) = ($1, $2);
                unless (exists $config_items{$key}) {
                    warn "ignoring configuration option '$key'\n";
                    next
                }

                my ($type, $varref, $desc, $default) = @{$config_items{$key}};
                if      ($type eq 'b') {
                    my $boolval = $legal_bool_values{$val};
                    unless (defined $boolval) {
                        warn "invalid boolean value '$val' for '$key'\n";
                        $$varref = $default
                    } else {
                        $$varref = $boolval
                    }

                } elsif ($type eq 'f') {
                    unless ($val =~ /^\d+(\.\d+)?$/) {
                        warn "invalid float value '$val' for '$key'\n";
                        $$varref = $default
                    } else {
                        $$varref = $val
                    }

                } elsif ($type eq 'i') {
                    unless ($val =~ /^\d+$/) {
                        warn "invalid int value '$val' for '$key'\n";
                        $$varref = $default
                    } else {
                        $$varref = $val
                    }

                } elsif ($type eq 's') {
                    unless ($val =~ /^"([^"]+)"$/) {
                        warn "invalid string value '$val' for '$key'\n";
                        $$varref = $default
                    } else {
                        $$varref = $1
                    }

                } elsif ($type =~ /^e /) {
                    my @values = split / /, $type; shift @values;
                    my %values; @values{@values} = ();
                    $val =~ /^"([^"]+)"$/;
                    my $stringval = $1;
                    unless (exists $values{$stringval}) {
                        warn "invalid enum value '$val' for '$key'\n";
                        $$varref = $default
                    } else {
                        $$varref = $stringval
                    }

                } else {
                    die "the impossible happened\n";
                }

            } else {
                die "invalid config line '$cline'\n";
            }
        }
    }

    sub save_config {
        mkdir $config_dir unless -d $config_dir;

        open (my $CONF, ">", "$config_dir$config_file_name") or die "Cannot write config file '$config_dir$config_file_name': $!\n";
        for my $key (sort keys %config_items) {
            my ($type, $varref) = @{$config_items{$key}};
            if      ($type eq 'b') {
                printf $CONF "%s = %s\n", $key, $$varref ? 'true' : 'false';
            } elsif ($type eq 'f') {
                printf $CONF "%s = %f\n", $key, $$varref;
            } elsif ($type eq 'i') {
                printf $CONF "%s = %d\n", $key, $$varref;
            } elsif ($type eq 's') {
                printf $CONF qq(%s = "%s"\n), $key, $$varref;
            } elsif ($type =~ /^e /) {
                printf $CONF qq(%s = "%s"\n), $key, $$varref;
            } else {
                die "the impossible happened\n";
            }
        }
    }

    sub foreach_config_item {
        my $sub = shift;
        for my $key (sort keys %config_items) {
            $sub->($key, $config_items{$key}, @_);
        }
    }
}

sub set_config_defaults {
    foreach_config_item(sub {my ($name, $desc) = @_; ${$desc->[1]} = $desc->[3]})
}

sub init_workdir {
    cleanup_workdir(); # make sure it's empty
    mkdir $WORKDIR or die "cannot create working dir: $!\n";
}

sub cleanup_workdir {
    # delete it recursively
    rmtree($WORKDIR);
}

{
    my %streamkeys;
    @streamkeys{qw(codec_type codec_name sample_rate channel_layout language width height avg_frame_rate)} = ();

    ## identify the video file
    sub get_file_info {
        my $filename = shift;
        my %fileinfo;

        my $safe_filename = escape_name($filename);
        my $fileinfo_str = `ffprobe $safe_filename -v quiet -print_format ini -show_format -show_streams`;

        $fileinfo{duration} = undef;

        my $current_block;
        my $current_stream;
        my @streams;
        for my $line (split /\n/, $fileinfo_str) {
            if ($line =~ /^\[(.*)\]$/) {
                $current_block = $1;
                if ($current_block eq 'format') {
                    undef $current_stream
                }
                if ($current_block =~ /stream.(\d+)/) { # the disposition and tags block also count
                    $current_stream = $1
                }
            } elsif ($line =~ /^(\w+)=(.+)/) {
                my ($key, $val) = ($1, $2);
                if ($current_block eq 'format') {
                    if ($key eq 'duration') {
                        $fileinfo{duration} = $val
                    } elsif ($key eq 'format_name') {
                        $fileinfo{container} = $val
                    }
                } elsif (defined $current_stream) {
                    $streams[$current_stream]{$key} = $val if exists $streamkeys{$key}
                }
            }
        }
        debug Dumper(\@streams);

        die "not a media file\n" unless defined $fileinfo{duration};
        die "probably an image file\n" unless $fileinfo{duration} > 1;

        for my $stream (@streams) {
            if ($stream->{codec_type} eq 'video') {
                # I assume there is only one video stream
                @fileinfo{qw(xres yres vcodec fps)} = @{$stream}{qw(width height codec_name avg_frame_rate)};

                debug "fps $stream->{avg_frame_rate}\n";
                if      ($stream->{avg_frame_rate} eq '24000/1001') {
                    $fileinfo{fps} = '23.976'
                } elsif ($stream->{avg_frame_rate} eq '30000/1001') {
                    $fileinfo{fps} = '29.97'
                } elsif ($stream->{avg_frame_rate} eq '60000/1001') {
                    $fileinfo{fps} = '59.94'
                } elsif ($stream->{avg_frame_rate} eq '500/21') {
                    $fileinfo{fps} = '23.81'
                } elsif ($stream->{avg_frame_rate} =~ /(\d+)\/(\d+)/) {
                    $fileinfo{fps} = $1/$2
                }
            } elsif ($stream->{codec_type} eq 'audio') {
                my $au = [@{$stream}{qw(language codec_name sample_rate channel_layout)}];
                $au->[2] /= 1000;
                $au->[0] = 'und' unless defined $au->[0];
                push @{$fileinfo{audio}}, $au;
            } elsif ($stream->{codec_type} eq 'subtitle') {
                push @{$fileinfo{subs}}, [@{$stream}{qw(codec_name language)}];
            }
        }

        die "no video\n" unless exists $fileinfo{vcodec};

        my ($videopath, undef, $videoname) = $filename =~ /^((.*)\/)?([^\/]*)$/;
        $videopath = './' unless defined $videopath;
        my ($basename, $suffix) = $videoname =~ /^(.*)\.(\w+)$/;
        $basename = $videoname unless defined $suffix;
        debug "'$videopath' '$videoname' '$basename' '$suffix'\n";

        #filename: original file name with full path
        #videopath: only the path, with '/' at the end
        #videoname: only the video file name, without path
        #basename: only the video file name, without path or suffix
        @fileinfo{qw(filename videopath videoname basename)} =
            ($filename, $videopath, $videoname, $basename);

        debug Dumper(\%fileinfo);

        return \%fileinfo
    }
}

## calculate the snapshot timings
sub calculate_shot_parameters {
    my $fileinfo = shift;
    my $starttime = 0;
    my $interval = 0;
    my $shotnum = 0;

    # $sheetconf_columns*$sheetconf_rows shots evenly distributed
    $starttime = $interval = $fileinfo->{duration} / ($sheetconf_columns*$sheetconf_rows+1);
    $shotnum = $sheetconf_columns*$sheetconf_rows - 1;
    return ($starttime, $interval, $shotnum);
}

## create a single snapshot at the specified time
sub create_file_single_snapshot {
    my ($fileinfo, $t) = @_;
    # note: $t is [shot number, shot time]
    my $imgfile = "$WORKDIR/$fileinfo->{basename}_$t->[0].jpg";
    my $safe_imgfile = escape_name($imgfile);
    my $safe_filename = escape_name($fileinfo->{filename});

    debug "shot $t->[0] '$imgfile' at $t->[1]\n";
    #debug "'$safe_filename' -- '$safe_imgfile'\n";
    #TODO how to include subtitles? do we need that?
    #TODO remember stdout&stderr for debug?
    my $cmd = "ffmpeg -ss $t->[1] -i $safe_filename -y -f image2 -vcodec mjpeg -vframes 1 $safe_imgfile 2>/dev/null";
    debug "'$cmd'\n";
    system ($cmd) == 0 or die "calling ffmpeg failed: $? $!\n";
    $fileinfo->{shots}{$t->[1]} = $imgfile;
}

## create all snapshots for the video
sub create_file_snapshots {
    my $fileinfo = shift;
    my @shottimes;
    my ($starttime, $interval, $shotnum) = calculate_shot_parameters($fileinfo);
    push @shottimes, [$_, $starttime + $_*$interval] for (0..$shotnum);
    for my $t (@shottimes) {
        create_file_single_snapshot($fileinfo, $t);
    }
}

## create a single color snapshot image and link it to all snapshots in $fileinfo
sub create_blank_snapshots {
    my $fileinfo = shift;

    # create one blank image with the right size in workdir
    my $blank = Image::Magick->new;
    my $blankname = "$WORKDIR/$fileinfo->{basename}_blank.jpg";
    $blank->Set(size=>"$fileinfo->{xres}x$fileinfo->{yres}");
    $blank->ReadImage('canvas:darkgrey');
    $blank->Write(filename=>$blankname);

    # use it for all snapshots
    my @shottimes;
    my ($starttime, $interval, $shotnum) = calculate_shot_parameters($fileinfo);
    push @shottimes, [$_, $starttime + $_*$interval] for (0..$shotnum);
    for my $t (@shottimes) {
        $fileinfo->{shots}{$t->[1]} = $blankname;
    }
}

sub delete_snapshots {
    my $fileinfo = shift;
    map { unlink } values %{$fileinfo->{shots}};
    %{$fileinfo->{shots}} = ()
}

## create the sheet using the given snapshots
sub create_video_sheet {
    my $fileinfo = shift;

    my $sheet = Image::Magick->new;

    # create a phony image to be able to query font metric
    $sheet->Set(size=>'2000x2000');
    $sheet->ReadImage('canvas:white');

    # an example shot description text to query font metric
    my $shot_description;
    if ($fileinfo->{duration} > 60*60 or $sheetconf_showhours) {
        $shot_description = "12:34:56.78";
    } else {
        $shot_description = "34:56.78";
    }
    my %shot_description_params = (
        text=>$shot_description,
        pointsize=>$sheetconf_fontsize-2,
        font=>FONT_FIXED,
        fill=>'white',
        background=>'none',
        x=>0,
        y=>0,
        gravity=>'NorthEast');
    my (undef, undef, undef, undef, $shot_description_x, $shot_description_y) =
        $sheet->QueryMultilineFontMetrics(%shot_description_params);

    my $shot_width = max($sheetconf_shotwidth, $shot_description_x);
    my $shot_height = $shot_width / $fileinfo->{xres} * $fileinfo->{yres};
    my $shot_distance = 10; #TODO configurable?
    my $rows = int(((keys %{$fileinfo->{shots}}) + 1) / $sheetconf_columns);
    warn "Row calculation is off!?!\n" unless $rows == $sheetconf_rows;

    # create title text
    my $sheet_title = "$fileinfo->{videoname}";
    my %sheet_title_params = (
        text=>$sheet_title,
        pointsize=>$sheetconf_fontsize+4,
        font=>FONT,
        fill=>'black',
        background=>'none',
        x=>$shot_distance,
        y=>$shot_distance,
        gravity=>'NorthWest');
    my (undef, undef, undef, undef, $sheet_title_x, $sheet_title_y) =
        $sheet->QueryMultilineFontMetrics(%sheet_title_params);

    # create sheet description
    my $duration = format_time($fileinfo->{duration}, $sheetconf_showhours);
    my $sheet_description = "Video: $duration, $fileinfo->{xres}x$fileinfo->{yres}, $fileinfo->{vcodec}";
    if (defined $fileinfo->{fps}) {
        $sheet_description .= ", $fileinfo->{fps} fps";
    }
    if (defined $fileinfo->{audio}) {
        my @audio = map { "$_->[0] ($_->[1], $_->[2] kHz, $_->[3])" } @{$fileinfo->{audio}};
        my $audio = '';
        while (my @next = splice(@audio, 0, 4)) {
            $audio .= join ', ', @next;
            $audio .= ",\n        " if (@audio)
        }
        $sheet_description .= "\nAudio: $audio";
    }
    if (defined $fileinfo->{subs}) {
        my @subs = map { "$_->[1] ($_->[0])" } @{$fileinfo->{subs}};
        my $subs = '';
        while (my @next = splice(@subs, 0, 8)) {
            $subs .= join ', ', @next;
            $subs .= ",\n        " if (@subs)
        }
        $sheet_description .= "\nSubtitles: $subs";
    }
    my %sheet_description_params = (
        text=>$sheet_description,
        pointsize=>$sheetconf_fontsize,
        font=>FONT,
        fill=>'black',
        background=>'none',
        x=>$shot_distance,
        y=>$shot_distance + $sheet_title_y + $shot_distance,
        gravity=>'NorthWest');
    my (undef, undef, undef, undef, $sheet_description_x, $sheet_description_y) =
        $sheet->QueryMultilineFontMetrics(%sheet_description_params);

    debug "shot description $shot_description_x, $shot_description_y\n";
    debug "sheet title $sheet_title_x, $sheet_title_y\n";
    debug "sheet description $sheet_description_x, $sheet_description_y\n";
    debug $sheet_description;

    # destroy phony image
    @$sheet = ();

    # calculate sheet size
    my $header_width = max($sheet_title_x, $sheet_description_x);
    my $header_hight = $shot_distance + $sheet_title_y + $shot_distance + $sheet_description_y + $shot_distance;
    my $sheet_width = $shot_width*$sheetconf_columns + $shot_distance*($sheetconf_columns-1) + 2*$shot_distance;
    my $sheet_height = $header_hight + $rows*($shot_height + $shot_distance);

    debug "sheet width $sheet_width height $sheet_height shot width $sheetconf_shotwidth height $shot_height\n";

    # create white sheet
    $sheet->Set(size=>"${sheet_width}x${sheet_height}");
    $sheet->ReadImage('canvas:white');

    # print header
    $sheet->Annotate(%sheet_title_params);
    $sheet->Annotate(%sheet_description_params);

    # add shots
    my $i = 0;
    for my $shot_time (sort {$a <=> $b} keys %{$fileinfo->{shots}}) {
        my $shot_filename = $fileinfo->{shots}{$shot_time};
        # calculate shot position
        my $shot_x = $shot_distance + ($i  % $sheetconf_columns) * ($sheetconf_shotwidth + $shot_distance);
        my $shot_y = $header_hight + int($i / $sheetconf_columns) * ($shot_height + $shot_distance);
        debug "shot $shot_time at $shot_x $shot_y\n";

        # add the snapshot
        my $shot = Image::Magick->new;
        $shot->Read($shot_filename);
        $shot->Resize(geometry=>"${sheetconf_shotwidth}x${shot_height}");
        $shot->Border(bordercolor=>'black', geometry=>"1x1") if $sheetconf_blackoutlines;
        $sheet->Composite(image=>$shot, compose=>'Atop', x=>$shot_x, y=>$shot_y);

        # print shot description (timestamp) if enabled
        if ($sheetconf_timestamps) {
            # gravity is NorthEast, so x measures from right
            $shot_description_params{x} = $sheet_width - $shot_x - $shot_width  + 0.5*$shot_description_y;
            $shot_description_params{y} = $shot_y + $shot_height - 1.5*$shot_description_y;
            $shot_description_params{text} = format_time($shot_time, $sheetconf_showhours, 1);
            debug "desc $shot_description_params{text} at $shot_description_params{x} $shot_description_params{y}\n";

            # black outline for white text: redraw method
            my %outline_params = %shot_description_params;
            $outline_params{fill} = 'black';
            for my $offset ([-1, -1], [ 1, -1], [-1,  1], [ 1,  1]) {
                $outline_params{x} += $offset->[0];
                $outline_params{y} += $offset->[1];
                $sheet->Annotate(%outline_params);
                $outline_params{x} -= $offset->[0];
                $outline_params{y} -= $offset->[1];
            }

            $sheet->Annotate(%shot_description_params);
        }

        $i++;
    }

    # save it
    unless (defined $fileinfo->{sheetname}) {
        $fileinfo->{sheetname} = "$fileinfo->{videopath}$fileinfo->{basename}.$sheetconf_format"
    }
    #png: quality/10 is zlib compression level, quality%10 is filtering (5 is the best)
    $sheet->Write(filename=>$fileinfo->{sheetname}, quality=>90); #TODO hardcoded jpeg quality? yep

    delete_snapshots($fileinfo);
}


############# Main

set_config_defaults();
load_config();
init_workdir();

if (@ARGV) {
    # all arguments are video file names, process them
    for my $filename (@ARGV) {
        print "processing '$filename' ...";
        my $fileinfo = get_file_info($filename);
        create_file_snapshots($fileinfo);
        create_video_sheet($fileinfo);
        print " done\n";
    }
    cleanup_workdir();
    exit 0;

}

debug "showing GUI\n";

use Glib qw(TRUE FALSE);
# don't connect to X in BEGIN
#use Gtk2 qw(-init -threads-init);
require Gtk2;
Gtk2::init();

my $win = Gtk2::Window->new;
$win->signal_connect(delete_event=>sub{
        cleanup_workdir();
        #TODO remember window size: load_config(); $conf_windowsize = gtk_window_get_size(); save_config();
        Gtk2->main_quit; FALSE
    });
$win->set_title('Video Thumbnail Sheet');

my $mainpaned = Gtk2::HPaned->new;
$win->add($mainpaned);
my $leftvbox = Gtk2::VBox->new;
$mainpaned->pack1($leftvbox, TRUE, TRUE);
my $righthbox = Gtk2::HBox->new;
$mainpaned->pack2($righthbox, TRUE, TRUE);

my $fileselect = Gtk2::FileChooserWidget->new('open');
$fileselect->set_select_multiple(TRUE);
#$fileselect->set_current_folder($conf_lastdir) if $conf_remember_lastdir;
$leftvbox->pack_start($fileselect, TRUE, TRUE, 4);
my $processhbox = Gtk2::HBox->new;
$leftvbox->pack_start($processhbox, FALSE, FALSE, 4);
my $processbutton = Gtk2::Button->new_with_label('Process selected videos');
$processhbox->pack_start($processbutton, TRUE, FALSE, 4);
my $progress = Gtk2::ProgressBar->new;
$leftvbox->pack_start($progress, FALSE, FALSE, 4);

my $middlevbox = Gtk2::VBox->new;
$righthbox->pack_start($middlevbox, TRUE, TRUE, 4);
my $zoomhbox = Gtk2::HBox->new;
$middlevbox->pack_start($zoomhbox, FALSE, FALSE, 4);
my $zoominbutton = Gtk2::Button->new_from_stock('gtk-zoom-in');
$zoomhbox->pack_start($zoominbutton, FALSE, FALSE, 4);
my $zoomoutbutton = Gtk2::Button->new_from_stock('gtk-zoom-out');
$zoomhbox->pack_start($zoomoutbutton, FALSE, FALSE, 4);
my $zoomnormbutton = Gtk2::Button->new_from_stock('gtk-zoom-100');
$zoomhbox->pack_start($zoomnormbutton, FALSE, FALSE, 4);
my $scrolled = Gtk2::ScrolledWindow->new;
$scrolled->set_policy('automatic', 'automatic');
$middlevbox->pack_start($scrolled, TRUE, TRUE, 4);
my $viewport = Gtk2::Viewport->new;
$scrolled->add($viewport);
my $previewimg = Gtk2::Image->new;
$viewport->add($previewimg);
my $settingstoggle = Gtk2::ToggleButton->new('Show settings >>');
$middlevbox->pack_start($settingstoggle, FALSE, FALSE, 4);

my $settingsvbox = Gtk2::VBox->new;
$righthbox->pack_start($settingsvbox, FALSE, FALSE, 4);
foreach_config_item(\&add_setting, $settingsvbox); #TODO much better config area
my $savesettingsbutton = Gtk2::Button->new_with_label('Save settings');
$settingsvbox->pack_end($savesettingsbutton, FALSE, FALSE, 4);

my $selection_changed = 0; # count changes since last preview update
my @last_selection; # previously selected files
my $previewname; # name of video file to preview
my $previewfile; # name of the preview sheet
my $zoomfactor = 1; # current zoom factor of the preview sheet

$fileselect->signal_connect('selection-changed' => sub {
        # the first of the newly selected ones is chosen for preview
        my @selection = $fileselect->get_filenames;
        debug "selected: ", join(', ', @selection), "\n";
        my $newly_selected = array_substract(\@selection, \@last_selection);
        my $unselected = array_substract(\@last_selection, \@selection);
        my $newname;
        if      (@$newly_selected) {
            debug "newly selected: ", join(', ', @$newly_selected), "\n";
            $newname = $newly_selected->[0]
        } elsif (@$unselected) {
            debug "unselected: ", join(', ', @$unselected), "\n";
            $newname = $selection[0] if @selection
        } else {
            debug "nothing selected and nothing unselected\n";
            debug "current selection: ", join(', ', @selection), "\n";
            debug "last selection: ", join(', ', @last_selection), "\n";
        }
        $previewname = $newname;

        update_preview_sheet();

        @last_selection = @selection;
    });
$fileselect->signal_connect('current-folder-changed' => sub {
        #$conf_lastdir = $fileselect->get_current_folder;# if $conf_remember_lastdir;
        return 0
    });
$processbutton->signal_connect('clicked' => sub {
        $processbutton->set_sensitive(FALSE);
        my @selection = $fileselect->get_filenames;
        #my $recentmanager = Gtk2::RecentManager->get_default;
        #my @uris = $fileselect->get_uris;
        my @job_queue;
        my $finished;
        debug "processing: ", join(', ', @selection), "\n";
        for my $filename (@selection) {
            next if -d $filename;
            my $fileinfo;
            eval {$fileinfo = get_file_info($filename)};
            if ($@) {
                chomp $@;
                debug "file '$filename' rejected '$@'\n";
            } else {
                #$recentmanager->add_item($filename);#TODO we need to use get_uris()
                my ($starttime, $interval, $shotnum) = calculate_shot_parameters($fileinfo);
                #debug "file '$filename' shotnum '$shotnum'\n";
                my @shottimes;
                push @shottimes, [$_, $starttime + $_*$interval] for (0..$shotnum);
                for my $t (@shottimes) {
                    push @job_queue, ['shot', $fileinfo, $t];
                }
                push @job_queue, ['sheet', $fileinfo];
            }
        }
        my $total_number_of_jobs = scalar @job_queue;
        #debug "total number of jobs $total_number_of_jobs\n";
        $progress->set_fraction(0);
        Glib::Idle->add(sub {
                if (@job_queue) {
                    my $job = shift @job_queue;
                    # execute action
                    my ($cmd, $fileinfo, $param) = @$job;
                    if ($cmd eq 'shot') {
                        create_file_single_snapshot($fileinfo, $param);
                    } elsif ($cmd eq 'sheet') {
                        create_video_sheet($fileinfo);
                    }
                    $finished++;
                    $progress->set_fraction($finished/(@job_queue+$finished));
                    return 1
                } else {
                    $processbutton->set_sensitive(TRUE);
                    $progress->set_fraction(0);

                    return 0
                }
            });
    });
$zoominbutton->signal_connect('clicked' => sub {
        $zoomfactor *= 1.5;
        update_preview();
    });
$zoomoutbutton->signal_connect('clicked' => sub {
        $zoomfactor /= 1.5;
        update_preview();
    });
$zoomnormbutton->signal_connect('clicked' => sub {
        $zoomfactor = 1;
        update_preview();
    });
$settingstoggle->signal_connect('toggled' => sub {
        if ($settingstoggle->get_active) {
            $settingstoggle->set_label('<< Hide settings');
            $settingsvbox->show
        } else {
            $settingstoggle->set_label('Show settings >>');
            $settingsvbox->hide
        }
    });
$savesettingsbutton->signal_connect('clicked' => sub {
        save_config();
    });

$win->show_all;
$settingsvbox->hide;
#TODO gtk_window_set_default_size($conf_windowsize);
Gtk2->main;

# update the displayed preview (does nothing to the sheet itself)
sub update_preview {
    return $previewimg->clear unless $previewfile;

    my $pixbuf = Gtk2::Gdk::Pixbuf->new_from_file($previewfile);
    my $scaledpix = Gtk2::Gdk::Pixbuf::scale_simple($pixbuf, $zoomfactor*$pixbuf->get_width, $zoomfactor*$pixbuf->get_height, 'nearest');
    $previewimg->set_from_pixbuf($scaledpix);
};

# delayed update of the preview sheet
sub update_preview_sheet {
    $selection_changed++;
    Glib::Timeout->add(200, sub {
            $selection_changed--;
            unless ($selection_changed) {
                # no change in the last .2s, update preview sheet with $previewname
                if (defined $previewname and not -d $previewname) {
                    my $fileinfo;
                    eval {$fileinfo = get_file_info($previewname)};
                    unless ($@) {
                        create_blank_snapshots($fileinfo);
                        my $previewsheet_name = "$WORKDIR/preview.jpg";
                        $fileinfo->{sheetname} = $previewsheet_name;
                        create_video_sheet($fileinfo);
                        $previewfile = $previewsheet_name;
                        update_preview();
                        return 0;
                    } else {
                        chomp $@;
                        debug "file '$previewname' rejected '$@'\n";
                    }
                }
                unlink $previewfile if defined $previewfile and -e $previewfile;
                undef $previewfile;
                update_preview();
            }
            return 0;
        });
}

# add a setting line to the settings panel
sub add_setting {
    my ($name, $desc, $vbox) = @_;
    my ($type, $var, $desctext, $default) = @$desc;
    debug "$name\n";
    #TODO better layout and grouping of the similar config items (must be done manually)
    my $hbox = Gtk2::HBox->new;
    $vbox->pack_start($hbox, 0, 0, 4);
    my $label = Gtk2::Label->new($desctext);
    $hbox->pack_start($label, 0, 0, 4);

    my $valueselect;
    if      ($type eq 'i') {
        $valueselect = Gtk2::SpinButton->new(Gtk2::Adjustment->new($$var, 1, 500, 1, 1, 0), 1, 0);

        $valueselect->signal_connect(changed => sub {
                $$var = $valueselect->get_value;
                update_preview_sheet();
            });

    } elsif ($type eq 'b') {
        $valueselect = Gtk2::CheckButton->new;
        $valueselect->set_active($$var);

        $valueselect->signal_connect(toggled => sub {
                $$var = $valueselect->get_active;
                update_preview_sheet();
            });
    } elsif ($type =~ /^e /) {
        $valueselect = Gtk2::ComboBox->new_text;
        my $index;
        my @values = split / /, $type; shift @values;
        for (my $i=0; $i<@values; ++$i) {
            $valueselect->append_text($values[$i]);
            $index = $i if $$var eq $values[$i]
        }
        die "value '$$var' is not a member of enum '$name'\n" unless defined $index;# should never happen
        $valueselect->set_active($index);

        $valueselect->signal_connect(changed => sub {
                $$var = $valueselect->get_active_text;
                update_preview_sheet();
            });
    } else {
        $valueselect = Gtk2::Entry->new;
        $valueselect->set_text($$var);

        $valueselect->signal_connect(changed => sub {
                $$var = $valueselect->get_text;
                update_preview_sheet();
            });
    }
    $hbox->pack_end($valueselect, 0, 0, 4);
}



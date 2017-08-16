Video Thumbnail Sheet generator tool
=====================================

Dependencies
------------

- perl
- ffmpeg (and ffprobe)
- perlmagick, the perl interface to ImageMagick
- libgtk2-perl for the GUI

On Debian you can install all of the above with `apt-get install libimage-magick-perl ffmpeg libgtk2-perl` as root.


Usage
-----

If it is started without any arguments, the GUI is shown. The videos to be processed can be selected on the left side, it is possible to select multiple files at once. A preview for the most recently selected video is in the middle, it shows how the sheet will look like with the current settings, using grey placeholders for the video snapshots.

The configuration options for the sheet can be changed on the right side. The size and the number of thumbnails, font size, and some other parameters can be customized here. The configuration needs to be saved explicitly to override the default ones in future invocations.

The finished thumbnail sheet is saved next to the original video file with the same file name, just with "jpg" or "png" suffix. See the the attached example for [Tears of Steel](https://mango.blender.org/).

If it is started with some arguments, all of them are assumed to be filenames of video files, and they will be processed. It is not possible to change the settings in this mode: the previously saved configuration is used, or the built-in defaults.


TODO
----

- Use appropriate font for non-latin filenames (now they are ????): the text needs to be split into sections of homogeneous language, and render each part with the correct font (this needs a lot of work)
- Nicer settings panel
- Better performance (can we create all screenshots with only one invocation of ffmpeg?)
- Only show video files in the file list
- Support saving the configuration on non-unix systems
- Better control over jpg/png quality

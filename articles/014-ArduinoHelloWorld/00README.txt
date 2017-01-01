


== Arduino tool chain on  CentOS 7

----
yum install avr-gcc avr-libc avrdude
----





== Converting a PDF image file to PNG

----
convert ./diagram.pdf -size 640 ./diagramp.png
----

You will need the ImageMagick package installed.





== To get the number of frames in a video

----
ffmpeg -i xxx.avi -vcodec copy -f null /dev/null
----





== Fade in at the begining, fadeout at the end

ffmpeg -i ./xxx.avi -vf "fade=in:0:60,fade=out:284:60" ./xxx2.avi


Description of metadata keys:
http://wiki.multimedia.cx/index.php?title=FFmpeg_Metadata

Keys are dependent on the file format being used.

----
ffmpeg \
    -i ~/Pictures/MVI_0078.AVI \
    -metadata title="Blinking led with an Arduino" \
    -metadata date="2014-03-08" \
    -metadata copyright="2014 Jorge Nunes" \
    -vf "fade=in:0:60,fade=out:284:60" \
     -vcodec mpeg4 -q:v 1 -an ./ArduinoHelloWorld-01.avi
----





== Raspbian on Raspberry Pi 3

Playing videos out of the box (http://elinux.org/Omxplayer)

----
omxplayer MyVideoFile.avi
----


VLC does not work out of the box. At least with MPEG4 files.

----
apt-get install vlc
----





== Tools for designing circuit diagrams

Tools for designing circuit diagrams:

----
apt-get install geda-gschem
----

The graphical tool for editing circuit diagrams is called `gschem`.

The `gschem` tool can export diagrams to image formats like EPS and
PNG. Unfortunately the current gEDA version shipping with Debian does
not support exporting to SVG.

But it is possible to convert EPS files to SVG:

----
inkscape \
    --export-plain-svg=./MyDiagram.svg \
    ./MyDiagram.eps
----

And to convert from SVG to PDF:

----
inkscape \
    --without-gui \
    --export-area-drawing \
    --export-pdf=./MyDiagram.pdf \
    ./MyDiagram.pdf
----

To convert from EPS to PDF:

----
epstopdf --outfile=./MyDiagram.eps ./MyDiagram.pdf
----




== Adding symbols to `gschem`

Instructions on how to add symbols to be used by the `geda-gschem`
tool:
http://wiki.geda-project.org/geda:gschem_ug:config#symbol_and_source_libraries

Additional symbols, including the ATMega328P controller:

* https://github.com/CWRU-EE/gschem-symbols
* https://github.com/jlamothe/symbols


Created directory `$HOME/local/gEDA/sym`

----
mkdir -p $HOME/local/gEDA/sym
cd $HOME/local/gEDA/sym

wget https://raw.githubusercontent.com/jlamothe/symbols/master/arduino-1.sym
wget https://raw.githubusercontent.com/CWRU-EE/gschem-symbols/master/atmega48_88_168.sym
----




Created `$HOME/.gEDA/gschemrc` with the following contents:

----
(component-library (build-path (getenv "HOME") "local/gEDA/sym") "My Symbols")
----





== Making a2pdf work on Debian

----
apt-get install asciidoc
----

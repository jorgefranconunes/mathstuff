





Converting a PDF image file to PNG:

convert ./diagram.pdf -size 640 ./diagramp.png





ffmpeg

ffmpeg -i ~/Pictures/MVI_0078.AVI -vcodec mpeg2video -sameq -an xxx.avi





To get the number of frames in a video:

ffmpeg -i xxx.avi -vcodec copy -f null /dev/null





Fade in at the begining, fadeout at the end:

ffmpeg -i ./xxx.avi -vf "fade=in:0:60,fade=out:284:60" ./xxx2.avi


Description of metadata keys:
http://wiki.multimedia.cx/index.php?title=FFmpeg_Metadata

Keys are dependent on the file format being used.


ffmpeg \
    -i ~/Pictures/MVI_0078.AVI \
    -metadata title="Blinking led with an Arduino" \
    -metadata date="2014-03-08" \
    -metadata copyright="2014 Jorge Nunes" \
    -vf "fade=in:0:60,fade=out:284:60" \
     -vcodec mpeg2video -q:v 1 -an ./ArduinoHelloWorld-01.avi


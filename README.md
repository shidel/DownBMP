# DownShiftBMP
Command-line utility to convert images to 256 color Windows 3.x BMP file format

## General Usage Note

This tool is primarily intended to convert modern large megapixel images to
smaller size BMPs usable in DOS. 

The color reduction algorithm is far from optimal or speedy. The bigger the 
output file size that is requested will exponentially increase the processing 
time. The scaling of an image is fairly quick. It is only color remapping is 
that is slow. If you output anything larger than 128x128, expect to wait. 
When it is creating a 320x200, expect to wait a lot longer. You may even 
think it has even crashed or hung.

As a general rule, you probably will want to always us the -m switch. Basically,
it will to a specific (built in) color palette. Even when I fed it a bunch of
photos, they turned out reasonable using that palette. Without that switch,
the most popular image colors are used for the palette. Some images will look
really good. But most will just look weird.

Maybe someday... Maybe, I'll improve the color reduction process and improve
performance. Maybe, I'll work out an additional palette generation method to
produce better unmatched images. Maybe, I'll provide Linux and Windows binaries.
Maybe, I'll add some other options. Maybe, ....


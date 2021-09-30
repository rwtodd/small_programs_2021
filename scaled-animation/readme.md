# Scaled Animation in WPF

They always say WPF is bad for games, but I wonder if I can
pull off a simple scenario -- which I would incidentally need
to do a Sierra AGI interpreter:  A 160x200 image, which must be
scaled at all times to the largest 4:3 aspect ratio image possible,
and which changes on a timer.

So, that's what this little corner of the small programs repo is for... to
try it out.

## Approach

I make an Image with a LayoutTransform to take the 160x200 image and scale
it to 320x240.  Then, I put that in a ViewBox with Uniform scaling, to further
scale the image all the way to whatever size the window will support.

I set the RenderOptions.BitmapScalingMode to NearestNeighbor so that the image
will remain crisp.

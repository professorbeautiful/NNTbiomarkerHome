# SVG plotting explorations.

# I learned that you can read an SVG into eazydraw,
# save as an ezdraw file, export as png,
# read into R, convert using a pixmap function,
# and plot it.
# The png is a 3d array:  x, y, and probably ARBG.

require(png)
thumbsdown.png = readPNG("thumbsdown.png")
str(thumbsdown.png)
thumbsdown.png.pixmap = pixmapRGB(thumbsdown.png)
plot(thumbsdown.png.pixmap)

unix.time({
thumbsdown.png.pixmap = pixmapRGB(thumbsdown.png, bbox=c(0,0,1,1))
plot(thumbsdown.png.pixmap, axes=T)  # Looks good. Very slow.
})

thumbsdown.png.pixmap = pixmapGrey(thumbsdown.png, bbox=c(0,0,1,1))
# Looks poor.
unix.time({
  thumbsdown.png.pixmap = pixmapIndexed(thumbsdown.png, bbox=c(0,0,1,1))
  plot(thumbsdown.png.pixmap, axes=T)  # Looks good. Very slow.
})
##Very little faster. Looks poor.
table(thumbsdown.png.pixmap@red == 1)
down.y = row(thumbsdown.pixmap@grey)[thumbsdown.pixmap@grey < 1]
down.x = col(thumbsdown.pixmap@grey)[thumbsdown.pixmap@grey < 1]
plot(down.x, down.y, pch="scircle")
plot(down.x, down.y)


which(thumbsdown.bmp > 1)
hist(thumbsdown.bmp[thumbsdown.bmp>1])
summary(thumbsdown.bmp[thumbsdown.bmp>1])
points(thumbsdown.bmp)
plot(thumbsdown.bmp, xlim=)
thumbsdown.pixmap = pixmapGrey(thumbsdown.bmp)
str(thumbsdown.pixmap)
max(thumbsdown.pixmap@grey) #1 = white, 0 = black
thumbsdown.pixmap@grey = 1 - thumbsdown.pixmap@grey
thumbsdown.pixmap@grey = floor(thumbsdown.pixmap@grey)
plot(thumbsdown.pixmap)
plot(thumbsdown.pixmap,
     xlim=c(220,270), ylim=c(50,100), axes=T)
plot(thumbsdown.pixmap,
     xlim=c(220,270), ylim=c(50,100), axes=T)



library(sp)
library(maptools)

x <- c(1,5,4,8)
y <- c(1,3,4,5)
xy <- cbind(x,y)
xy.sp = sp::SpatialPoints(xy)
spl <- sp::SpatialLines(list(Lines(Line(xy.sp), ID=i)))

# Rotation of a SpatialLines object
spl90 <- maptools::elide(spl, rotate=90, center=apply(bbox(spl), 1, mean))
spl180 <- maptools::elide(spl, rotate=180, center=apply(bbox(spl), 1, mean))

plot(spl)
plot(spl90)
plot(spl180)
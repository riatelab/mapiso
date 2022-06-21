library(sf)
library(terra)


r <- rast(system.file("tif/elevation.tif", package = "mapiso"))
s <- st_read(system.file("gpkg/elevation.gpkg", package = "mapiso"), quiet = TRUE)
d <- read.csv(system.file("csv/elevation.csv", package = "mapiso"))
bks <-c(98,100, 150, 200, 250, 300, 350, 400, 412.6)



expect_silent(mapiso(x = r))

expect_silent(mapiso(x = s, var = "elevation", breaks = bks))

expect_silent(mapiso(x = d, var = 'elevation', coords = c('x', 'y'), crs = 'epsg:2154'))






# test errors

expect_error(mapiso("textx"))
r$coco <- r$elevation +1
expect_error(mapiso(r))

expect_error(mapiso(s, var = "nope"))

expect_error(mapiso(s))

expect_error(mapiso(s[-1, ], var = "elevation"))

expect_error(mapiso(d))

expect_error(mapiso(d, var = "elevation"))

expect_error(mapiso(d, var = "nope", coords = c("x", "y")))


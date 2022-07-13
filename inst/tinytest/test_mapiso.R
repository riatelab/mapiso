suppressPackageStartupMessages(library(sf))
s <- st_read(system.file("gpkg/elevation.gpkg", package = "mapiso"),
             layer = "elevation", quiet = TRUE)
m <- st_read(system.file("gpkg/elevation.gpkg", package = "mapiso"),
             layer = "com", quiet = TRUE)
d <- read.csv(system.file("csv/elevation.csv", package = "mapiso"))
bks <-c(98,100, 150, 200, 250, 300, 350, 400, 412.6)



if(suppressPackageStartupMessages(require(terra, quietly = TRUE))){
  r <- rast(system.file("tif/elevation.tif", package = "mapiso"))
  expect_silent(mapiso(x = r))
  r$coco <- r$elevation +1
  expect_error(mapiso(r))
  expect_silent(mapiso(x = vect(s), var = "elevation",
                       breaks = bks, mask = vect(m)))

}

expect_silent(mapiso(x = s, var = "elevation", breaks = bks, mask = m))
expect_silent(mapiso(x = s, var = "elevation", breaks = bks))
expect_silent(mapiso(x = d, var = 'elevation', coords = c('x', 'y'),
                     crs = 'epsg:2154'))



# test errors
expect_error(mapiso("textx"))
expect_silent(mapiso(x = s, var = "elevation", breaks = bks,
                    mask = st_transform(m, 4326)))
expect_error(mapiso(s, var = "nope"))
expect_error(mapiso(s))
expect_error(mapiso(s[-1, ], var = "elevation"))
expect_error(mapiso(d))
expect_error(mapiso(d, var = "elevation"))
expect_error(mapiso(d, var = "nope", coords = c("x", "y")))


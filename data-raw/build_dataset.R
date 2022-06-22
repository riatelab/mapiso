library(terra)
ra <- terra::rast(system.file("tif/elev.tif", package = "tanaka"))
names(ra) <- "elevation"
ra2 <- ra
res(ra2) <- 250
a <- resample(ra, ra2)
terra::writeRaster(a, "inst/tif/elevation.tif", overwrite = T)
ra <- terra::rast("inst/tif/elevation.tif")
df <- as.data.frame(ra, xy = T)
write.csv(df, "inst/csv/elevation.csv", row.names = F)
df <- read.csv("inst/csv/elevation.csv")
sdf <- sf::st_as_sf(df, coords = c("x", "y"), crs = 2154)
sf::st_write(sdf, "inst/gpkg/elevation.gpkg", append = FALSE)
com <- sf::st_read(system.file("gpkg/com.gpkg", package = "tanaka"),
                   quiet = TRUE)
sf::st_write(com, "inst/gpkg/elevation.gpkg", layer = "com", append = FALSE)

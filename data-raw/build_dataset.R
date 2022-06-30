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
  quiet = TRUE
)
sf::st_write(com, "inst/gpkg/elevation.gpkg", layer = "com", append = FALSE)



## Build logo

library(mapiso)
library(sf)
library(mapsf)
# gridded data
s <- st_read(system.file("gpkg/elevation.gpkg", package = "mapiso"),
  layer = "elevation", quiet = TRUE
)
# mask
m <- st_read(system.file("gpkg/elevation.gpkg", package = "mapiso"),
  layer = "com", quiet = TRUE
)
# custom breaks
bks <- c(98, 100, 150, 200, 250, 300, 350, 400, 412.6)
isos <- mapiso(x = s, var = "elevation", breaks = bks)
mf_export(m, "toto.svg")
mf_map(isos, "isomin", "choro",
  add = T,
  breaks = bks, border = NA, leg_pos = NA,
  leg_title = "elevation", pal = "Sunset"
)
mf_map(s, "elevation", "prop",
  leg_pos = NA,
  col = "#ffffff80", inches = .05, border = "#00000050"
)
mf_map(isos, add = T, border = "grey30", col = NA, lwd = .5)

dev.off()

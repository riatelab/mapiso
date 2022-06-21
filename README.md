
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapiso

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![R-CMD-check](https://github.com/rCarto/mapiso/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rCarto/mapiso/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `mapiso` is to create isopleth polygon sf object from raster
or regular grids.

## Installation

You can install the development version of `mapiso` from GitHub with:

``` r
remotes::install_github("rcarto/mapiso")
```

## Usage

### Raster

``` r
library(mapiso)
library(terra)
#> terra 1.5.34
library(mapsf)
r <- rast("inst/tif/elevation.tif")
isor <- mapiso(x = r)
mf_theme(mar = c(0, 0, 0, 0))
mf_raster(r)
mf_map(isor, col = NA, add = TRUE)
```

<img src="man/figures/README-raster-1.png" width="80%" />

### sf regular grid

``` r
library(mapiso)
library(sf)
#> Linking to GEOS 3.9.0, GDAL 3.2.2, PROJ 7.2.1; sf_use_s2() is TRUE
library(mapsf)
s <- st_read("inst/gpkg/elevation.gpkg", quiet = TRUE)
bks <-c(98,100, 150, 200, 250, 300, 350, 400, 412.6) 
isos <- mapiso(x = s, var = "elevation", breaks = bks)
mf_map(isos, "isomin", "choro", breaks = bks, leg_title = "elevation")
```

<img src="man/figures/README-sf-1.png" width="80%" />

### data.frame

``` r
library(mapiso)
library(mapsf)
d <- read.csv("inst/csv/elevation.csv")
head(d)
#>          x       y elevation
#> 1 586231.3 6431677  282.0299
#> 2 586481.3 6431677  263.7194
#> 3 586731.3 6431677  244.9597
#> 4 586981.3 6431677  222.3232
#> 5 587231.3 6431677  211.1522
#> 6 587481.3 6431677  232.1604
isod <- mapiso(x = d, var = 'elevation', coords = c('x', 'y'), crs = 'epsg:2154')
bks <- unique(c(isod$isomin, isod$isomax))
mf_map(isod, "isomin", "choro", breaks = bks, leg_title = "elevation")
```

<img src="man/figures/README-data.frame-1.png" width="80%" />

## Community Guidelines

One can contribute to the package through [pull
requests](https://github.com/rcarto/mapiso) and report issues or ask
questions [here](https://github.com/rcarto/mapiso/issues).  
This project uses [conventional
commits](https://www.conventionalcommits.org/en/v1.0.0-beta.3/) and
[semantic versioning](https://semver.org/).

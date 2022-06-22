#' Title
#'
#' @param x a data.frame, an sf data.frame or a SpatRaster.
#' @param var df and sf only
#' @param breaks list of break values (default to equal interval)
#' @param nbreaks nclass number of classes
#' @param mask sf object
#' @param coords df only, vector of names (x and y)
#' @param crs df only epsg or other code
#' @importFrom sf st_union st_intersection st_cast st_agr<- st_coordinates
#' st_crs st_geometry st_geometry<- st_make_valid st_sf st_sfc
#' st_collection_extract
#' @importFrom isoband isobands iso_to_sfg
#' @return and sf object of iso polygones
#' @export
#' @examples
#' # terra
#' if (require(terra, quietly = TRUE)) {
#'   r <- rast(system.file("tif/elevation.tif", package = "mapiso"))
#'   isor <- mapiso(x = r)
#'   plot(r)
#'   library(sf)
#'   plot(st_geometry(isor), add = TRUE, col = NA)
#' }
#'
#' # sf, using a mask
#' s <- st_read(system.file("gpkg/elevation.gpkg", package = "mapiso"),
#'   layer = "elevation", quiet = TRUE
#' )
#' m <- st_read(system.file("gpkg/elevation.gpkg", package = "mapiso"),
#'   layer = "com", quiet = TRUE
#' )
#' isos <- mapiso(
#'   x = s, var = "elevation",
#'   mask = m
#' )
#' plot(isos)
#'
#' # data.frame, using user breaks values
#' d <- read.csv(system.file("csv/elevation.csv", package = "mapiso"))
#' bks <- c(98, 100, 150, 200, 250, 300, 350, 400, 412.6)
#' isod <- mapiso(
#'   x = d, var = "elevation",
#'   breaks = bks, coords = c("x", "y"), crs = "epsg:2154"
#' )
#' plot(isod)
#' if (require(mapsf, quietly = TRUE)) {
#'   mf_map(isod, "isomin", "choro", breaks = bks, leg_title = "Elevation")
#' }
mapiso <- function(x, var, breaks, nbreaks = 8, mask, coords, crs) {
  # test inputs
  if (!inherits(x = x, what = c("SpatRaster", "sf", "data.frame"))) {
    stop(
      "'x' should be a data.frame, an sf data.frame or a SpatRaster.",
      call. = FALSE
    )
  }

  if (inherits(x = x, what = "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(
        paste0(
          "This function needs the 'terra' package to work with ",
          "SpatRaster objects. Please install it."
        ),
        call. = FALSE
      )
    }
    if (terra::nlyr(x) != 1) {
      stop(
        "'x' should be a single layer SpatRaster.",
        call. = FALSE
      )
    }
    ext <- terra::ext(x)
    nc <- terra::ncol(x)
    nr <- terra::nrow(x)
    xr <- terra::xres(x) / 2
    yr <- terra::yres(x) / 2
    crs <- st_crs(x)
    lon <- seq(ext[1] + xr, ext[2] - xr, length.out = nc)
    lat <- seq(ext[4] - yr, ext[3] + yr, length.out = nr)
    m <- matrix(
      data = terra::values(x),
      nrow = nr,
      dimnames = list(lat, lon),
      byrow = TRUE
    )
  }


  if (inherits(x = x, what = "sf")) {
    if (missing(var)) {
      stop("'var' is missing.", call. = FALSE)
    }
    if (!var %in% names(x)) {
      stop("'var' is not a valid variable of 'x'.", call. = FALSE)
    }
    crs <- st_crs(x)
    x <- data.frame(st_coordinates(x), var = x[[var]])
    coords <- c("X", "Y")
    var <- "var"
  }


  if (inherits(x = x, what = "data.frame")) {
    if (missing(var)) {
      stop("'var' is missing.", call. = FALSE)
    }
    if (missing(coords)) {
      stop("'coords' is missing.", call. = FALSE)
    }
    if (!var %in% names(x)) {
      stop("'var' is not a valid variable of 'x'.", call. = FALSE)
    }
    if (length(unique(x[[coords[1]]])) * length(unique(x[[coords[2]]])) != length(x[[var]])) {
      stop(
        "It seems that 'x' is not a regular grid.",
        call. = FALSE
      )
    }

    m <- t(
      matrix(
        data = x[[var]],
        nrow = length(unique(x[[coords[1]]])),
        dimnames = list(
          unique(x[[coords[1]]]),
          unique(x[[coords[2]]])
        )
      )
    )
  }

  # breaks management
  vmin <- min(m, na.rm = TRUE)
  vmax <- max(m, na.rm = TRUE)
  if (missing(breaks)) {
    breaks <- seq(
      from = vmin,
      to = vmax,
      length.out = (nbreaks + 1)
    )
  } else {
    breaks <- sort(unique(c(vmin, breaks[breaks > vmin & breaks < vmax], vmax)))
  }
  lev_low <- breaks[1:(length(breaks) - 1)]
  lev_high <- breaks[2:length(breaks)]
  raw <- isobands(
    x = as.numeric(colnames(m)),
    y = as.numeric(rownames(m)),
    z = m,
    levels_low = lev_low,
    levels_high = c(lev_high[-length(lev_high)], vmax + 1e-10)
  )

  bands <- iso_to_sfg(raw)
  iso <- st_sf(
    id = 1:length(bands),
    isomin = lev_low,
    isomax = lev_high,
    geometry = st_sfc(bands),
    crs = crs
  )

  # invalid polygons mgmnt
  st_geometry(iso) <- st_make_valid(st_geometry(iso))

  if (inherits(st_geometry(iso), "sfc_GEOMETRYCOLLECTION") ||
    inherits(st_geometry(iso), "sfc_GEOMETRY")) {
    st_geometry(iso) <- st_collection_extract(st_geometry(iso), "POLYGON")
  }

  # masl mgmt
  if (!missing(mask)) {
    st_agr(iso) <- "constant"
    if (st_crs(iso) == st_crs(mask)) {
      iso <- st_cast(st_intersection(x = iso, y = st_union(st_geometry(mask))))
    } else {
      stop(
        "CRS of 'x' and 'mask' should be identical.",
        call. = FALSE
      )
    }
  }


  return(iso)
}

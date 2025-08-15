#' Clean source data.
#'
#' `gm_clean()` tests if all elements in the sf object are polygons and recasts
#'    them as MULTYPOLYGONs if necessary; validates the polygons; and reprojects
#'    the polygons to EPSG 6933, Global Equal Area.
#' @import rlang
#' @import dplyr
#' @import sf
#' @param geom An sf object to clean.
#' @returns A cleaned sf object.
#' @export
#' @examples
#' # Create a simple polygon as an sf object
#' library(sf)
#' geom <- st_as_sf(
#'   data.frame(
#'     id = 1,
#'     wkt = "POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))"
#'   ),
#'   wkt = "wkt",
#'   crs = 4326
#' )
#'
#' gm_clean(geom)

gm_clean <- function(geom) {

  # library(dplyr)
  # library(sf)

  sf::sf_use_s2()

  # test if features are polygons and recast as multipolygons is necessary
  if (all(sf::st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON"))) {
    message("All features are polygons.")

    if (any(sf::st_geometry_type(geom) != "MULTIPOLYGON")) {
      message("Recasting polygons as multipolygons.")
      geom <- sf::st_cast(geom, "MULTIPOLYGON")
    }

  } else {
    stop("Some features are not polygons. geomatchR only works with polygons.")
  }

  # Validate geometries
  if(all(sf::st_is_valid(geom) == TRUE)) {
    message("All geometries are valid.")
  } else {
    geom <- sf::st_make_valid(geom)
    message("Geometries have been validated.")
  }

  # Reproject to Global equal-area CRS https://epsg.io/6933
  if(sf::st_crs(geom) == 6933) {
    message("Features are projected to EPSG 6933.")
  } else {
    message("Reprojecting features to EPSG 6933.")
    geom <- sf::st_transform(geom, 6933)
  }

  message("Features are clean.")

  return(geom)

}


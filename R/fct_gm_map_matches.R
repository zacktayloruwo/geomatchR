#' Visually represent a single polygon's matches
#'
#' `gm_map_matches()` creates a map of all matches to a single specified polygon.
#'
#' @param polygon The identification code of the polygon in geom1 to match
#' @param geom1 The first polygon sf object
#' @param geom2 The second polygon sf object
#' @param id1 The unique polygon identification code of the first polygon sf object
#' @param id2 The unique polygon identification code of the second polygon sf object
#' @param name1 The unique polygon identification name of the first polygon sf object
#' @param name2 The unique polygon identification name of the second polygon sf object
#' @param table An object of class list created by `gm_crossmatch()`
#' @param context If TRUE, draws the boundaries of the first polygon sf object as background visual reference. Defaults to TRUE.
#' @param minimum Do not display matches with a similarity score less than value in context.min. Defaults to 0.05.
#' @returns Returns a ggplot object
#' @importFrom rlang sym
#' @import dplyr
#' @import ggplot2
#' @import sf
#' @import ggrepel
#' @import viridis
#' @export
#'
#' @examples
#' library(sf)
#'
#' # Create example polygon set 1
#' sf1 <- st_sf(
#'   geoid = c("A1", "A2", "A3"),
#'   geoname1 = c("Alpha", "Beta", "Gamma"),
#'   geometry = st_sfc(
#'     st_polygon(list(rbind(c(0,0), c(0,2), c(2,2), c(2,0), c(0,0)))),
#'     st_polygon(list(rbind(c(2,0), c(2,2), c(4,2), c(4,0), c(2,0)))),
#'     st_polygon(list(rbind(c(0,2), c(0,4), c(2,4), c(2,2), c(0,2))))
#'   ), crs = 4326
#' )
#'
#' # Create example polygon set 2
#' sf2 <- st_sf(
#'   geoid = c("B1", "B2", "B3"),
#'   geoname2 = c("Eins", "Zwei", "Drei"),
#'   geometry = st_sfc(
#'     st_polygon(list(rbind(c(0.25,0), c(0,2), c(2,2), c(2,0), c(0.25,0)))), # same
#'     st_polygon(list(rbind(c(2,0), c(2,2), c(4,2), c(4,0), c(2,0)))),
#'     st_polygon(list(rbind(c(0,2), c(0.5,4), c(2.25,4), c(2.75,2), c(0,2))))
#'   ), crs = 4326
#' )
#'
#' # Run gm_crossmatch
#' result <- gm_crossmatch(
#'   geom1 = sf1,
#'   geom2 = sf2,
#'   id1 = "geoid",
#'   id2 = "geoid",
#'   name1 = "geoname1",
#'   name2 = "geoname2",
#'   cutoff = 0.5,
#'   clean = TRUE
#' )
#'
#' # map matches of polygon 'A1'
#' gm_map_matches(
#'   polygon = "A1",
#'   geom1 = sf1,
#'   geom2 = sf2,
#'   id1 = "geoid",
#'   id2 = "geoid",
#'   name1 = "geoname1",
#'   name2 = "geoname2",
#'   table = result,
#'   context = TRUE,
#'   context.min = .05
#' )


gm_map_matches <- function(polygon, geom1, geom2, id1, id2, name1, name2, table, context = TRUE, minimum = .05) {

  `%notin%` <- Negate(`%in%`)

  lwd = .25
  s = 2
  a = .8
  f = 50

  if (polygon %notin% geom1[[id1]]) {
    stop(paste0("Polygon with identifier ", deparse(substitute(id1)), " does not exist in layer ", deparse(substitute(geom1)), "."))
  }

  # declare variables
  id <- similarity_area <- NULL

  # plot
    g1 <- geom1 |>
      dplyr::rename(id = !!rlang::sym(id1)) |>
      dplyr::filter(.data$id == polygon)

    g2 <- geom2 |>
      dplyr::rename(id = !!rlang::sym(id2)) |>
      dplyr::right_join(table[['similarity']] |>
                        dplyr::filter(.data$id1 == polygon, .data$similarity_area >= minimum) |>
                        dplyr::select(id1, id2, similarity_area),
                by = dplyr::join_by(id == id2)
                ) |>
      dplyr::arrange(dplyr::desc(.data$similarity_area)) |>
      dplyr::mutate(rank = dplyr::row_number())

    ## suppresses warning during st_centroid()
    sf::st_agr(g1) = "constant"
    sf::st_agr(g2) = "constant"

    lim = 0 # min(g2$similarity_area)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = g2, ggplot2::aes(fill = .data$similarity_area), colour = "#000000", lwd = lwd * 2) +
      ggplot2::geom_sf(data = g1, fill = NA, colour = "red", lwd = lwd * 4) +
      viridis::scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      ggrepel::geom_label_repel(ggplot2::aes(label = paste0(.data$id, "\n s = ", round(.data$similarity_area,2)), geometry = .data$geometry),
                                         size = s, color = "black", stat = "sf_coordinates",
                                         min.segment.length = 0, segment.color = "black",
                                         max.overlaps = Inf, alpha = a, seed = 519,
                                         force = f, force_pull = -f,
                                         data = suppressWarnings(sf::st_centroid(g2))) +
      ggplot2::geom_label(ggplot2::aes(label = .data$id, geometry = .data$geometry),
                          size = s * 2, color = "red", stat = "sf_coordinates",
                          data = suppressWarnings(sf::st_centroid(g1))
      ) +
      ggplot2::labs(subtitle = paste0("Matches for polygon '", polygon, "'"),
                    caption = paste0("Minimum similarity score mapped: ", minimum),
                    x = "", y = "") +
      ggplot2::theme_bw()

    if(context == TRUE) {
      p <- p +
        ggplot2::geom_sf(data = geom1 |> dplyr::summarise(), fill = "grey90",
                         colour = "grey70", lwd = lwd * 2)

      p$layers <- p$layers[c(5, 1, 2, 3, 4)]

      }

    return(p)

}

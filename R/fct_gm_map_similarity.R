#' Visually represent similarity scores
#'
#' `gm_map_similarity()` creates a map of similarity scores.
#'
#' @param geom1 The first polygon sf object
#' @param geom2 The second polygon sf object
#' @param id1 The unique polygon identification code of the first polygon sf object
#' @param id2 The unique polygon identification code of the second polygon sf object
#' @param name1 The unique polygon identification name of the first polygon sf object
#' @param name2 The unique polygon identification name of the second polygon sf object
#' @param table An object of class list created by `gm_crossmatch()`
#' @param label.cutoff Minimum similarity score of polygons to label. Defaults to 0.8
#' @param labels "id" to show polygon identifiers defined by id1 and id2;
#'   "name" to show polygon names defined by name1 and name2;
#'   "none" to suppress polygon labels. Defaults to "none".
#' @param plot "combined" or "c" to create a two-panel plot showing forward and backward comparisons;
#'   "forward" or "f" to show forward comparison only; "backward" or "b" to show backward comparison only.
#' @returns Returns a plot object
#' @importFrom rlang sym
#' @import dplyr
#' @import ggplot2
#' @import sf
#' @import ggrepel
#' @import viridis
#' @import ggpubr
#' @export
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
#' # map similarity
#' gm_map_similarity(geom1 = sf1,
#'                   geom2 = sf2,
#'                   id1 = "geoid",
#'                   id2 = "geoid",
#'                   name1 = "geoname1",
#'                   name2 = "geoname2",
#'                   table = result,
#'                   label.cutoff = .5,
#'                   labels = "name",
#'                   plot = "combined"
#' )

gm_map_similarity <- function(geom1, geom2, id1, id2, name1, name2, table,
                              label.cutoff = .8, labels = "none",
                              plot = "combined") {

  `%notin%` <- Negate(`%in%`)

  lwd = .25
  a = .8
  s = 2
  f = 75

  # declare variables
  id <- similarity_area <- NULL

  # plot = c("forward", "f")
  if (plot %in% c("forward", "f")) {

    g <- geom1 |>
      dplyr::rename(id = !!rlang::sym(id1)) |>
      dplyr::left_join(table[['similarity']] |>
                         dplyr::filter(.data$rank_areal_f == 1) |>
                         dplyr::select(id1, similarity_area),
                by = dplyr::join_by(id == id1))

    # if name does not exist, make identical to id
    if (!is.na(name1)) { g <- g |> dplyr::rename(name = !!rlang::sym(name1))}
    if (is.na(name1)) { g <- g |> dplyr::mutate(name = .data$id) }

    ## suppresses warning during st_centroid()
    sf::st_agr(g) = "constant"

    lim = min(g$similarity_area)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = g, ggplot2::aes(fill = similarity_area), colour = "#000000", lwd = lwd) +
      viridis::scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      ggplot2::labs(title = "Similarity scores, forward matching",
           x = "", y = "") +
      ggplot2::theme_bw()

    if (labels %in% c("id", "name")) {

      p <- p + ggrepel::geom_label_repel(ggplot2::aes(label = paste0(!!rlang::sym(labels), "\n", round(.data$similarity_area,2)), geometry = .data$geometry),
                                         size = s, color = "red", stat = "sf_coordinates",
                                         min.segment.length = 0, segment.color = "red",
                                         max.overlaps = Inf, alpha = a, seed = 519, force = f,
                                         data = sf::st_centroid(g |> dplyr::filter(.data$similarity_area < label.cutoff))) +
        ggplot2::labs(caption = paste0("Label display cut-off: < ", label.cutoff))
    }

    return(p)

  }

  # plot = c("backward", "b")
  if (plot %in% c("backward", "b")) {

    g <- geom2 |>
      dplyr::rename(id = !!rlang::sym(id2)) |>
      dplyr::left_join(table[['similarity']] |>
                         dplyr::filter(.data$rank_areal_b == 1) |>
                         dplyr::select(id2, similarity_area),
                by = dplyr::join_by(id == id2))

    # if name does not exist
    if (!is.na(name2)) { g <- g |> dplyr::rename(name = !!rlang::sym(name2))}
    if (is.na(name2)) { g <- g |> dplyr::mutate(name = .data$id) }

    ## suppresses warning during st_centroid()
    sf::st_agr(g) = "constant"

    lim = min(g$similarity_area)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = g, ggplot2::aes(fill = .data$similarity_area), colour = "#000000", lwd = lwd) +
      viridis::scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      ggplot2::labs(title = "Similarity scores, backward matching",
           x = "", y = "") +
      ggplot2::theme_bw()

    if (labels %in% c("id", "name")) {

      p <- p + ggrepel::geom_label_repel(ggplot2::aes(label = paste0(!!rlang::sym(labels), "\n", round(.data$similarity_area,2)), geometry = .data$geometry),
                                         size = s, color = "red", stat = "sf_coordinates",
                                         min.segment.length = 0, segment.color = "red",
                                         max.overlaps = Inf, alpha = a, seed = 519, force = f,
                                         data = sf::st_centroid(g |> dplyr::filter(.data$similarity_area < label.cutoff))) +
        ggplot2::labs(caption = paste0("Label display cut-off: < ", label.cutoff))
    }

    return(p)

  }

  # plot = c("combined", "c")
  if (plot %in% c("combined", "c")) {

    geom_1 <- geom1 |> dplyr::rename(id = !!rlang::sym(id1)) |> dplyr::left_join(table[['similarity']] |> dplyr::filter(.data$rank_areal_f == 1) |> dplyr::select(id1, similarity_area), by = dplyr::join_by(id == id1))
    geom_2 <- geom2 |> dplyr::rename(id = !!rlang::sym(id2)) |> dplyr::left_join(table[['similarity']] |> dplyr::filter(.data$rank_areal_b == 1) |> dplyr::select(id2, similarity_area), by = dplyr::join_by(id == id2))

    # if name does not exist
    if (!is.na(name1)) { geom_1 <- geom_1 |> dplyr::rename(name = !!rlang::sym(name1))}
    if (is.na(name1)) { geom_1 <- geom_1 |> dplyr::mutate(name = .data$id) }

    if (!is.na(name2)) { geom_2 <- geom_2 |> dplyr::rename(name = !!rlang::sym(name2))}
    if (is.na(name2)) { geom_2 <- geom_2 |> dplyr::mutate(name = .data$id) }

    ## suppresses warning during st_centroid()
    sf::st_agr(geom_1) = "constant"
    sf::st_agr(geom_2) = "constant"

    lim = min(c(min(geom_1$similarity_area), min(geom_2$similarity_area)))

    p1 <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = geom_2, fill = NA, color = NA) +
      ggplot2::geom_sf(data = geom_1, ggplot2::aes(fill = .data$similarity_area), colour = "grey50", lwd = lwd) +
      viridis::scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      ggplot2::labs(title = "Similarity scores, forward matching", x = "", y = "") +
      ggplot2::theme_bw()

    p2 <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = geom_1, fill = NA, color = NA) +
      ggplot2::geom_sf(data = geom_2, ggplot2::aes(fill = .data$similarity_area), colour = "grey50", lwd = lwd) +
      viridis::scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      ggplot2::labs(title = "Similarity scores, backward matching", x = "", y = "") +
      ggplot2::theme_bw()

    if (labels %in% c("id", "name")) {

      p1 <- p1 + ggrepel::geom_label_repel(ggplot2::aes(label = paste0(!!rlang::sym(labels), "\n", round(similarity_area,2)), geometry = .data$geometry),
                                           size = s, color = "red", stat = "sf_coordinates",
                                           min.segment.length = 0, segment.color = "red",
                                           max.overlaps = Inf, alpha = a, seed = 519, force = f,
                                           data = sf::st_centroid(geom_1 |> dplyr::filter(.data$similarity_area >= label.cutoff))) +
        ggplot2::labs(caption = paste0("Label display cut-off: < ", label.cutoff))

      p2 <- p2 + ggrepel::geom_label_repel(ggplot2::aes(label = paste0(!!rlang::sym(labels), "\n", round(similarity_area,2)), geometry = .data$geometry),
                                         size = s, color = "red", stat = "sf_coordinates",
                                         min.segment.length = 0, segment.color = "red",
                                         max.overlaps = Inf, alpha = a, seed = 519, force = f,
                                         data = sf::st_centroid(geom_2 |> dplyr::filter(.data$similarity_area >= label.cutoff))) +
        ggplot2::labs(caption = paste0("Label display cut-off: < ", label.cutoff))
    }

    p <- ggpubr::ggarrange(p1, p2, ncol = 1, legend = "bottom", common.legend = TRUE, align ="hv")
    return(p)

  }

  if (plot %notin% c("combined", "forward", "backward", "c", "f", "b")) {
    stop("Plot not correctly specified")
  }

}

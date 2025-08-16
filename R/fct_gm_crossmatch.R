#' Calculates similarity scores and other diagnostics
#'
#' `gm_crossmatch()` compares polygon sf objects geom1 and geom2 and returns a list
#'    of tables that help the user assess which polygons are high-probability matches.
#'    The returned list can be used to cartographically visualize similarity scores using
#'    the `gm_map_similarity()` and `gm_map_matches()` functions.
#'
#' @param geom1 The first polygon sf object
#' @param geom2 The second polygon sf object
#' @param id1 The unique polygon identification code of the first polygon sf object
#' @param id2 The unique polygon identification code of the second polygon sf object
#' @param name1 The unique polygon identification name of the first polygon sf object
#' @param name2 The unique polygon identification name of the second polygon sf object
#' @param cutoff Minimal areal similarity cut-off. Defaults to 0.8
#' @param clean Make TRUE if you want to run `gm_clean()` on the sf objects. Defaults to TRUE
#' @returns Returns a list of tables:
#'   \itemize{
#'     \item similarity = match scores for all overlapping polygons
#'     \item top_ranked = top-ranked matches where rank_areal_f and rank_areal_b = 1 and similarity > cutoff
#'     \item no_match1 = polygons in geom1 without top-ranked matches
#'     \item no_match2 = polygons in geom2 without top-ranked matches
#'   }
#' @importFrom rlang sym
#' @importFrom stats dist
#' @import dplyr
#' @import sf
#' @import units
#' @import tidyr
#' @import tibble
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
#' # Inspect outputs
#' names(result)
#' result$similarity
#' result$top_ranked
#' result$no_match1
#' result$no_match2


# outputs:
# similarity_area = similarity score based on areal overlap. Higher is better.
# area1 = area of first polygon (km2)
# area2 = area of matched second polygon (km2)
# overlap = area of overlap between both polygons (km2)
# pct_f = percentage overlap, forward comparison
# pct_b = percentage overlap, backward comparison
# pct_diff = difference between pct_f and pct_b, divided by 2
# rank_areal_f = rank order of first polygons (id1) by similarity_area
# rank_areal_b = rank order of second polygons (id2) by similarity_area
# gap_f = difference between the similarity score of the top-ranked and second-ranked forward match
# gap_b = difference between the similarity score of the top-ranked and second-ranked backward match
# gap_areal_diff = absolute difference between gap_f and gap_b (smaller is better)
# width1 = maximum width of first polygon
# width2 = maximum width of matched second polygon
# distance = distance between the centroids of the matched polygons (m)
# dist_ratio1 = distance / width1
# dist_ratio2 = distance / width2
# rank_dist_f = rank order of first polygons (id1) by distance (lower value means centroids are closer)
# rank_dist_b = rank order of second polygons (id2) by distance (lower value means centroids are closer)

gm_crossmatch <- function(geom1, geom2, id1, id2, name1, name2, cutoff = .8, clean = TRUE){

  `%notin%` <- Negate(`%in%`)

  # declare variables
  overlap <- pct_f <- pct_b <- NULL
  rank_areal_f <- rank_areal_b <- similarity_area <- gap_f <- gap_b <- gap_areal_diff <- width1 <- width2 <- gap_width <- distance <- rank_dist_f <- rank_dist_b <- dist_ratio1 <- dist_ratio2 <- NULL

  # clean files if necessary
  if (clean == TRUE) {
    if (exists("gm_clean", mode = "function")) {
      message("** Cleaning ", deparse(substitute(geom1)))
      geom1 <- gm_clean(geom1)
      message("** Cleaning ", deparse(substitute(geom2)))
      geom2 <- gm_clean(geom2)
    } else {
      warning("gm_clean() not found. Skipping cleaning step.")
    }
  }

  # Clean polygons safely
  if (clean) {
    if (exists("gm_clean", mode = "function")) {
      message("** Cleaning ", deparse(substitute(geom1)))
      geom1 <- gm_clean(geom1)
      message("** Cleaning ", deparse(substitute(geom2)))
      geom2 <- gm_clean(geom2)
    } else {
      warning("gm_clean() not found. Skipping cleaning step.")
    }
  }

  # rename id and name columns
  geom1 <- geom1 |> dplyr::rename(id1 = !!rlang::sym(id1))
  geom2 <- geom2 |> dplyr::rename(id2 = !!rlang::sym(id2))

  if (!is.na(name1)) { geom1 <- geom1 |> dplyr::rename(name = !!rlang::sym(name1))}
  if (is.na(name1)) { geom1 <- geom1 |> dplyr::mutate(name = NA) }

  if (!is.na(name2)) { geom2 <- geom2 |> dplyr::rename(name = !!rlang::sym(name2))}
  if (is.na(name2)) { geom2 <- geom2 |> dplyr::mutate(name = NA) }

  # calculate areas
  message("Calculating areas.")
  geom1$area1 <- sf::st_area(geom1)
  geom2$area2 <- sf::st_area(geom2)

  # calculate maximum widths
  message("calculating maximum widths.")
  geom1 <- geom1 |>
    dplyr::rowwise() |>
    dplyr::mutate(width1 = {
          mrr <- sf::st_minimum_rotated_rectangle(.data$geometry)
          coords <- sf::st_coordinates(mrr)[, 1:2]  # x and y only
          dists <- as.matrix(stats::dist(coords))
          max(dists)
          }
          ) |>
    dplyr::ungroup()

  geom2 <- geom2 |>
    dplyr::rowwise() |>
    dplyr::mutate(width2 = {
      mrr <- sf::st_minimum_rotated_rectangle(.data$geometry)
      coords <- sf::st_coordinates(mrr)[, 1:2]  # x and y only
      dists <- as.matrix(stats::dist(coords))
      max(dists)
      }
      ) |>
    dplyr::ungroup()

  message("Areal units: ", units::deparse_unit(geom1$area1))

  # drop geometry and unit formatting and express areas in sqkm
  area1 <- units::drop_units(sf::st_drop_geometry(geom1))
  area1$area1 = area1$area1 / 1000000

  area2 <- units::drop_units(sf::st_drop_geometry(geom2))
  area2$area2 = area2$area2 / 1000000

  # Intersect geom1 and geom2 polygons
  message("Intersecting polygons.")

  ## suppresses warning during st_intersection()
  sf::st_agr(geom1) = "constant"
  sf::st_agr(geom2) = "constant"

  intersection <- sf::st_intersection(geom1, geom2)

  # calculate area of overlap between all geom1 and geom2 polygons
  intersection$overlap = sf::st_area(intersection)
  # drop geometry and unit formatting and express areas in sqkm
  intarea <- sf::st_drop_geometry(intersection) |> units::drop_units()
  intarea$overlap = intarea$overlap / 1000000
  intarea <- intarea |> dplyr::select(id1, id2, overlap)

  message("Calculating areal overlap.")
  # Calculate % overlap going forward and backward
  forward <- area2 |>
    # join areas of intersections to areas of geom2 polygons on the geom2 id
    dplyr::left_join(intarea, by = "id2", multiple = "all") |>
    # calculate forward areal overlap
    dplyr::mutate(pct_f = round(.data$overlap / .data$area2, 4)) |>
    dplyr::rename(overlap_f = .data$overlap)

  if (max(forward$pct_f, na.rm = TRUE) > 1) {
    message("Forward overlap is greater than 1. Max = ", max(forward$pct_f, na.rm = TRUE), ". Setting to 1.")
    # if pct overlap > 100% due to area calculation error, then set to 1
    forward <- forward |> dplyr::mutate(pct_f = ifelse(.data$pct_f > 1, 1, .data$pct_f))
  }

  backward <- area1 |>
    # join areas of intersections to areas of geom1 polygons on the geom1 id
    dplyr::left_join(intarea, by = "id1", multiple = "all") |>
    # calculate backward areal overlap
    dplyr::mutate(pct_b = round(.data$overlap / .data$area1, 4)) |>
    dplyr::rename(overlap_b = .data$overlap)

  if (max(backward$pct_b, na.rm = TRUE) > 1) {
    message("Backward overlap is greater than 1. Max = ", max(backward$pct_b, na.rm = TRUE), ". Setting to 1.")
    # if pct overlap > 100% due to area calculation error, then set to 1
    backward <- backward |> dplyr::mutate(pct_b = ifelse(.data$pct_b > 1, 1, .data$pct_b))
  }

  similarity <- list()

  # Compare forward and backward % overlap and generate areal similarity score
  message("Calculating similarity scores based on areal overlap.")

  vars <- c("id1", "name1", "id2", "name2", "similarity_area", "pct_f", "pct_b",
            "gap_f", "gap_b", "gap_areal_diff",
            "rank_areal_f", "rank_areal_b",
            "width1", "width2", "gap_width"
  )

  similarity[['similarity']] <- dplyr::full_join(forward, backward, by = c("id1", "id2"), multiple = "all") |>
    dplyr::relocate(id1) |>
    # drop if pct = NA, meaning there is no overlap
    dplyr::filter(!is.na(.data$pct_f)) |>
    dplyr::filter(!is.na(.data$pct_b)) |>
    # calculate difference from mean percentage overlap: small is good
    dplyr::mutate(pct_diff = abs(.data$pct_f - .data$pct_b) / 2) |>
    # maximize product of % overlap while minimizing difference of %s from mean of %s
    dplyr::mutate(similarity_area = (.data$pct_f * .data$pct_b) * (1 - .data$pct_diff)) |>
    dplyr::mutate(similarity_area = round(.data$similarity_area, 4)) |>
    # rank y2 scores in descending order within each y1 polygon
    dplyr::group_by(id1) |>
      dplyr::arrange(id1, dplyr::desc(.data$similarity_area)) |>
      dplyr::mutate(rank_areal_f = dplyr::row_number()) |>
      # calculate gap between first and second score in group
      # if only one row in group, gap is maximum (= 1)
      dplyr::mutate(gap_f = dplyr::first(.data$similarity_area) - dplyr::nth(.data$similarity_area, 2)) |>
      dplyr::mutate(gap_f = ifelse(is.na(.data$gap_f), 1, .data$gap_f)) |>
    dplyr::ungroup() |>
    # rank y1 scores in descending order within each y2 polygon
    dplyr::group_by(.data$id2) |>
      dplyr::arrange(.data$id2, dplyr::desc(.data$similarity_area)) |>
      dplyr::mutate(rank_areal_b = dplyr::row_number()) |>
        # calculate gap between first and second score in group
        # if only one row in group, gap is maximum (= 1)
      dplyr::mutate(gap_b = dplyr::first(.data$similarity_area) - dplyr::nth(.data$similarity_area, 2)) |>
      dplyr::mutate(gap_b = ifelse(is.na(.data$gap_b), 1, .data$gap_b)) |>
    dplyr::ungroup() |>
    dplyr::mutate(gap_areal_diff = abs(.data$gap_f - .data$gap_b)) |>
    # calculate width gap
    dplyr::mutate(gap_width = abs(.data$width1 - .data$width2)) |>
    # sort table by descending y2 score within each y1 polygon
    dplyr::arrange(id1, dplyr::desc(.data$similarity_area)) |>
    dplyr::select(dplyr::any_of(vars))

  message("Calculating distances between centroids.")
  # calculate distances between centroids
  dist_matrix <- sf::st_distance(sf::st_centroid(geom1), sf::st_centroid(geom2))
  rownames(dist_matrix) <- geom1$id1
  colnames(dist_matrix) <- geom2$id2

  dist_matrix <- dist_matrix |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "id1") |>
    tidyr::pivot_longer(-id1, names_to = "id2", values_to = "distance") |>
    dplyr::group_by(.data$id1) |>
      dplyr::arrange(.data$id1, .data$distance) |>
      dplyr::mutate(rank_dist_f = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$id2) |>
      dplyr::arrange(.data$id2, .data$distance) |>
      dplyr::mutate(rank_dist_b = dplyr::row_number()) |>
    dplyr::ungroup()

  similarity[['similarity']] <- similarity[['similarity']] |>
    dplyr::left_join(dist_matrix, by = c("id1", "id2")) |>
    dplyr::mutate(dist_ratio1 = .data$distance / .data$width1) |>
    dplyr::mutate(dist_ratio2 = .data$distance / .data$width2)

  message("Identifying top-ranked matches.")
  # top-ranked matches
  similarity[['top_ranked']] <- similarity[['similarity']] |>
    dplyr::filter(.data$rank_areal_f == 1 & .data$rank_areal_b == 1 & .data$similarity_area >= cutoff)

  message("Identifying polygons with no good matches.")
  # Identify polygons with no good matches
  keep <- area1 |> dplyr::filter(.data$id1 %notin% similarity[['top_ranked']][['id1']]) |> dplyr::pull(id1)
  similarity[['no_match1']] <- similarity[['similarity']] |> dplyr::filter(.data$id1 %in% keep)

  keep <- area2 |> dplyr::filter(.data$id2 %notin% similarity[['top_ranked']][['id2']]) |> dplyr::pull(id2)
  similarity[['no_match2']] <- similarity[['similarity']] |> dplyr::filter(.data$id2 %in% keep)

  return(similarity)

}


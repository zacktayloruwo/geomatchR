#' Global summary statistics
#'
#' `gm_global()` returns a list of global summary statistics from the output of [gm_crossmatch()].
#'
#' @param table An object of class list created by [gm_crossmatch()]
#' @param cutoff Minimum areal similarity cut-off. Defaults to 0.8
#' @param top.only Only include top-ranked matches in the quantile table and histogram
#' @returns The function returns a list of tables:
#'   \itemize{
#'     \item top.only = TRUE if quantile table and histogram include only top-ranked matches
#'     \item quantiles = a `quantile()` summary of the similarity scores
#'     \item histogram = a histogram of the similarity scores with the cutoff indicated
#'     \item matched_cutoff1 = proportion of geom1 polygons matched relative to cutoff
#'     \item matched_cutoff2 = proportion of geom2 polygons matched relative to cutoff
#'     \item matched_toprank1 = proportion of geom1 polygons 'top rank' matched
#'     \item matched_toprank2 = proportion of geom2 polygons 'top rank' matched
#'     }
#' @importFrom rlang sym
#' @importFrom stats quantile
#' @import dplyr
#' @import ggplot2
#' @import sf
#' @export
#' @examples
#' library(geomatchR)
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
#' # global summary statistics
#' global <- gm_global(
#'   table = result,
#'   cutoff = 0.5,
#'   top.only = TRUE
#' )
#'
#' # inspect outputs
#' global$top.only
#' global$quantiles
#' global$histogram
#' global$matched_cutoff1
#' global$matched_cutoff2
#' global$matched_toprank1
#' global$matched_toprank2

gm_global <- function(table, cutoff = 0.8, top.only = TRUE) {

  `%notin%` <- Negate(`%in%`)
  bin = .05

  df <- table[['similarity']]

  if (top.only %notin% c(TRUE, FALSE)) {
    stop("top.only must be TRUE or FALSE.")
  }

  # declare variables
  summary <- id <- s <- i <- . <- NULL

  summary[['top.only']] <- top.only

  message("Generating quantile summary.")
  if (top.only == FALSE) {
    q <- df |> dplyr::pull(s) |> sort() |>
    stats::quantile(probs = seq(0, 1, .1), na.rm = FALSE)

  summary[['quantiles']] <- tibble::tibble(
    quantile = names(q),
    value = as.numeric(q)
  )
  }

  if (top.only == TRUE) {
    q <- df |>
      dplyr::filter(.data$rank_areal_f == 1 & .data$rank_areal_b == 1 & .data$s >= cutoff) |>
      dplyr::pull(s) |> sort() |>
      stats::quantile(probs = seq(0, 1, .1), na.rm = FALSE)

    summary[['quantiles']] <- tibble::tibble(
      quantile = names(q),
      value = as.numeric(q)
    )
  }

  message("Generating histogram.")
  if (top.only == FALSE) {
  summary[['histogram']] <- df |>
    ggplot(aes(s)) +
    geom_histogram(binwidth = bin, fill = "grey30", color = "white", lwd = .25, boundary = 0) +
    geom_vline(xintercept = cutoff, color = "red", linetype = "11") +
    scale_x_continuous(breaks = seq(0, 1, 0.05)) +
    labs(
      subtitle = "Histogram of similarity scores, all intersecting polygons",
      caption = paste0("Cutoff = ", cutoff, ". Bin width = ", bin)
    ) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  }

  if (top.only == TRUE) {
    summary[['histogram']] <- df |>
      dplyr::filter(.data$rank_areal_f == 1 & .data$rank_areal_b == 1 & .data$s >= cutoff) |>
      ggplot(aes(s)) +
      geom_histogram(binwidth = bin, fill = "grey30", color = "white", lwd = .25, boundary = 0) +
      geom_vline(xintercept = cutoff, color = "red", linetype = "11") +
      scale_x_continuous(breaks = seq(0, 1, 0.05)) +
      labs(
        subtitle = "Histogram of similarity scores, top-ranked ",
        caption = paste0("Cutoff = ", cutoff, ". Bin width = ", bin)
      ) +
      theme_bw() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  }

    message(paste0("Calculating proportion of geom1 polygons with a match with s >= ", cutoff))
    summary[['matched_cutoff1']] <- df |>
      group_by(id1) |>
        summarise(s = max(s)) |>
      ungroup() |>
      summarise(pct = mean(s >= cutoff))

    message(paste0("Calculating proportion of geom2 polygons with a match with s >= ", cutoff))
    summary[['matched_cutoff2']] <- df |>
      group_by(id2) |>
        summarise(s = max(s)) |>
      ungroup() |>
      summarise(pct = mean(s >= cutoff))

    message(paste0("Calculating proportion of geom1 polygons with top-ranked match."))
    summary[['matched_toprank1']] <- df |>
      mutate(i = ifelse(.data$rank_areal_f == 1 & .data$rank_areal_b == 1 & .data$s >= cutoff,
                        TRUE,
                        FALSE)) |>
      group_by(id1) |>
        summarise(i = max(i)) |>
      ungroup() |>
      summarise(pct = mean(i))

    message(paste0("Calculating proportion of geom2 polygons with top-ranked match."))
    summary[['matched_toprank2']] <- df |>
      mutate(i = ifelse(.data$rank_areal_f == 1 & .data$rank_areal_b == 1 & .data$s >= cutoff,
                        TRUE,
                        FALSE)) |>
      group_by(id2) |>
      summarise(i = max(i)) |>
      ungroup() |>
      summarise(pct = mean(i))

}

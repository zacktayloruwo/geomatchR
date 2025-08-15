# Function: gm_crossmatch

# Calculates similarity scores and other diagnostics
# Returns a list of tables: 
# similarity = match scores for all overlapping polygons
# top_ranked = top-ranked matches, whereby rank_areal_f and rank_areal_b equal 1 and the similarity score > provided cutoff threshold
# no_match1 = if not top-ranked, all matches for polygons in the first set
# no_match2 = if not top-ranked, all matches for polygons in the second set

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
  
  library(dplyr)
  library(sf)
  library(units)
  library(rio)
  `%notin%` <- Negate(`%in%`)
  
  #test
  # geom1 <- geoms_clean[['ct_1981']]
  # geom2 <- geoms_clean[['ct_2006']]
  # id1 = id2 = "geosid"
  # name1 = name2 = NA
  # clean = FALSE

  # clean files if necessary
  if (clean == TRUE) {
    
    message("** Cleaning ", deparse(substitute(geom1)))
    gm_clean(geom1)
    message("** Cleaning ", deparse(substitute(geom2)))
    gm_clean(geom2)
    
  }
  
  # rename id and name columns
  geom1 <- geom1 |> rename(id1 = !!sym(id1))
  geom2 <- geom2 |> rename(id2 = !!sym(id2))
  
  if (!is.na(name1)) { geom1 <- geom1 |> rename(name = !!sym(name1))}
  if (is.na(name1)) { geom1 <- geom1 |> mutate(name = NA) }
  
  if (!is.na(name2)) { geom2 <- geom2 |> rename(name = !!sym(name2))}
  if (is.na(name2)) { geom2 <- geom2 |> mutate(name = NA) }
  
  # calculate areas
  message("Calculating areas.")
  geom1$area1 <- sf::st_area(geom1)
  geom2$area2 <- sf::st_area(geom2)
  
  # calculate maximum widths
  message("calculating maximum widths.")
  geom1 <- geom1 |> 
    rowwise() |>
      mutate(width1 = {
          mrr <- st_minimum_rotated_rectangle(geometry)
          coords <- st_coordinates(mrr)[, 1:2]  # x and y only
          dists <- as.matrix(dist(coords))
          max(dists)}) |>
      ungroup()
  
  geom2 <- geom2 |> 
    rowwise() |>
    mutate(width2 = {
      mrr <- st_minimum_rotated_rectangle(geometry)
      coords <- st_coordinates(mrr)[, 1:2]  # x and y only
      dists <- as.matrix(dist(coords))
      max(dists)}) |>
    ungroup()

  message("Areal units: ", units:::units.units(geom1$area1))
  
  # drop geometry and unit formatting and express areas in sqkm
  area1 <- units::drop_units(sf::st_drop_geometry(geom1))  
  area1$area1 = area1$area1 / 1000000
  
  area2 <- units::drop_units(sf::st_drop_geometry(geom2))
  area2$area2 = area2$area2 / 1000000

  # Intersect geom1 and geom2 polygons 
  message("Intersecting polygons.")
  
  ## suppresses warning during st_intersection()
  st_agr(geom1) = "constant"
  st_agr(geom2) = "constant"
  
  intersection <- sf::st_intersection(geom1, geom2) 
  
  # calculate area of overlap between all geom1 and geom2 polygons   
  intersection$overlap = sf::st_area(intersection)
  # drop geometry and unit formatting and express areas in sqkm
  intarea <- sf::st_drop_geometry(intersection) |> units::drop_units()
  intarea$overlap = intarea$overlap / 1000000
  intarea <- intarea |> select(id1, id2, overlap)
  
  message("Calculating areal overlap.")
  # Calculate % overlap going forward and backward
  forward <- area2 |>
    # join areas of intersections to areas of geom2 polygons on the geom2 id
    left_join(intarea, by = "id2", multiple = "all") |>
    # calculate forward areal overlap
    mutate(pct_f = round(overlap / area2, 4)) |>
    rename(overlap_f = overlap)
  
  if (max(forward$pct_f, na.rm = TRUE) > 1) {
    message("Forward overlap is greater than 1. Max = ", max(forward$pct_f, na.rm = TRUE), ". Setting to 1.")
    # if pct overlap > 100% due to area calculation error, then set to 1
    forward <- forward |> mutate(pct_f = ifelse(pct_f > 1, 1, pct_f)) 
  }

  backward <- area1 |>
    # join areas of intersections to areas of geom1 polygons on the geom1 id
    left_join(intarea, by = "id1", multiple = "all") |>
    # calculate backward areal overlap
    mutate(pct_b = round(overlap / area1, 4)) |>
    rename(overlap_b = overlap) 
  
  if (max(backward$pct_b, na.rm = TRUE) > 1) {
    message("Backward overlap is greater than 1. Max = ", max(backward$pct_b, na.rm = TRUE), ". Setting to 1.")
    # if pct overlap > 100% due to area calculation error, then set to 1
    backward <- backward |> mutate(pct_b = ifelse(pct_b > 1, 1, pct_b)) 
  }
  
  similarity <- list()
  
  # Compare forward and backward % overlap and generate areal similarity score
  message("Calculating similarity scores based on areal overlap.")
  
  vars <- c("id1", "name1", "id2", "name2", "similarity_area", "pct_f", "pct_b",
            "gap_f", "gap_b", "gap_areal_diff", 
            "rank_areal_f", "rank_areal_b", 
            "width1", "width2", "gap_width"
  )
  
  similarity[['similarity']] <- full_join(forward, backward, by = c("id1", "id2"), multiple = "all") |>
    relocate(id1) |>
    # drop if pct = NA, meaning there is no overlap
    filter(!is.na(pct_f)) |>
    filter(!is.na(pct_b)) |>
    # calculate difference from mean percentage overlap: small is good
    mutate(pct_diff = abs(pct_f - pct_b) / 2) |>
    # maximize product of % overlap while minimizing difference of %s from mean of %s
    mutate(similarity_area = (pct_f * pct_b) * (1 - pct_diff)) |>
    mutate(similarity_area = round(similarity_area, 4)) |>
    # rank y2 scores in descending order within each y1 polygon 
    group_by(id1) |>
      arrange(id1, desc(similarity_area)) |>
      mutate(rank_areal_f = row_number()) |>
      # calculate gap between first and second score in group
      # if only one row in group, gap is maximum (= 1)
      mutate(gap_f = first(similarity_area) - nth(similarity_area, 2)) |>
      mutate(gap_f = ifelse(is.na(gap_f), 1, gap_f)) |>
    ungroup() |>
    # rank y1 scores in descending order within each y2 polygon 
    group_by(id2) |>
      arrange(id2, desc(similarity_area)) |>
      mutate(rank_areal_b = row_number()) |>
      # calculate gap between first and second score in group
      # if only one row in group, gap is maximum (= 1)
      mutate(gap_b = first(similarity_area) - nth(similarity_area, 2)) |>
      mutate(gap_b = ifelse(is.na(gap_b), 1, gap_b)) |>
    ungroup() |>
    mutate(gap_areal_diff = abs(gap_f - gap_b)) |>
    # calculate width gap
    mutate(gap_width = abs(width1 - width2)) |>
    # sort table by descending y2 score within each y1 polygon
    arrange(id1, desc(similarity_area)) |>
    select(any_of(vars))

  message("Calculating distances between centroids.")
  # calculate distances between centroids
  dist_matrix <- st_distance(st_centroid(geom1), st_centroid(geom2))
  rownames(dist_matrix) <- geom1$id1
  colnames(dist_matrix) <- geom2$id2
  
  dist_matrix <- dist_matrix |>
    as.data.frame() |>
    rownames_to_column(var = "id1") |>
    pivot_longer(-id1, names_to = "id2", values_to = "distance") |>
    group_by(id1) |>
    arrange(id1, distance) |>
    mutate(rank_dist_f = row_number()) |>
    ungroup() |>
    group_by(id2) |>
    arrange(id2, distance) |>
    mutate(rank_dist_b = row_number()) |>
    ungroup()
  
  similarity[['similarity']] <- similarity[['similarity']] |>
    left_join(dist_matrix, by = c("id1", "id2")) |>
    mutate(dist_ratio1 = distance / width1) |>
    mutate(dist_ratio2 = distance / width2)

  message("Identifying top-ranked matches.")
  # top-ranked matches
  similarity[['top_ranked']] <- similarity[['similarity']] |>
    filter(rank_areal_f == 1 & rank_areal_b == 1 & similarity_area >= cutoff)
  
  message("Identifying polygons with no good matches.")
  # Identify polygons with no good matches
  keep <- area1 |> filter(id1 %notin% similarity[['top_ranked']][['id1']]) |> pull(id1)
  similarity[['no_match1']] <- similarity[['similarity']] |> filter(id1 %in% keep)
  
  keep <- area2 |> filter(id2 %notin% similarity[['top_ranked']][['id2']]) |> pull(id2)
  similarity[['no_match2']] <- similarity[['similarity']] |> filter(id2 %in% keep)
  
  return(similarity)
  
}


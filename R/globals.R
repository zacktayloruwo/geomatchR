# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(
  ".data",
  "id", "id1", "id2", "name", "name1", "name2",
  "overlap", "overlap_f", "overlap_b",
  "area1", "area2",
  "pct_f", "pct_b", "pct_diff",
  "similarity_area", "rank_areal_f", "rank_areal_b",
  "gap_f", "gap_b", "gap_areal_diff",
  "width1", "width2", "gap_width",
  "distance", "rank_dist_f", "rank_dist_b",
  "geometry"
))

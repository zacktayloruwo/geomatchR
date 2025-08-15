# Function: gm_map_similarity

# Map superimposes polygon sets with alpha with shading indicating similarity score.

gm_map_similarity <- function(geom1, geom2, id1, id2, name1, name2, table, 
                              label.cutoff = .8, labels = "none", 
                              plot = "combined") {
  
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(ggrepel)
  library(viridis)
  `%notin%` <- Negate(`%in%`)

  #test
  # geom1 <- geoms_clean[['csd_1981']]
  # geom2 <- geoms_clean[['csd_1996']]
  # id1 = id2 = "geosid"
  # name1 = name2 = "geoname"
  # table = csd1981_csd1996
  # label = "id"
  # label.cutoff = .8
  # plot = "combined"
  
  lwd = .25
  a = .8
  s = 2
  f = 75

  if (plot %in% c("forward", "f")) {
    
    g <- geom1 |> 
      rename(id = !!sym(id1)) |>
      left_join(table[['similarity']] |> 
                  filter(rank_areal_f == 1) |> 
                  select(id1, similarity_area), 
                by = join_by(id == id1))
    
    # if name does not exist
    if (!is.na(name1)) { g <- g |> rename(name = !!sym(name1))}
    if (is.na(name1)) { g <- g |> mutate(name = id) }

    ## suppresses warning during st_centroid()
    st_agr(g) = "constant"

    lim = min(g$similarity_area)
    
    p <- ggplot() + 
      geom_sf(data = g, aes(fill = similarity_area), colour = "#000000", lwd = lwd) + 
      viridis::scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      labs(title = "Similarity scores, forward matching", 
           x = "", y = "") +
      theme_bw()
    
    #ggtitle(paste("(b) geomatchR backward match score\n", geo, y2, sprintf('\u2192'), y1, sep = " "))
    
    
    if (labels %in% c("id", "name")) {
      
      p <- p + ggrepel::geom_label_repel(aes(label = paste0(!!sym(labels), "\n", round(similarity_area,2)), geometry = geometry), 
                                         size = s, color = "red", stat = "sf_coordinates", 
                                         min.segment.length = 0, segment.color = "red",
                                         max.overlaps = Inf, alpha = a, seed = 519, force = f, 
                                         data = st_centroid(g |> filter(similarity_area < label.cutoff))) +
        labs(caption = paste0("Label display cut-off: < ", label.cutoff))
    }
    
    return(p)

  }
  
  if (plot %in% c("backward", "b")) {
    
    g <- geom2 |> 
      rename(id = !!sym(id2)) |>
      left_join(table[['similarity']] |> 
                  filter(rank_areal_b == 1) |> 
                  select(id2, similarity_area), 
                by = join_by(id == id2))
    
    # if name does not exist
    if (!is.na(name2)) { g <- g |> rename(name = !!sym(name2))}
    if (is.na(name2)) { g <- g |> mutate(name = id) }
    
    ## suppresses warning during st_centroid()
    st_agr(g) = "constant"
    
    lim = min(g$similarity_area)
    
    p <- ggplot() + 
      geom_sf(data = g, aes(fill = similarity_area), colour = "#000000", lwd = lwd) + 
      viridis::scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      labs(title = "Similarity scores, backward matching", 
           x = "", y = "") +      
      theme_bw()
    
    if (labels %in% c("id", "name")) {
      
      p <- p + ggrepel::geom_label_repel(aes(label = paste0(!!sym(labels), "\n", round(similarity_area,2)), geometry = geometry), 
                                         size = s, color = "red", stat = "sf_coordinates", 
                                         min.segment.length = 0, segment.color = "red",
                                         max.overlaps = Inf, alpha = a, seed = 519, force = f, 
                                         data = st_centroid(g |> filter(similarity_area < label.cutoff))) +
        labs(caption = paste0("Label display cut-off: < ", label.cutoff))
    }
    
    return(p)
    
  }
  
  if (plot %in% c("combined", "c")) {
    
    geom_1 <- geom1 |> rename(id = !!sym(id1)) |> left_join(table[['similarity']] |> filter(rank_areal_f == 1) |> select(id1, similarity_area), by = join_by(id == id1))
    geom_2 <- geom2 |> rename(id = !!sym(id2)) |> left_join(table[['similarity']] |> filter(rank_areal_b == 1) |> select(id2, similarity_area), by = join_by(id == id2))
    
    # if name does not exist
    if (!is.na(name1)) { geom_1 <- geom_1 |> rename(name = !!sym(name1))}
    if (is.na(name1)) { geom_1 <- geom_1 |> mutate(name = id) }
    
    if (!is.na(name2)) { geom_2 <- geom_2 |> rename(name = !!sym(name2))}
    if (is.na(name2)) { geom_2 <- geom_2 |> mutate(name = id) }
    
    ## suppresses warning during st_centroid()
    st_agr(geom_1) = "constant"
    st_agr(geom_2) = "constant"
    
    lim = min(c(min(geom_1$similarity_area), min(geom_2$similarity_area)))
    
    p1 <- ggplot() + 
      geom_sf(data = geom_2, fill = NA, color = NA) + 
      geom_sf(data = geom_1, aes(fill = similarity_area), colour = "grey50", lwd = lwd) + 
      scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      labs(title = "Similarity scores, forward matching", x = "", y = "") +
      theme_bw()
    
    p2 <- ggplot() + 
      geom_sf(data = geom_1, fill = NA, color = NA) + 
      geom_sf(data = geom_2, aes(fill = similarity_area), colour = "grey50", lwd = lwd) + 
      scale_fill_viridis(direction = -1, limits = c(lim,1))  +
      labs(title = "Similarity scores, backward matching", x = "", y = "") +
      theme_bw()
    
    if (labels %in% c("id", "name")) {
      
      p1 <- p1 + ggrepel::geom_label_repel(aes(label = paste0(!!sym(labels), "\n", round(similarity_area,2)), geometry = geometry), 
                                           size = s, color = "red", stat = "sf_coordinates", 
                                           min.segment.length = 0, segment.color = "red",
                                           max.overlaps = Inf, alpha = a, seed = 519, force = f, 
                                           data = st_centroid(geom_1 |> filter(similarity_area < label.cutoff))) +
        labs(caption = paste0("Label display cut-off: < ", label.cutoff))
      
      p2 <- p2 + ggrepel::geom_label_repel(aes(label = paste0(!!sym(labels), "\n", round(similarity_area,2)), geometry = geometry), 
                                         size = s, color = "red", stat = "sf_coordinates", 
                                         min.segment.length = 0, segment.color = "red",
                                         max.overlaps = Inf, alpha = a, seed = 519, force = f, 
                                         data = st_centroid(geom_2 |> filter(similarity_area < label.cutoff))) +
        labs(caption = paste0("Label display cut-off: < ", label.cutoff))
    }    
    
    p <- ggpubr::ggarrange(p1, p2, ncol = 1, legend = "bottom", common.legend = TRUE, align ="hv")
    return(p)
    
  }
  
  if (plot %notin% c("combined", "forward", "backward", "c", "f", "b")) {
    stop("Plot not correctly specified")
  }
  
}
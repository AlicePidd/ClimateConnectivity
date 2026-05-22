# Plotting graph theory metrics and residence time geographically
  # Written by Alice Pidd
        # Jan/Feb 2026


# Helpers ----------------------------------------------------------------------
 
  source("Helpers.R")
  metric <- "VoCCtracers"

  
  

# Folders ----------------------------------------------------------------------
  
  sum_fol <- make_folder(disk, metric, "4_pairs_mpa_summed")
  GT_node_fol <- make_folder(disk, metric, "9_graph_theory_calc/node")
  GT_edge_fol <- make_folder(disk, metric, "9_graph_theory_calc/edge")
  GT_network_fol <- make_folder(disk, metric, "9_graph_theory_calc/network")
  
  restime_fol <- make_folder(disk, metric, "7_restime_esm")
  GT_plot_allnet_fol <- make_folder(disk, metric, "10_graph_theory_plot/geoplot/allnetworks")
  GT_plot_GBR_fol <- make_folder(disk, metric, "10_graph_theory_plot/geoplot/GBR")
  
  
  
  
# Get centroids of MPA_IDs -----------------------------------------------------
  
  cent <- mpa_shp %>% 
    st_centroid() %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2],
      lon_cat = round(lon),
      lat_cat = round(lat),
      MPA_ID = as.character(MPA_ID)) %>%
    st_drop_geometry()
  
  head(cent)
  unique(cent$MPA_ID)

  
  
  
# Get residence time data for colouring the dots -------------------------------
  
  res_dat <- readRDS(paste0(restime_fol, "/med_restime_MPA-ID_per_ssp-term-combo.RDS")) # Data from the calc script
  head(res_dat)
  

  
  
# Get data ---------------------------------------------------------------------
  
  node_files <- dir(GT_node_fol, full.names = TRUE) %>% 
    str_subset(., "invweighted") # Unnormalised, only inversely weighted
  node_files
  edge_files <- dir(GT_edge_fol, full.names = TRUE)
  edge_files
  
  
  
  ## Get 'global' ranges ---------
  
    all_nodes <- map_dfr(node_files, readRDS) # Read in all the node files
    betweenness_range <- range(all_nodes$betweenness_inverse_weighted, na.rm = TRUE)
    betweenness_range # range: 0-42487 (normalised was 0.00-0.4322969)
    restime_range <- range(res_dat$med_restime, na.rm = TRUE)
    restime_range # range: 1-240
  
    

  ## Get max number of total_exchanges --------   
    
    max_exch <- map_dfr(edge_files, ~{
        d <- readRDS(.x)
        tibble(max_val = max(d$total_exchange))
      })
    max(max_exch) # 13,153,361 max total_exchange, crazy!
    
    
      
  ## Get proportion of total_exchanges that are over a certain threshold --------   
    
      prop_over_max_exch <- map_dfr(edge_files, ~{
        d <- readRDS(.x)
        tibble(total_rows = nrow(d),
               rows_over = d %>% filter(total_exchange >= 40000) %>% 
                 nrow(),
               prop_over = rows_over/total_rows)
      })
      round(range(prop_over_max_exch$prop_over * 100), digits = 2)
        #  6.42-7.54% of edges have a total_exchange >= 30,000
          # I have set the truncation to this
      
      

  
# Get summary stats for quoting network analysis stuff in text -----------------
  
  get_ssp <- function(filepath) {
    str_extract(basename(filepath), "ssp[0-9]+")
  }
  
  ## All networks ------------------
    ### Betweenness --------
      mid_node_files <- node_files %>% str_subset(., "mid")
      top_betweenness <- map_dfr(mid_node_files, function(f) {
        readRDS(f) %>%
          mutate(ssp = get_ssp(f),
                 term = "mid") %>%
          slice_max(betweenness_inverse_weighted, n = 10) %>%
          select(ssp, term, MPA_ID, betweenness_inverse_weighted)
      }) %>%
        arrange(ssp, desc(betweenness_inverse_weighted))
      top_betweenness

    ### Get max log10 betweenness across all files for node size breaks ------------
      global_max_log_betweenness <- node_files %>%
        map_dbl(~ readRDS(.x) %>% 
                  mutate(log_betweenness = log10(betweenness_inverse_weighted + 1)) %>%
                  pull(log_betweenness) %>% 
                  max(na.rm = TRUE)) %>%
        max()
      global_max_log_betweenness # 4.628266

    ### Edge strength --------
      mid_edge_files <- edge_files %>% str_subset(., "mid")
      top_edgestrength <- map_dfr(mid_edge_files, function(f) {
        readRDS(f) %>%
          filter(mpa_1 != mpa_2) %>%
          mutate(ssp = get_ssp(f),
                 term = "mid") %>%
          slice_max(total_exchange, n = 10) %>%
          select(ssp, term, mpa_1, mpa_2, total_exchange)
      }) %>%
        arrange(ssp, desc(total_exchange))
      top_edgestrength %>% print(n = 40)

    ### Residence time --------
      top_restime <- res_dat %>%
        filter(term == "mid-term") %>%
        group_by(ssp) %>%
        slice_max(med_restime, n = 10) %>%
        select(ssp,term, MPA_ID, med_restime, NETNAME, recalc_AREA_KM2) %>%
        arrange(ssp, desc(med_restime))
      top_restime %>% print(n = 40)

    ### Residence time top 10 per network --------
      top_net_restime <- res_dat %>%
        filter(term == "mid-term") %>%
        group_by(ssp, NETNAME) %>%
        slice_max(med_restime, n = 10) %>%
        select(ssp,term, MPA_ID, med_restime, NETNAME, recalc_AREA_KM2) %>%
        arrange(ssp, desc(med_restime))
      top_net_restime %>% print(n = 2000)
      

  ## GBR only ------------------
    ### Betweenness --------
      mid_node_files <- node_files #%>% str_subset(., "mid")
      top_betweenness_GBR <- map_dfr(mid_node_files, function(f) {
        readRDS(f) %>%
          filter(MPA_ID %in% network_mpas$GBR) %>% 
          mutate(ssp = get_ssp(f),
                 term = "mid") %>%
          slice_max(betweenness_inverse_weighted, n = 10) %>%
          select(ssp, term, MPA_ID, betweenness_inverse_weighted)
      }) %>%
        arrange(ssp, desc(betweenness_inverse_weighted))
      top_betweenness_GBR

    ### Get GBR max log10 betweenness across all files for node size breaks ------------
      global_max_log_betweenness_GBR <- node_files %>%
        map_dbl(~ readRDS(.x) %>% 
                  filter(MPA_ID %in% network_mpas$GBR) %>%
                  mutate(log_betweenness = log10(betweenness_inverse_weighted + 1)) %>%
                  pull(log_betweenness) %>% 
                  max(na.rm = TRUE)) %>%
        max()
      global_max_log_betweenness_GBR # 4.29752

    ### Edge strength --------
      mid_edge_files <- edge_files #%>% str_subset(., "mid")
      top_edgestrength_GBR <- map_dfr(mid_edge_files, function(f) {
        readRDS(f) %>%
          filter(mpa_1 %in% network_mpas$GBR & mpa_2 %in% network_mpas$GBR) %>% 
          filter(mpa_1 != mpa_2) %>%
          mutate(ssp = get_ssp(f),
                 term = "mid") %>%
                 # term = term) %>%
          slice_max(total_exchange, n = 10) %>%
          select(ssp, term, mpa_1, mpa_2, total_exchange)
      }) %>%
        arrange(ssp, desc(total_exchange))
      top_edgestrength_GBR %>% print(n = 200)

    ### Residence time --------
      top_restime_GBR <- res_dat %>%
        filter(term == "mid-term") %>%
        filter(MPA_ID %in% network_mpas$GBR) %>% 
        group_by(ssp) %>%
        slice_max(med_restime, n = 10) %>%
        select(ssp,term, MPA_ID, med_restime, NETNAME, recalc_AREA_KM2) %>%
        arrange(ssp, desc(med_restime))
      top_restime_GBR %>% print(n = 40)
  
  
  
  
# Plot network metrics geogrpahicaly with MPAs as points and geom_curves -------

  geoplot_GT_metrics <- function(node_file, 
                                 edge_file, 
                                 top_n_labels,
                                 edge_percentile,
                                 edge_trunc,
                                 betweenness_transform,
                                 GBR) {
    
    # Get ssp and term from filename
      ssp <- basename(node_file) %>% str_split_i("_", 4)
      term <- basename(node_file) %>% str_split_i("_", 5) %>% str_remove(".RDS")

    if(GBR == TRUE) {
      message(paste0("🕒 Plotting ", ssp, " ", term, " for the GBR ..."))
      
      edge_df <- readRDS(edge_file) %>% filter(mpa_1 %in% network_mpas$GBR & mpa_2 %in% network_mpas$GBR) %>% 
        filter(mpa_1 != mpa_2)  # Remove self-loops FIRST
      
      res <- res_dat %>% 
        filter(ssp == .env$ssp & term == .env$term) %>% 
        filter(MPA_ID %in% network_mpas$GBR)
      
      node_df <- readRDS(node_file) %>% 
        mutate(MPA_ID = as.numeric(MPA_ID)) %>%
        left_join(res %>% dplyr::select(MPA_ID, med_restime, lon, lat, NETNAME), by = "MPA_ID") %>% 
        filter(MPA_ID %in% network_mpas$GBR)
      
      # If betweenness_transform is "log" do x or "quant" do y
      if(betweenness_transform == "log"){
        node_df <- node_df %>% 
          mutate(log10_betweenness = log10(betweenness_inverse_weighted + 1),
                 plot_betweenness = log10_betweenness)
        brks <- seq(0, ceiling(global_max_log_betweenness_GBR), by = 1)
      } else {
        node_df$plot_betweenness <- node_df$betweenness_inverse_weighted
        brks <- pretty(range(node_df$plot_betweenness), n = 4)
      }

      bbox <- mpa_shp %>% filter(NETNAME == "GBR") %>% 
        st_bbox()
      
      # Dynamic buffer based on network size
      x_range <- bbox["xmax"] - bbox["xmin"]
      y_range <- bbox["ymax"] - bbox["ymin"]
      buffer_x <- max(x_range * 0.1, 0.5)
      buffer_y <- max(y_range * 0.1, 0.5)
      
      xlims <- c(bbox["xmin"] - buffer_x, bbox["xmax"] + buffer_x)
      ylims <- c(bbox["ymin"] - buffer_y, bbox["ymax"] + buffer_y)
      mpa_shapefile <- mpa_shp %>% filter(NETNAME == "GBR")
      
    } else {
      message(paste0("🕒 Plotting ", ssp, " ", term, " ..."))
      edge_df <- readRDS(edge_file) %>% 
        filter(mpa_1 != mpa_2)  # Remove self-loops FIRST
      
      node_df <- readRDS(node_file) %>% 
        mutate(MPA_ID = as.numeric(MPA_ID)) %>%
        left_join(res %>% dplyr::select(MPA_ID, med_restime, lon, lat, NETNAME), by = "MPA_ID")
      
      res <- res_dat %>% 
        filter(ssp == .env$ssp & term == .env$term)
      
      bbox <- mpa_shp %>% 
        st_bbox()
      x_range <- bbox["xmax"] - bbox["xmin"]
      y_range <- bbox["ymax"] - bbox["ymin"]
      buffer_x <- max(x_range * 0.1, 0.5)
      buffer_y <- max(y_range * 0.1, 0.5)
      
      xlims <- c(bbox["xmin"] - buffer_x, bbox["xmax"] + buffer_x)
      ylims <- c(bbox["ymin"] - buffer_y, bbox["ymax"] + buffer_y)
      
      mpa_shapefile <- mpa_shp
    }

    # If betweenness_transform is "log" do x or "quant" do y
      if(betweenness_transform == "log"){
        node_df <- node_df %>% 
          mutate(log10_betweenness = log10(betweenness_inverse_weighted + 1),
                 plot_betweenness = log10_betweenness)
        # brks <- seq(0, round(max(node_df$plot_betweenness), digits = 2), by = 0.02)
        brks <- pretty(c(0, global_max_log_betweenness), n = 7)
      } else {
        node_df$plot_betweenness <- node_df$betweenness_inverse_weighted
        brks <- pretty(range(node_df$plot_betweenness), n = 7)
      }

    # Calc threshold on remaining edges
      threshold <- quantile(edge_df$total_exchange, edge_percentile)
      
      edge_df <- edge_df %>%
        mutate(edge_category = if_else(total_exchange >= threshold, "top", "other")) %>%
        mutate(mpa_1 = as.character(mpa_1),
               mpa_2 = as.character(mpa_2)) %>%
        left_join(cent %>% select(MPA_ID, lon, lat), by = c("mpa_1" = "MPA_ID")) %>%
        rename(lon_1 = lon, lat_1 = lat) %>%
        left_join(cent %>% select(MPA_ID, lon, lat), by = c("mpa_2" = "MPA_ID")) %>%
        rename(lon_2 = lon, lat_2 = lat) %>%
        filter(!is.na(lon_1), !is.na(lon_2))
      
      top_edges <- edge_df %>% 
        filter(edge_category == "top") %>% 
        mutate(total_exchange = ifelse(total_exchange >= edge_trunc, edge_trunc, total_exchange), # Truncate the total_exchange of edges to 40,000
               edge_alpha = ifelse(mpa_1 == "29" | mpa_2 == "29", 0.3, 0.9)) # Making MPA 29 alpha lower
    
    
    # Top betweenness nodes for labelling
      top_nodes_gbr <- node_df %>%
        filter(NETNAME == "GBR") %>%
        group_by(NETNAME) %>%
        slice_max(plot_betweenness, n = top_n_labels)
      
      top_nodes_east <- node_df %>%
        filter(NETNAME == "Temperate East" | NETNAME == "North" | NETNAME == "Coral Sea" | NETNAME == "South-east"
        ) %>%
        group_by(NETNAME) %>% 
        slice_max(plot_betweenness, n = 2)
      
      top_nodes_west <- node_df %>%
        filter(NETNAME == "South-west" | NETNAME == "North-west"
               ) %>%
        group_by(NETNAME) %>% 
        slice_max(plot_betweenness, n = 2)
      
      # geom_curve aesthetics
      other_edge_colour <- if (GBR == FALSE) "grey20" else if (highlight) "grey20" else "grey30"
      other_edge_alpha  <- if (GBR == FALSE) 0.7     else if (highlight) 0.6     else 0.7
      other_edge_lwd    <- if (GBR == FALSE) 0.3     else if (highlight) 0.3     else 0.15
      
      # For conditional partitioning below in plot
      # highlight <- GBR == TRUE &  ssp == "ssp126" & term == "mid-term"
      highlight <- GBR == TRUE & term == "near-term"

      # Base geoms for either GBR plots or whole network
      base_geoms <- if (highlight) { # Only plot the aus outline for highlight cases
        list(geom_sf(data = mpa_shapefile, fill = "#C6BDA2", alpha = 0.7, lwd = 0.05, colour = "white"), # #CBC6B7
             geom_sf(data = aus_detailed_shp, fill = "#404F4B", colour = NA))
        } else if (GBR == TRUE) { # GBR TRUE but not highlight
          list(geom_sf(data = mpa_shapefile, fill = "#C6BDA2", alpha = 0.7, lwd = 0.05, colour = "white")) # #CBC6B7
          } else { # GBR FALSE
            list(
              geom_sf(data = aus_detailed_shp, fill = "#404F4B", colour = NA),
              geom_sf(data = mpa_shapefile, fill = "#C6BDA2", alpha = 0.7, lwd = 0.05, colour = "white"), # #CBC6B7
              geom_sf(data = eez_shp, fill = NA, color = "black", lwd = 0.25))
            }
      
      p <- ggplot() +
        base_geoms + # Paste base geoms onto everything else
        geom_curve(data = edge_df %>% filter(edge_category == "other"),
                   aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2),
                   colour = other_edge_colour,
                   alpha  = other_edge_alpha,
                   lwd    = other_edge_lwd,
                   curvature = 0.2) +
        # Nodes
        geom_point(data = node_df,
                   aes(x = lon, y = lat,
                       size = plot_betweenness,
                       fill = med_restime),
                   shape = 21,
                   colour = "black",
                   stroke = 0.15,
                   alpha = 0.95) +
        # Top edges
        geom_curve(data = top_edges,
                   aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2,
                       lwd = total_exchange),
                   colour = "#ee9b00",
                   alpha = top_edges$edge_alpha,
                   curvature = 0.2) +
        # Labels for GBR only
        geom_text_repel(data = top_nodes_gbr,
                        aes(x = lon, y = lat, label = MPA_ID),
                        seed = 123,
                        force = 1,
                        direction = "both",
                        nudge_y = if(GBR == TRUE) 0.5 else 1.5,
                        nudge_x = if(GBR == TRUE) 0.5 else 1,
                        point.padding = unit(0.5, "lines"), 
                        size = 3,
                        max.overlaps = 20,
                        colour = "black") +
        # Labels - east coast
        geom_text_repel(data = top_nodes_east,
                        aes(x = lon, y = lat, label = MPA_ID),
                        seed = 15,
                        force = 1,
                        direction = "y",
                        nudge_y = 1.5,
                        nudge_x = 1,
                        point.padding = unit(0.5, "lines"), 
                        size = 3,
                        max.overlaps = 20,
                        colour = "black") +
        # Labels - west coast
        geom_text_repel(data = top_nodes_west,
                        aes(x = lon, y = lat, label = MPA_ID),
                        seed = 15,
                        force = 1,
                        direction = "y",
                        nudge_y = 1.5,
                        nudge_x = -0.3,
                        point.padding = unit(0.5, "lines"), 
                        size = 3,
                        max.overlaps = 20,
                        colour = "black") +
        # Scales
        coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +  # Does the clipping
        scale_linewidth_continuous(range = c(0.5, 4), 
                                   breaks = seq(round(1, digits = -3), # From 0, rounded to nearest 1000
                                                round(edge_trunc, digits = -3), # Truncated
                                                if(edge_trunc == 40000) 10000 else 5000), # By 10000 or 5000 at a time
                                   limits = c(1, edge_trunc), 
                                   name = "Edge strength") +
        # Scales with fixed limits
        scale_size_continuous(range = c(1.3, 5), 
                              # limits = range(node_df$plot_betweenness),
                              limits = range(brks),
                              breaks = brks,
                              name = if(betweenness_transform == "log"){
                                "Betweenness (log10)"
                              } else if(betweenness_transform == "norm"){
                                "Betweenness"}
                                ) +
        scale_fill_gradientn(colours = rev(restime_geoplot_pal),
                             limits = restime_range,
                             name = "Residence time\n(months)") +
        theme_void() +
        labs(title = paste0("MPA connectivity under ", ssp, " (", term, ")"),
             subtitle = paste0(
               "Edges (orange): ", edge_percentile * 100, "th percentile (top ", 100 - (edge_percentile * 100), 
               "% of pairwise edge strengths)\n", "Nodes sized by: ", switch(betweenness_transform,
                                                                             log   = "log10(betweenness + 1)",
                                                                             " inversely weighted betweenness") # default
               )
             )
    
    # Save it conditionally
      file_suffix <- paste0(if (highlight) "_LAND" else "", "_", betweenness_transform)
      
      if (GBR == TRUE) {
        o_nm <- paste0(GT_plot_GBR_fol, "/geoplot_GT_", ssp, "_", term, "_GBR_", edge_percentile * 100, "th-pct", file_suffix, "_invweighted-betweenness.pdf")
        ggsave(o_nm, plot = p, width = 14, height = 10, dpi = 300)
        message(paste0("✅ ", ssp, " ", term, " for the GBR done!", file_suffix))
      } else {
        o_nm <- paste0(GT_plot_allnet_fol, "/geoplot_GT_", ssp, "_", term, "_", edge_percentile * 100, "th-pct", file_suffix, "_invweighted-betweenness.pdf")
        ggsave(o_nm, plot = p, width = 14, height = 10, dpi = 300)
        message(paste0("✅ ", ssp, " ", term, " done!", file_suffix))
      }
  }
  
  
  # All networks
    walk2(node_files, edge_files, ~geoplot_GT_metrics(
      node_file = .x,
      edge_file = .y,
      top_n_labels = 10,
      edge_percentile = 0.75,
      edge_trunc = 40000,
      betweenness_transform = "log",
      # betweenness_transform = "norm",
      GBR = FALSE)) 

  # Just GBR
    walk2(node_files, edge_files, ~geoplot_GT_metrics(
      node_file = .x,
      edge_file = .y,
      top_n_labels = 10,
      edge_percentile = 0.75, 
      edge_trunc = 40000,
      betweenness_transform = "log",
      # betweenness_transform = "norm",
      GBR = TRUE))

    

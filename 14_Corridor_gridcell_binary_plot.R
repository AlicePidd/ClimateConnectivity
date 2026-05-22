# Binarising aggregated grid strength (by SSP) and plotting as selection frequency of well-worn corridors (through MPAs) of trajectory sequences for each SSP (terms combined)
    # Written by Alice Pidd
        # Jan 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"

  

  
# Folders ----------------------------------------------------------------------
  
  strength_agg_fol <- make_folder(disk, metric, "12_gridcell_strength_aggregated")
  strength_bin_fol <- make_folder(disk, metric, "13_gridcell_strength_binary")
  plot_fol <- make_folder(disk, metric, "14_gridcell_binary_plot")
  
  
  
  
# Get data and other things ----------------------------------------------------
  
  network_mpas <- readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))
  network_mpas



  
# Plot these on top of each other ----------------------------------------------
  ## i.e., plot for each SSP for the whole study region, as BINARY layers

  files <- dir(strength_agg_fol, full.names = TRUE, pattern = ".RDS")
  files
  percentile <- 0.9 # 90th percentile (top 10%)

  all_bin <- map_dfr(files, function(f) {
    
    d_bin <- readRDS(f) # Get the binary file
    ssp_name <- basename(f) %>% str_split_i(., "_", 6) %>% str_remove(., ".RDS") # get the SSP for each file
    
    # Calc threshold for each ssp
    threshold <- quantile(d_bin$n_trajectories, percentile) # Find the threshold of the top 10% of n_trajectories
    
    # Filter to this threshold and add yn_bin (binary classifer for each cell, value of 1)
    d_top <- d_bin %>%
      filter(n_trajectories >= threshold) %>%
      mutate(yn_bin = 1,
             ssp = ssp_name) %>%
      select(grid_ID, n_trajectories, yn_bin, ssp, geometry)
    
    message(paste0(ssp_name, ": ", nrow(d_top), " cells (threshold = ", round(threshold), ", ", percentile * 100, "th percentile)"))
    return(d_top)
  })

  
  ## Count no. of ssps correspond to each cell
    bin_summary <- all_bin %>%
      st_drop_geometry() %>%
      group_by(grid_ID) %>%
      summarise(bin_freq = sum(yn_bin),
                ssp_combo = paste(sort(unique(ssp)), collapse = "_"),
                .groups = "drop")
    
    bin_summary %>% count(bin_freq) # Check dist
      # FOR 90th PERCENTILE:
      #   bin_freq     n
      #             <dbl> <int>
      # 1        1  7550
      # 2        2  5062
      # 3        3  3823
      # 4        4  8106
    
    
  ## Get geometry back per grid_ID
    bin_tot <- all_bin %>%
      select(grid_ID, geometry) %>%
      distinct(grid_ID, .keep_all = TRUE) %>%
      left_join(bin_summary, by = "grid_ID")
  
  
  ## Get centroids for each grid cell
    bin_cent <- bin_tot %>%
      st_centroid() %>%
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2]
      ) %>%
      st_drop_geometry() %>%
      arrange(bin_freq) # Plot lower values first
  
    
    
    
# Plot these on top of each other ----------------------------------------------
  ## Plot it for the whole study region, ignoring MPAs, for each term still, but as BINARY layers for layering on top 

  s_plot <- ggplot() +
    geom_sf(data = mpa_shp, fill = "#C6BDA2", alpha = 0.7, color = NA) +
    geom_tile(data = bin_cent,
              aes(x = lon, y = lat, fill = factor(bin_freq)),
              width = 0.125,
              height = 0.125,
              alpha = 0.7) +
    scale_fill_manual(values = freq_colours,
                      name = "SSP\nagreement",
                      labels = c("1" = "1 SSP", "2" = "2 SSPs", "3" = "3 SSPs", "4" = "4 SSPs"),
                      drop = FALSE) +
    geom_sf(data = oceania_stanford_shp, fill = "#404F4B", col = NA) + #404F4B
    geom_sf(data = eez_shp, fill = NA, color = "black", lwd = 0.25) +
    coord_sf(xlim = c(110, 180), ylim = c(-50, -5)) +
    theme_void(base_size = 12) +
    labs(title = "Climate connectivity corridor robustness across SSP scenarios",
         subtitle = paste0("Grid cells in top ", (1-percentile)*100, "% (", percentile*100, "th percentile per SSP) of distinct trajectories passing through."))
  s_plot

  ggsave(paste0(plot_fol, "/corridor_grid_frequency_distinct-traj-visits_tile_", percentile*100, "th_perc_allSSPs_warm_DARK.pdf"), 
         s_plot, width = 14, height = 8, dpi = 300, bg = "white")
  ggsave(paste0(plot_fol, "/corridor_grid_frequency_distinct-traj-visits_tile_", percentile*100, "th_perc_allSSPs_warm_DARK.png"), 
         s_plot, width = 14, height = 8, dpi = 300, bg = "white")
  
    
  

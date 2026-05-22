# Plot well-worn corridors (for grid cells, not just MPAs) of entire 20-year trajectory sequences per SSP
  ## As a density plot heatmap
    # Written by Alice Pidd
        # Dec 2025


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"


  
  
# Folders ----------------------------------------------------------------------
  
  seq_fol <- make_folder(disk, metric, "2_sequence")
  corridor_fol <- make_folder(disk, metric, "16_corridors")
  plot_fol <- make_folder(disk, metric, "17_corridors_plot_geo")
  
  
  
  
# Get data ---------------------------------------------------------------------
  
  network_mpas <- readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))
  network_mpas

  top90pct_seq <- readRDS(paste0(corridor_fol, "/corridor_strength_allESMs_MPAtoMPA_top90pct.RDS")) # Top 90% of MPAtoMPA trajectories by cumulative frequency
  top90pct_seq 

  
  
  
# Plot density heatmap for the whole of Australia, not just MPA-MPA, per SSP only ------------

  plot_main_corridors_by_ssp <- function(ssp_val, pct) {
    
    dat <- top90pct_seq
    
    # Get all sequences for this SSP (across all terms)
      seq_data <- dat %>%
        ungroup() %>%
        filter(ssp == ssp_val) %>%
        dplyr::select(ssp, term, seq_key, freq, traj_IDs)
      
      if(nrow(seq_data) == 0) {
        message(paste0("No data for ", ssp_val))
        return(NULL)
      }
    
    # Expand to get which traj_ID corresponds with each term
      traj_id_lookup <- seq_data %>%
        rowwise() %>%
        mutate(traj_id_list = list(tibble(traj_ID = traj_IDs[[1]], term = term))) %>%
        dplyr::select(traj_id_list) %>%
        unnest(traj_id_list) %>%
        distinct(traj_ID, term)
      
      message(paste0(ssp_val, ": Loading ", nrow(traj_id_lookup),
                     " unique trajectories from the top ", pct, "% most frequented sequences")) # If using top X seqs (summarystats_topXseq.RDS)

    # Load actual trajectory spatial data from files
      set.seed(123) # For reproducibility
      tic()
      traj_paths <- map_dfr(unique(traj_id_lookup$term), function(term_val) { # For sampling full trajectories
        
        term_traj_ids <- traj_id_lookup$traj_ID[traj_id_lookup$term == term_val]
        
        term_files <- dir(seq_fol,
                          pattern = paste0("traj_sequence_.*_", ssp_val, "_", term_val, "\\.RDS"), 
                          full.names = TRUE)
        
        if(length(term_files) == 0) return(tibble())
        
        map_dfr(term_files, function(file_path) {
          esm <- str_extract(basename(file_path), "(?<=traj_sequence_)[^_]+")
          readRDS(file_path) %>%
            filter(traj_ID %in% term_traj_ids) %>%
            mutate(traj_ID_unique = paste(traj_ID, term_val, esm, sep = "_")) # Make a unique traj_ID but name it also with the esm and term it comes from, so they are distinguishable later on.
        })
      })
      toc() 

    # Plot
    message(paste0("Plotting ", length(unique(traj_paths$traj_ID_unique)), " trajectories"))

    ## Heatmap of climate corridors for whole century, pretty sick ------
    # Sample trajectories bc there are too many for R to handle
      if(nrow(traj_paths) > 10000000) { # For sampling full trajectories
        n_trajs_needed <- round(10000000 / 240)  # 10 million points is roughly ~42,000 trajectories
        
        set.seed(123)
        # Randomly pick 42,000 unique traj_IDs
        sampled_ids <- sample(unique(traj_paths$traj_ID_unique), size = n_trajs_needed) 
        traj_sampled <- traj_paths %>% filter(traj_ID_unique %in% sampled_ids)  # Keep ALL points from those 42,000 trajectories
        message(paste0("Sampled ", length(sampled_ids), " complete trajectories (", 
                       nrow(traj_sampled), " points)"))  # Changed from "trajectories" to "points"
      } else {
        traj_sampled <- traj_paths
      }

  # Plot density heatmap vibes
    tic()
    p <- ggplot() +
      geom_sf(data = mpa_shp, fill = "#C6BDA2", alpha = 0.7, color = NA, lwd = 0.04) + # Other colours #CBC6B7 #BADDD8 or #CEDEDC 
      stat_density_2d(data = traj_sampled,  # Use sampled data
                      aes(x = lon, y = lat, fill = after_stat(level)),
                      geom = "polygon", alpha = 0.4,
                      bins = 40) + # Change bins for level of detail (e.g., 30 vs 300)
      
      geom_sf(data = oceania_stanford_shp, fill = "#404F4B", col = NA) + #536560
      geom_sf(data = eez_shp, fill = NA, color = "black", lwd = 0.25) + # Other colours #BADDD8 or #CEDEDC
      coord_sf(xlim = c(110, 180), ylim = c(-50, -5)) +  # crop to study area
      scale_fill_gradientn(colors = heatpal, name = "Trajectory\ndensity",
                           limits = c(0, 0.035),
                           n.breaks = 4) +
      theme_void() +
      theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0),
            plot.subtitle = element_text(color = "grey50")) +
      labs(title = paste0("MPA connectivity corridors under ", ssp_val),
           subtitle = paste0("Sequences representing ", pct, "% of linkages, ", 
                             scales::comma(length(sampled_ids)), " trajectories sampled"))    
    toc() # 4 mins per plot

    o_nm <- paste0(plot_fol, "/main_corridors_trajIDs_", ssp_val, "_top", pct, "pct_", 
                   scales::comma(length(sampled_ids)), "trajs_DARK1.pdf")
    ggsave(o_nm, plot = p, width = 14, height = 8, dpi = 300, bg = "white")
    message(paste0("✅ Saved: ", o_nm))
    return(p)
  }
  
  tic()
  plots <- map2(ssp_list, 90, plot_main_corridors_by_ssp)
  toc() # 34 mins on Alice's machine
  
  beep(2)
  message("All corridor plots saved!")
  

  
  
# Plot density heatmap for the whole of Australia, not just MPA-MPA, per SSP-term combo ------------
  ssp_val <- "ssp245"
  term_val <- "near-term"
  pct <- "90"
  
  plot_main_corridors_by_sspterm <- function(ssp_val, term_val, pct) {
    
    dat <- top90pct_seq
    
    # Get all sequences for this SSP (across all terms)
      seq_data <- dat %>%
        ungroup() %>%
        filter(ssp == ssp_val) %>%
        filter(term == term_val) %>%
        dplyr::select(ssp, term, seq_key, freq, traj_IDs)
      
      if(nrow(seq_data) == 0) {
        message(paste0("No data for ", ssp_val))
        return(NULL)
      }
    
    # Expand to get which traj_ID corresponds with each term
      traj_id_lookup <- seq_data %>%
        rowwise() %>%
        mutate(traj_id_list = list(tibble(traj_ID = traj_IDs[[1]], term = term))) %>%
        dplyr::select(traj_id_list) %>%
        unnest(traj_id_list) %>%
        distinct(traj_ID, term)
      
      message(paste0(ssp_val, ": Loading ", nrow(traj_id_lookup),
                     " unique trajectories from the top ", pct, "% most frequented sequences")) # If using top X seqs (summarystats_topXseq.RDS)

    # Load actual trajectory spatial data from files
      set.seed(123) # For reproducibility
      tic()
      traj_paths <- map_dfr(unique(traj_id_lookup$term), function(term_val) { # For sampling full trajectories
        
        term_traj_ids <- traj_id_lookup$traj_ID[traj_id_lookup$term == term_val]
        
        term_files <- dir(seq_fol,
                          pattern = paste0("traj_sequence_.*_", ssp_val, "_", term_val, "\\.RDS"), 
                          full.names = TRUE)
        
        if(length(term_files) == 0) return(tibble())
        
        map_dfr(term_files, function(file_path) {
          esm <- str_extract(basename(file_path), "(?<=traj_sequence_)[^_]+")
          readRDS(file_path) %>%
            filter(traj_ID %in% term_traj_ids) %>%
            mutate(traj_ID_unique = paste(traj_ID, term_val, esm, sep = "_")) # Make a unique traj_ID but name it also with the esm and term it comes from, so they are distinguishable later on.
        })
      })
      toc() 

    # Plot
    message(paste0("Plotting ", length(unique(traj_paths$traj_ID_unique)), " trajectories"))

    ## Heatmap of climate corridors for whole century, pretty sick ------
    # Sample trajectories bc there are too many for R to handle
      if(nrow(traj_paths) > 10000000) { # For sampling full trajectories
        n_trajs_needed <- round(10000000 / 240)  # 10 million points is roughly ~42,000 trajectories
        
        set.seed(123)
        # Randomly pick 42,000 unique traj_IDs
        sampled_ids <- sample(unique(traj_paths$traj_ID_unique), size = n_trajs_needed) 
        traj_sampled <- traj_paths %>% filter(traj_ID_unique %in% sampled_ids)  # Keep ALL points from those 42,000 trajectories
        message(paste0("Sampled ", length(sampled_ids), " complete trajectories (", 
                       nrow(traj_sampled), " points)"))  # Changed from "trajectories" to "points"
      } else {
        traj_sampled <- traj_paths
      }

  # Plot density heatmap vibes
    tic()
    p <- ggplot() +
      geom_sf(data = mpa_shp, fill = "#CBC6B7", alpha = 0.7, color = NA, lwd = 0.04) + # Other colours #BADDD8 or #CEDEDC
      stat_density_2d(data = traj_sampled,  # Use sampled data
                      aes(x = lon, y = lat, fill = after_stat(level)),
                      geom = "polygon", alpha = 0.4,
                      bins = 40) + # Change bins for level of detail (e.g., 30 vs 300)
      
      geom_sf(data = oceania_stanford_shp, fill = "grey80", col = NA) +
      geom_sf(data = eez_shp, fill = NA, color = "black", lwd = 0.25) + # Other colours #BADDD8 or #CEDEDC
      coord_sf(xlim = c(110, 180), ylim = c(-50, -5)) +  # crop to study area
      scale_fill_gradientn(colors = heatpal, name = "Trajectory\ndensity",
                           limits = c(0, 0.035),
                           n.breaks = 4) +
      theme_void() +
      theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0),
            plot.subtitle = element_text(color = "grey70")) +
      labs(title = paste0("MPA connectivity corridors under ", ssp_val),
           subtitle = paste0("Sequences representing ", pct, "% of linkages, ", 
                             scales::comma(length(sampled_ids)), " trajectories sampled"))    
    toc() # 4 mins per plot

    o_nm <- paste0(plot_fol, "/main_corridors_trajIDs_", ssp_val, "_top", pct, "pct_", 
                   scales::comma(length(sampled_ids)), "trajs.pdf")
    ggsave(o_nm, plot = p, width = 14, height = 8, dpi = 300, bg = "white")
    message(paste0("✅ Saved: ", o_nm))
    return(p)
  }
  
  tic()
  plots <- map2(ssp_list, 90, plot_main_corridors_by_ssp)
  toc() # 34 mins on Alice's machine
  
  beep(2)
  message("All corridor plots saved!")
  

  
  
# Plotting oceania in case I need it separately --------------------------------
  
  p <- ggplot() +
    geom_sf(data = oceania_stanford_shp, fill = "grey80", col = NA) +
    theme_void()
  
  ggsave("/Volumes/AliceShield/conn_data/VoCCtracers/_figures/Oceania_land_grey80.svg", 
         plot = p,
         width = 14, 
         height = 8, 
         bg = "transparent",
         device = svglite)
    
  

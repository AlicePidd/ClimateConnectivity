# Calculate the well-worn corridors (through MPAs) of trajectory sequences for each SSP all up
    # Written by Alice Pidd
        # Jan 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"

  
  

# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(disk, metric, "11_gridcell_strength")
  o_fol <- make_folder(disk, metric, "12_gridcell_strength_aggregated")

  
  
  
# Get data and other things ----------------------------------------------------
  
  network_mpas <- readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))
  network_mpas
  # term_list <- paste0(term_list, "-term")
  
  # Takes ages to read in FYI
  all_strength <- readRDS(paste0(in_fol, "/ALL_COMBOS_gridcell_strength_distinct-traj-visits.RDS")) 
  

  

# Aggregate grid strengths per SSP (combined terms) ----------------------------
  
  agg_dat <- function(ssp_val) {
    o_nm <- paste0(o_fol, "/grid_strength_distinct-traj-visits_aggterms_noZeros_", ssp_val, ".RDS")
    
    # Skip if already exists
    if (file.exists(o_nm)) {
      message(paste0("⏭️  Skipping ", ssp_val, " - file already exists: ", basename(o_nm)))
      return(invisible(NULL))
    }
    
    message(paste0("Processing: ", ssp_val, ", filtering out where n_trajectories = 0"))
    
    # Filter FIRST to reduce from 11 million and something lines
    tic()
    d_sub <- all_strength %>%
      filter(n_trajectories > 0) %>% # Remove 0's
      filter(ssp == ssp_val) # Only care about SSP here, want to combine all terms
    toc() # 30 sec per SSP
    
    message(paste0("  Filtered to ", nrow(d_sub), " rows"))
    
    # Aggregate frequencies across ESMs and terms
    tic()
    d_agg <- d_sub %>%
      group_by(grid_ID) %>%
      summarise(n_trajectories = sum(n_trajectories), .groups = "drop")
    toc() # Takes about 2 hours per ssp 
      ##**Saving so don't need to do it again**
    saveRDS(d_agg, o_nm)
  
    message(paste0("  Saved:   ", basename(o_nm)))
  }
  
  tic()
  walk(ssp_list, agg_dat) # Can't run in parallel, need 9.26 GiB memory (could only increase it to 10.00 MiB)
  toc() # 3.2 hours total for the 4 SSPs
  beep(5)
  

  
  
  
  

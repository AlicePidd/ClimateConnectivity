# Grid cell strength - the number of unique trajectories that travel through each grid_ID, for ALL ESMs aggregated
    # Written by Alice Pidd
        # Nov 2025


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"

  
  
  
# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(disk, metric, "2_sequence")
  o_fol <- make_folder(disk, metric, "11_gridcell_strength")

  
  
  
# Compute the number of unique trajectories (traj_ID) that travel through each grid_ID ------
  
  get_grid_counts <- function(f) {
    message("Processing: ", basename(f))
    
    esm <- basename(f) %>% str_split_i(., "_", 3)
    ssp <- basename(f) %>% str_split_i(., "_", 4)
    term <- basename(f) %>% str_split_i(., "_", 5) %>% str_remove(., ".RDS")
    o_nm <- paste0(o_fol, "/gridcell_strength_", esm, "_", ssp, "_", term, ".RDS")
    
    if(file.exists(paste0(o_fol, "/", o_nm))) { 
      message(paste0("⏭️  Skipping (already exists): ", basename(o_nm)))
      return(invisible(NULL))
    }
    
    traj_data <- readRDS(f)
    
    # Count the number of unique trajectories that pass through each grid_cell
    c <- traj_data %>%
      group_by(grid_ID) %>%
      summarise(n_trajectories = n_distinct(traj_ID)) %>% # Distinct trajectories
      mutate(ssp = ssp, term = term, esm = esm)    
    
    strength_grid <- base_grid %>% 
      left_join(c, by = "grid_ID") %>% # Join the counts of trajs to each grid_ID
      replace_na(list(n_trajectories = 0, # Where there are no grid_ID_visits, make it zero
                      ssp = ssp, # Keep all the other details as is
                      term = term,
                      esm = esm))

    saveRDS(strength_grid, o_nm)
    message("✅ Saved: ",  paste0("gridcell_strength_distinct_trajectories_", esm, "_", ssp, "_", term, ".RDS"))
    
  }
  
  files <- dir(in_fol, full.names = TRUE)
  files
  tic()
  walk(files, get_grid_counts)
  toc() # 33 mins on Alice's machine
  beep(2)
  
  
  
  
# Bind all the trajectory grid count files into one ----------------------------
  ## Takes a while
  
  bind_files <- function(pth) {
    files <- dir(pth, full.names = TRUE, pattern = "term")
    all_data <- map_dfr(files, readRDS)
    return(all_data)
  }
  
  all_strength <- bind_files(o_fol) %>% 
    mutate(term = factor(term, # Reorder the terms
                  levels = c("recent-term", "near-term", "mid-term", "intermediate-term", "long-term")))
  all_strength
  saveRDS(all_strength, paste0(o_fol, "/ALL_COMBOS_gridcell_strength_distinct-traj-visits.RDS"))
  beep(2)
  

  
    
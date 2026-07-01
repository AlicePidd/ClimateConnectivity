# Calculating the proportion of time that trajectories are protected throughout their lifetime
    # Written by Alice Pidd
        # July 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"


  

# Folders ----------------------------------------------------------------------

  seq_fol <- make_folder(disk, metric, "2_sequence")
  sf_fol <- make_folder(disk, metric, "2_sequence_sf")
  prop_fol <- make_folder(disk, metric, "18_cumulative_traj_protection")
  propall_fol <- make_folder(disk, metric, "18_cumulative_traj_protection/all")
  propmpa_fol <- make_folder(disk, metric, "18_cumulative_traj_protection/mpa-starts")
  

  
    
# Function ---------------------------------------------------------------------
  ## Calculates the proportion of time each traj_ID spends in MPAs over its lifetime for:
    # ALL trajs
    # Only the trajs that start in MPAs
  
  get_prop <- function(ssp, term){
    
    files <- dir(seq_fol, full.names = TRUE, pattern = ssp) %>% 
      str_subset(., "recent-term", negate = TRUE) %>% 
      str_subset(., term)
    
    do_prop <- function(f) {
      
      s <- readRDS(f) 
      esm <- basename(f) %>% 
        str_split_i(., "_", 3)
      ssp <- basename(f) %>% 
        str_split_i(., "_", 4)
      term <- basename(f) %>% 
        str_split_i(., "_", 5) %>% str_remove(., ".RDS")
      
      # For ALL trajectories, what is their cumulative thermal protection over their lifetime
        s_prop <- s %>%
          group_by(traj_ID) %>%
          summarise(total_steps = n(),
                    steps_in_MPA = sum(MPA_ID != -999),
                    prop_in_MPA = steps_in_MPA / total_steps) %>%
          mutate(ssp = ssp, esm = esm, term = term)
  
      # For MPA-start trajs only, what is their cumulative thermal protection over their lifetime
        MPA_start_IDs <- s %>%
          filter(Time == 0 & MPA_ID != -999) %>%
          pull(traj_ID)
        
        s_MPAstart <- s %>%
          filter(traj_ID %in% MPA_start_IDs) %>% # 3,970,500 more rows
          mutate(ssp = ssp, esm = esm, term = term)
        
        s_MPAstart_prop <- s_MPAstart %>% 
          group_by(traj_ID) %>%
          summarise(total_steps = n(), # 
                    steps_in_MPA = sum(MPA_ID != -999),
                    prop_in_MPA = steps_in_MPA / total_steps) %>% 
          mutate(ssp = ssp,
                 esm = esm, 
                 term = term)
        
        return(list(all = s_prop, 
                    mpa_start = s_MPAstart_prop))
        
    }
    
    results <- map(files, do_prop)
    
    comb_all <- map(results, "all") %>% 
      bind_rows()
    comb_MPAstart <- map(results, "mpa_start") %>% 
      bind_rows()
    
    saveRDS(comb_all, paste0(propall_fol, "/cumulative_traj_protection_all_", ssp, "_", term, "_ESMscombined.RDS"))
    saveRDS(comb_MPAstart, paste0(propmpa_fol, "/cumulative_traj_protection_MPAstarts_", ssp, "_", term, "_ESMscombined.RDS"))
    
    message("Done: ", ssp, " ", term)
  }
  
  ssp_list
  terms <- paste0(term_list, "-term")[2:5]
  combos <- expand.grid(ssp = ssp_list, term = terms, stringsAsFactors = FALSE)
  # c <- combos %>% slice(1)
  
  tic()
  pwalk(combos, get_prop) # Applies each element in the columns (only if correctly named) to get_prop
  toc() # 28.31 mins
  beep(2)

    
  
  
# Get starting points for all trajectories -------------------------------------
  
  ## Checking that points are identical in their geometries across ESMs, so I can just take one set ----------
    # # tic()
    # f1 <- readRDS("/Volumes/AliceShield/conn_data/VoCCtracers/2_sequence_sf/traj_sequence_sf_ACCESS-CM2_ssp126_near-term.RDS") %>%
    #   filter(Time == 0) %>%
    #   dplyr::select(traj_ID, geometry) %>%
    #   arrange(traj_ID)
    # 
    # f2 <- readRDS("/Volumes/AliceShield/conn_data/VoCCtracers/2_sequence_sf/traj_sequence_sf_NorESM2-LM_ssp585_long-term.RDS") %>%
    #   filter(Time == 0) %>%
    #   dplyr::select(traj_ID, geometry) %>%
    #   arrange(traj_ID)
    # 
    # identical(f1, f2)
    # # toc()
    # beep(2)
    
    
  ## Extract starting points of MPA-starters from one ESM per SSP-term combo ----------

    sf_files <- dir(sf_fol, full.names = TRUE)
    tic()
    start_points <- readRDS(sf_files[1]) %>%
      filter(Time == 0 & MPA_ID != -999) %>%
      dplyr::select(traj_ID, geometry)
    start_points
    toc()
    beep(2)
    
    saveRDS(start_points, paste0(prop_fol, "/trajectory_MPA-start_point_geometries.RDS"))
    
  
  ## Extract starting points of all trajectories from one ESM per SSP-term combo ----------
    
    tic()
    start_points_all <- readRDS(sf_files[1]) %>%
      filter(Time == 0) %>%
      dplyr::select(traj_ID, geometry)
    start_points_all
    toc()
    beep(2)
    
    saveRDS(start_points_all, paste0(prop_fol, "/trajectory_all-trajs_point_geometries.RDS"))
    
    
    
    
# Get proportion data ----------------------------------------------------------
  
  read_and_join <- function(f){
    d <- readRDS(f)
    return(d)
  }
  
  
  ## MPA-start files ---------
  
    mpa_files <- dir(propmpa_fol, full.names = TRUE, pattern = "MPAstarts") %>% 
      str_subset(., "recent-term", negate = TRUE)
    mpa_comb <- map(mpa_files, read_and_join) %>% 
      bind_rows() %>% 
      mutate(term_label = factor(term,
                                 levels = c("near-term", 
                                            "mid-term", "intermediate-term", 
                                            "long-term"),
                                 labels = c("Near-term\n(2021-2040)", 
                                            "Mid-term\n(2041-2060)", 
                                            "Intermediate-term\n(2061-2080)", 
                                            "Long-term\n(2081-2100)")),
             ssp = factor(ssp, levels = c("ssp126", "ssp245", "ssp370", "ssp585")))
    mpa_comb

    med_mpastart_prop <- mpa_comb %>% 
      filter(total_steps == 241) %>%
      group_by(traj_ID, term_label, ssp) %>%
      reframe(med_prop = pmax(median(prop_in_MPA), 0.5), # Truncate from 0.5-1
              group = "MPAstart")
    med_mpastart_prop
    saveRDS(med_mpastart_prop, paste0(prop_fol, "/median_proportions_of_protection_for_MPA-start_trajectories.RDS"))
    
  
  ## All files ---------
    
    all_files <- dir(propall_fol, full.names = TRUE, pattern = "all") %>% 
      str_subset(., "recent-term", negate = TRUE)
    all_comb <- map(all_files, read_and_join) %>% 
      bind_rows() %>% 
      mutate(term_label = factor(term,
                                 levels = c("near-term", 
                                            "mid-term", "intermediate-term", 
                                            "long-term"),
                                 labels = c("Near-term\n(2021-2040)", 
                                            "Mid-term\n(2041-2060)", 
                                            "Intermediate-term\n(2061-2080)", 
                                            "Long-term\n(2081-2100)")),
             ssp = factor(ssp, levels = c("ssp126", "ssp245", "ssp370", "ssp585")))
    all_comb

    tic()
    med_all_prop <- all_comb %>% 
      filter(total_steps == 241) %>%
      group_by(traj_ID, term_label, ssp) %>%
      reframe(med_prop = pmin(median(prop_in_MPA), 0.5), # Truncate from 0-0.5
              group = "Alltraj")
    med_all_prop
    toc() # 115 sec
    saveRDS(med_all_prop, paste0(prop_fol, "/median_proportions_of_protection_for_ALL_trajectories.RDS"))
    
# Calculating the proportion of time that trajectories are protected throughout their lifetime
    # Written by Alice Pidd
        # July 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"


  

# Folders ----------------------------------------------------------------------

  seq_fol <- make_folder(disk, metric, "2_sequence")
  propall_fol <- make_folder(disk, metric, "18_cumulative_traj_protection/all")
  propmpa_fol <- make_folder(disk, metric, "18_cumulative_traj_protection/mpa-starts")
  

  
    
# Function ---------------------------------------------------------------------
  ## Calculates the proportion of time each traj_ID spends in MPAs over its lifetime for:
    # ALL trajs
    # Only the trajs that start in MPAs

  ssp = "ssp245"
  term = "mid-term"
  f <- files[1]
  f
  
  get_prop <- function(ssp, term){
    
    files <- dir(seq_fol, full.names = TRUE, pattern = ssp) %>% 
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
  terms <- paste0(term_list, "-term")
  terms
  combos <- expand.grid(ssp = ssp_list, term = terms, stringsAsFactors = FALSE)
  combos
  # c <- combos %>% slice(1)
  
  tic()
  pwalk(combos, get_prop) # Applies each element in the columns (only if correctly named) to get_prop
  toc() # 28.31 mins
  beep(2)


    
  
  
# Get starting points for all trajectories, filtered to 
  
  
  
  f1 <- readRDS("/Volumes/AliceShield/conn_data/VoCCtracers/2_sequence_sf/traj_sequence_sf_ACCESS-CM2_ssp245_mid-term.RDS") %>%
    filter(Time == 0) %>%
    dplyr::select(traj_ID, geometry) %>%
    arrange(traj_ID)
  
  f2 <- readRDS("/Volumes/AliceShield/conn_data/VoCCtracers/2_sequence_sf/traj_sequence_sf_ACCESS-ESM1-5_ssp245_mid-term.RDS") %>%
    filter(Time == 0) %>%
    dplyr::select(traj_ID, geometry) %>%
    arrange(traj_ID)
  
  identical(f1, f2)
  
  
  
  
    
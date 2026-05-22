# Get total strength between MPA pairs
  # Sums the MPA pairs counts (in + out) for all ESMs, under each SSP-term combination, and make matrices
    # Written by Alice Pidd
        # Nov 2025


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"

  
  
# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(disk, metric, "3_pairs_mpa")
  o_fol <- make_folder(disk, metric, "4_pairs_mpa_summed")
  stats_fol <- make_folder(disk, metric, "5_pairs_mpa_stats")
  
  
  
# Sum pairwise MPA counts for all ESMs, under each SSP-term combination, and make matrices -------------
  
  # ssp <- ssp_list[1]
  # term <- term_list[4]
  
  get_and_list <- function(ssp, term) {
    sel_files <- dir(in_fol, full.names = TRUE) %>% # Get files based on the combo of ssp and term
      str_subset(., ssp) %>% 
      str_subset(., term)
    
    o_nm <- paste0("/traj_pairs_MPAs_ESMs-summed_", ssp, "_", term, "-term.RDS")
    
    if(length(sel_files) == 0) {
      message(paste0("⚠️ No files found for ", ssp, " ", term, "-term, skipping..."))
      return(invisible(NULL))
    }
    
    if(file.exists(paste0(o_fol, "/", o_nm))) { # Have to make sure the path is included
      message(paste0("⏭️  Skipping (already exists): ", basename(o_nm)))
      return(invisible(NULL))
    }
    
    get_df <- function(f){
      d <- readRDS(f)
      return(d)
    }

    summed_df <- map(sel_files, get_df) %>% 
      list() %>% # List the files in the selection
      bind_rows() %>% # Bind them, so we can sum n
      group_by(from, to) %>%
      summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
      arrange(from, to) %>% 
      as.data.frame()
    saveRDS(summed_df, paste0(paste0(o_fol, "/", o_nm))) # No non-MPAs
  }
  
  combo <- expand_grid(ssp = ssp_list, term = term_list) # Get all combinations
  pwalk(combo, get_and_list) # For every ssp-term combo in combo, do the fn
  

  
  
# Proportion of MPA pairs that are self-retention, or between different MPAs --------------
  
  pairs_prop <- function(f) {
    p <- readRDS(f)
    ssp <- basename(f) %>%
      str_split_i(., "_", 5)
    term <- basename(f) %>%
      str_split_i(., "_", 6) %>% 
      str_remove(., ".RDS")
    
    pair_props <- p %>% 
      na.omit() %>% 
      summarise(ssp = ssp,
                term = term,
                total_n = sum(n),
                self_n = sum(n[from == to]), # Self-seeding (A -> A)
                between_n = sum(n[from != to]), # Between different MPA numbers (A -> B, A -> C, B -> F)
                prop_self = self_n / total_n, # Proportion of self-seeding trajs out of all trajs
                prop_between = between_n / total_n, # Proportion of trajs between different MPAs out of all trajs
                totals_match = ifelse(sum(self_n, between_n) == total_n, # Checking the totals match
                                      TRUE, FALSE),
                props_equal_1 = ifelse(sum(prop_self, prop_between) == 1, 
                                       TRUE, FALSE)) # And the proportions
    return(pair_props)
  }
  
  files <- dir(o_fol, full.names = TRUE)
  files

  results <- map(files, pairs_prop) %>% 
    bind_rows() %>% 
    arrange(ssp, term)
  results
  saveRDS(results, paste0(stats_fol, "/proportion_self_retention_or_between_MPAs.RDS"))
  

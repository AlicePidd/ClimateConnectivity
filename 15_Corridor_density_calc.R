# Isolate well-worn corridors (through MPAs) of entire 20-year trajectory sequences
    # Written by Alice Pidd
        # Dec 2025

# This script does a fair bit:
  # Gets a frequency of use for each unique sequence travelled by trajectories, per ssp-term-esm combination
    # Also lists all the corresponding traj_IDs for each sequence
  # Splits the full sequences into stages (when the traj goes from one thing to another, including non-MPAs)
    # Aggregates the frequencies across ESMs, so representing each ssp-term combo
  # Filters these aggregated sequences for where there are 2+ different MPAs visited, so only MPA-to-MPA, excluding self-retention
  # Uses the MPA-to-MPA files, calculates some summary stats 


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"

  
  
  
# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(source_disk, metric, "2_sequence")
  o_fol <- make_folder(source_disk, metric, "16_corridors")

  
  
  
# Get the unique, 20-year MPA sequences travelled by trajectories, per ssp-term-esm combo -------------

  count_unique_sequences <- function(f) {
    esm <- basename(f) %>% str_split_i(., "_", 3)
    ssp <- basename(f) %>% str_split_i(., "_", 4)
    term <- basename(f) %>% str_split_i(., "_", 5) %>% str_remove(., ".RDS")
    
    seq <- readRDS(f) 
    
    # Get sequences WITH trajectory IDs
    traj_seqs <- seq %>%
      arrange(traj_ID, Time) %>% 
      group_by(traj_ID) %>%
      reframe(seq_list = list(rle(MPA_ID)$values),
              .groups = "drop") %>% 
      mutate(seq_key = map_chr(seq_list, ~ paste(.x, collapse = "-"))) %>%
      mutate(ssp = ssp,
             term = term,
             esm = esm,
             traj_ID_unique = paste(traj_ID, esm, sep = "_")) %>%  # ADD THIS
      select(ssp, term, esm, traj_ID, traj_ID_unique, seq_key)
    
    # Count frequencies per sequence
    seq_counts <- traj_seqs %>%
      group_by(ssp, term, esm, seq_key) %>%
      summarise(
        freq = n(),
        traj_IDs = list(traj_ID),  # Store all trajectory IDs for this sequence
        traj_IDs_unique = list(traj_ID_unique),  # ADD THIS
        .groups = "drop"
      ) %>%
      arrange(desc(freq))
    
    return(seq_counts)
  }
  
  files <- dir(in_fol, full.names = TRUE)
  files

  tic()
  results <- map(files, count_unique_sequences) %>% 
    bind_rows() # Put it all in the one
  toc() # 1.9 hours (Alice's machine)
  
  beep(2)
  results
    unique(results$term)
    unique(results$ssp)
    unique(results$esm)

  saveRDS(results, paste0(o_fol, "/corridor_strength_allESMs_sequence.RDS"))
  

  
  
# Load the seq_key file, and split it up into stages (for multiple MPAs along a traj) ---------
  
  ## Read in all unique sequences and their frequencies of use by trajectories, per ssp-term-esm combo ------------
    results <- readRDS(paste0(o_fol, "/corridor_strength_allESMs_sequence.RDS")) 
  

  ## Create network lookup using existing list ------------
    network_lookup <- map_dfr(names(network_mpas), function(net) {
      tibble(mpa_id = as.character(network_mpas[[net]]), network = net)
    })
    network_lookup


  ## Split the sequences into stages, without non-MPAs ------------
    parse_sequence <- function(seq_key, max_stages = 20) {
      # Replace -999 with a placeholder that won't be split
      temp <- str_replace_all(seq_key, "^-999", "NONMPA")
      temp <- str_replace_all(temp, "--999", "-NONMPA")
      
      stages <- str_split(temp, "-")[[1]]
      
      # Convert NONMPA back to Non-MPA after splitting
      stages <- ifelse(stages == "NONMPA", "Non-MPA", stages)
      
      # Assign networks
      networks <- map_chr(stages, function(s) {
        if(s == "Non-MPA") return("Non-MPA")
        net <- network_lookup$network[network_lookup$mpa_id == s]
        if(length(net) > 0) return(net[1])
        return("Unknown")
      })
      
      # Pad to max number of stages
      length(stages) <- max_stages
      length(networks) <- max_stages
      names(stages) <- paste0("stage_", 1:max_stages)
      names(networks) <- paste0("network_", 1:max_stages)
      
      as_tibble_row(c(as.list(stages), as.list(networks)))
    }
    
    tic()
    dat <- results %>%
      rowwise() %>%
      mutate(parsed = list(parse_sequence(seq_key))) %>%
      unnest(parsed) %>%
      ungroup()
    toc() # 4.5 mins on Alice's machine
    dat
  

  ## Aggregate sequence frequencies for all ESMs across ssp-term combos ------------
    dat_aggregated <- dat %>%
      group_by(ssp, term, seq_key) %>%
      summarise(
        freq = sum(freq),  # Sum frequencies across all ESMs
        traj_IDs = list(unlist(traj_IDs)),  # Combine all traj_IDs from all ESMs
        # Keep the stage and network info (same for all ESMs with same seq_key)
        across(starts_with("stage_"), first),
        across(starts_with("network_"), first),
        .groups = "drop"
      )
    dat_aggregated
    saveRDS(dat_aggregated, paste0(o_fol, "/corridor_strength_allESMs_sequence_splitstaged.RDS"))

  
    
    
# Filter for MPA-to-MPA connectivity (2+ different MPAs) -----------------------
  ## This filters for MPA-to-MPA connectivity by keeping only trajs that visit 2 or more dif MPAs, and allows for Non-MPA visitation as a stepping stone between different MPAs
  
  dat_aggregated <- readRDS(paste0(o_fol, "/corridor_strength_allESMs_sequence_splitstaged.RDS"))
  dat_aggregated
  
  tic()
  dat_MPAtoMPA <- dat_aggregated %>%
    rowwise() %>%
    filter({
      stages <- c_across(starts_with("stage_"))
      stages <- stages[!is.na(stages)]
      mpa_stages <- stages[stages != "Non-MPA"]  # Only count MPAs
      n_distinct(mpa_stages) >= 2  # At least 2 different MPAs
    }) %>%
    ungroup()
  toc() # 2 mins
  
  dat_MPAtoMPA # Check it
  
  # Check what I filtered out
  message("Original rows: ", nrow(dat_aggregated)) # Original rows: 381,749
  message("MPA-to-MPA rows: ", nrow(dat_MPAtoMPA)) # MPA-to-MPA rows: 332,233
  message("Filtered out: ", nrow(dat_aggregated) - nrow(dat_MPAtoMPA), 
          " (", round(100 * (1 - nrow(dat_MPAtoMPA)/nrow(dat_aggregated)), 1), "%)")  # Filtered out: 49,516 (13%)
  
  saveRDS(dat_MPAtoMPA, paste0(o_fol, "/corridor_strength_allESMs_sequence_MPAtoMPA.RDS"))
  

  
  
  
# Summary stats for the top X% most frequented sequences ----------------------------------
  
  ##** In case it is needed for in-text. Don't need this for the density heatmap corridor plots**
  dat_MPAtoMPA <- readRDS(paste0(o_fol, "/corridor_strength_allESMs_sequence_MPAtoMPA.RDS"))

  
  
  
# Get the top X% of trajectories by cumulative frequency -----------------------
      
  get_top_percent_sequences <- function(d, pct) {
    d %>%
      group_by(ssp, term) %>%
      arrange(desc(freq)) %>%
      mutate(
        cumulative_freq = cumsum(freq),
        total_freq = sum(freq),
        cumulative_pct = cumulative_freq / total_freq
      ) %>%
      filter(cumulative_pct <= pct) %>%
      dplyr::select(-cumulative_freq, -total_freq, -cumulative_pct) %>%
      ungroup()
  }
  
      
  ## Top 75% of sequences ------------
    sum_stats_75 <- get_top_percent_sequences(dat_MPAtoMPA, 0.75)
    sum_stats_75
    # saveRDS(sum_stats_75, paste0(o_fol, "/corridor_strength_allESMs_MPAtoMPA_top75pct.RDS"))
  
    
  ## Top 90% ** USED THIS** ------------
    sum_stats_90 <- get_top_percent_sequences(dat_MPAtoMPA, 0.90)
    sum_stats_90
    saveRDS(sum_stats_90, paste0(o_fol, "/corridor_strength_allESMs_MPAtoMPA_top90pct.RDS")) ##**USED THIS**
  
    
  ## Top 95% ------------
    sum_stats_95 <- get_top_percent_sequences(dat_MPAtoMPA, 0.95)
    sum_stats_95
    # saveRDS(sum_stats_95, paste0(o_fol, "/corridor_strength_allESMs_MPAtoMPA_top95pct.RDS"))
  
    
  ## Check how many sequences each represents ------------
    sum_stats_75 %>% count(ssp, term)
    sum_stats_90 %>% count(ssp, term)
    sum_stats_95 %>% count(ssp, term)
  
    

    

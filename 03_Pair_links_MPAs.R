# Get the stepwise pairings of MPAs for each trajectory — no non-MPAs (flagged as -999)
    # Written by Alice Pidd
        # Nov 2025


  # Uses the grid cell 
  # Gets the MPA pairings and counts the number of times they are linked.


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"

  
  
# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(disk, metric, "2_sequence")
  o_fol <- make_folder(disk, metric, "3_pairs_mpa")
  
  
  
# Compute link pairs between MPAs along each trajectory ------------------------
  
  get_from_to <- function(x) {
    d1 <- x %>%
      filter(MPA_ID > 0) %>% # No non-MPAs
      data.frame()
    n <- nrow(d1) # How many steps are in MPAs
    if(n > 0) {
      out <- tibble(traj_ID = d1$trajID[-n],
                    from = d1$MPA_ID[-n],
                    to = d1$MPA_ID[-1])
    } else {
      out <- tibble(traj_ID = NA,
                    from = NA,
                    to = NA)
    }
    return(out)
  }
  
  
  
  
  ## Do it to all the files -----------
  
    get_pairs <- function(f) {
      
      d <- readRDS(f) 
      esm <- basename(f) %>% str_split_i(., "_", 3)
      ssp <- basename(f) %>% str_split_i(., "_", 4)
      term <- basename(f) %>% str_split_i(., "_", 5)
      pairs_nm <- basename(f) %>%
        str_replace(., "sequence", "pairs_MPAs")
      
      if(file.exists(paste0(o_fol, "/", pairs_nm))) {
        message(paste0("⏭️  Skipping (already exists): ", basename(pairs_nm)))
        return(invisible(NULL))
      }

      message(paste0("🕒 Processing ", basename(f), " ..."))
      
      mpa_pairs <- d %>% 
        group_by(traj_ID) %>% 
        group_split() %>% # Split into list by group
        map(get_from_to) %>% # Do function
        list_drop_empty() %>% # Drop empty elements from a list
        bind_rows() %>% 
        group_by(from, to) %>% 
        count() %>% 
        data.frame()
      
      saveRDS(mpa_pairs, paste0(o_fol, "/", pairs_nm))
      
    }
    
    files <- dir(in_fol, full.names = TRUE)
    files  

    tic()
    walk(rev(files), get_pairs)
    toc() # several hours on the Big Mac
    beep(2)
  
  
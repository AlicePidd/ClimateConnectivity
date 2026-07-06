# Calculate trajectory residence time (months) for each MPA
    # Written by Alice Pidd and Jason Everett
        # Nov 2025


# Helpers ----------------------------------------------------------------------
 
  source("Helpers.R")
  metric <- "VoCCtracers"

  
  
  
# Folders ----------------------------------------------------------------------

  in_fol <- make_folder(source_disk, metric, "2_sequence")
  o_fol <- make_folder(source_disk, metric, "7_restime_esm")
  

  
  
# Get the trajectory residence time in each MPA --------------------------------
  
  get_restime_links <- function(f) {
    l <- readRDS(f)
    f_nm <- basename(f) %>% 
      str_replace(., "sequence", "res-time")
    
    if(file.exists(paste0(o_fol, "/", f_nm))) {
      message(paste0("⏭️  Skipping (already exists): ", basename(f_nm)))
      return(invisible(NULL))
    }
    
    message(paste0("🕒 Processing ", basename(f), " ..."))
    
    restime_links <- l %>% 
      group_by(traj_ID, MPA_ID) %>%
      summarise(residence_time = n(), .groups = "drop") %>% # Monthly residence time, out of total 240 points (20 years in months)
      filter(!is.na(MPA_ID)) %>% 
      left_join(st_drop_geometry(mpa_shp), by = "MPA_ID") %>%
      mutate(OBJECTID = if_else(MPA_ID == -999, -999, OBJECTID), # Fill non-mpa cells with -9999 across the board
             NETNAME = if_else(MPA_ID == -999, "Non-MPA", NETNAME),
             IUCN = if_else(MPA_ID == -999, "Non-MPA", IUCN)) %>% 
      dplyr::select(-AREA_KM2)
    
    saveRDS(restime_links, paste0(o_fol, "/", f_nm))
  }
  
  files <- dir(in_fol, full.names = TRUE)
  files

  tic()
  walk(rev(files), get_restime_links) 
  toc()
  beep(2)

  
  
  
# Get median trajectory residence times for each MPA_ID and save ---------------
  
  ## Get each of the above data files and join them into one df -----------
  
    files <- dir(o_fol, full.names = TRUE)
    files
    
    read_and_join <- function(f){
      d <- readRDS(f) %>% 
        mutate(ssp = basename(f) %>% str_split_i(., "_", 4),
               term = basename(f) %>% str_split_i(., "_", 5) %>% str_remove(., ".RDS"))
      return(d)
    }
    
    d_comb <- map(files, read_and_join) %>% 
      bind_rows()
    d_comb
    unique(d_comb$ssp)
    unique(d_comb$term)
    beep(2)
  
  
  ## Calculate the median res time for each MPA_ID -----------
  
    d_med <- d_comb %>% 
      filter(MPA_ID != -999) %>% 
      group_by(MPA_ID, ssp, term) %>% 
      summarise(med_restime = median(residence_time), .groups = "drop") %>% 
      left_join(d_comb, by = "MPA_ID", relationship = "many-to-many") %>% 
      dplyr::select(-ssp.y, -term.y) %>%
      mutate(ssp = as.factor(ssp.x),
             term = as.factor(term.x)) %>% 
      select(-ssp.x, -term.x) %>%
      group_by(ssp, term, MPA_ID) %>% # For every MPA_ID
      dplyr::select(-traj_ID, -residence_time) %>%
      distinct(MPA_ID, .keep_all = TRUE) # Remove duplicated rows (which were duplicated for all traj_IDs)
    d_med
  
  
  ## Get centroids for each MPA -----------
    
    cent <- mpa_shp %>% 
      st_centroid() %>%
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2],
        lon_cat = round(lon),
        lat_cat = round(lat)) %>%
      st_drop_geometry()
    
    head(cent)
    range(cent$MPA_ID)
  
  
  ## Join the lat and lon centroid bins to -----------
    
    dat <- left_join(d_med, cent, by = "MPA_ID") %>% 
      dplyr::select(-OBJECTID.y, -NETNAME.y, -IUCN.y, -AREA_KM2, -recalc_AREA_KM2.y) %>% 
      mutate(OBJECTID = OBJECTID.x,
             NETNAME = NETNAME.x,
             IUCN = IUCN.x, 
             recalc_AREA_KM2 = recalc_AREA_KM2.x,
             term = fct_relevel(term, "recent-term", "near-term", "mid-term", "intermediate-term", "long-term")) %>% 
      dplyr::select(-OBJECTID.x, -NETNAME.x, -IUCN.x, -recalc_AREA_KM2.x)
    dat
    unique(dat$term) # Check they're all there
    
    saveRDS(dat, paste0(o_fol, "/med_restime_MPA-ID_per_ssp-term-combo.RDS"))
  
  
  
  
# Separate out the sequence of MPA_IDs visited by each step along the trajectories
    # Written by Alice Pidd
        # Nov 2025


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"

  
  
# Folders ----------------------------------------------------------------------
  
  shps_fol <- make_folder(disk, "", "shapefiles")
  in_fol <- make_folder(disk, metric, "1_VoCC_tracers_cropped")
  seq_fol <- make_folder(disk, metric, "2_sequence")
  seq_sf_fol <- make_folder(disk, metric, "2_sequence_sf")
  
  
  
# Load data --------------------------------------------------------------------

  # mpa_grid <- readRDS(paste0(shps_fol, "/mpas_grid_joined.RDS")) # Grid for MPAs
  base_grid <- readRDS(paste0(shps_fol, "/base_polygonsf_grid_ID.RDS")) # Grid for whole extent

  # grid_id_rast <- terra::rasterize(vect(mpa_grid), base_r, field = "grid_ID", touches = TRUE)
  mpa_id_rast <- terra::rasterize(vect(mpa_shp), base_r, field = "MPA_ID", touches = TRUE)
  base_id_rast <- terra::rasterize(vect(base_grid), base_r, field = "grid_ID", touches = TRUE)
  

  
# Build trajectory sequences ---------------------------------------------------
  
  # Steps = monthly step for each point along a trajectory (max 240, unless it dies on land/too hot)
  # lon/lat = coords for each point
  # ID column = trajectory ID

  get_traj_sequence <- function(f){
    
    # Get bits of each filename
    esm <- basename(f) %>% str_split_i(., "_", 4)
    ssp <- basename(f) %>% str_split_i(., "_", 5)
    term <- basename(f) %>% str_split_i(., "_", 8) %>% paste0(., "-term")
    t <- readRDS(f) # Read in traj file
    coords <- as.matrix(t[, c("lon", "lat")]) # Get coords from the traj file
    
    o_nm1 <- paste0(seq_fol, "/traj_sequence_", esm, "_", ssp, "_", term, ".RDS")
    if(file.exists(o_nm1)) { # Check if file already exists
      message(paste0("⏭️  Skipping (already exists): ", basename(o_nm1)))
      return(invisible(NULL)) # If TRUE, it exits early without processing. If FALSE, code continues
    }
    
      seq <- t %>% 
        mutate(MPA_ID = terra::extract(mpa_id_rast, coords)[,1], # Extract the MPA_ID from the coords
               grid_ID = terra::extract(base_id_rast, coords)[,1], # Extract the grid_ID from the coords
               Time = Steps,
               traj_ID = ID,
               MPA_ID = ifelse(is.na(MPA_ID), -999, MPA_ID)) %>% # If MPA_ID is NA, make it -999, else make it MPA_ID
        select(traj_ID, MPA_ID, grid_ID, Time, lon, lat)
      saveRDS(seq, o_nm1)
    
    # Save it as an sf in case I need
    o_nm2 <- paste0(seq_sf_fol, "/traj_sequence_sf_", esm, "_", ssp, "_", term, ".RDS")
    if(file.exists(o_nm2)) { 
      message(paste0("⏭️  Skipping (already exists): ", basename(o_nm2)))
      return(invisible(NULL))
    }
    
      seq_sf <- seq %>%
        st_as_sf(coords = c("lon", "lat"), crs = st_crs(mpa_shp))
      saveRDS(seq_sf, o_nm2)
    
  }

  files <- dir(in_fol, pattern = ".RDS", full.names = TRUE)
  files
  # rev(files) # To do the files in reverse also for dual processing
  
  tic()
  walk(rev(files), get_traj_sequence) 
  toc() # 12 hours on the Big Mac
  beep(2) # 2hr 15min for 55 files on Alice's machine
  

    
# Check this worked ------------------------------------------------------------
  
  seq <- readRDS("/Volumes/AliceShield/conn_data/VoCCtracers/2_sequence/traj_sequence_ACCESS-CM2_ssp126_intermediate-term.RDS")
  seq_sf <- readRDS("/Volumes/AliceShield/conn_data/VoCCtracers/2_sequence_sf/traj_sequence_sf_ACCESS-CM2_ssp126_intermediate-term.RDS")
  
  
  ## Get the points in a single traj_id ----------
  
    points <- seq_sf %>% filter(traj_ID == 24245)
    cell <- mpa_grid %>% filter(grid_ID == 77122)
      
    points <- seq_sf %>% filter(traj_ID == 24246)
    cell <- mpa_grid %>% filter(grid_ID == 77124)

    
  ## Convert to linestring ----------
    
    string <- seq_sf %>%
      filter(traj_ID == 24245) %>% # Pick an example traj_ID
      st_combine() %>% # Combine the points to one line
      st_cast("LINESTRING") # Cast as a single linestring
    
    
  ## Plotting the grid cell, points, and MPA shapefile ----------
    
    ggplot() +
      geom_sf(data = mpa_shp,# | MPA_ID == 29), # Plot specific MPA_IDs
              fill = "blue", alpha = 0.5) +
      geom_sf(data = cell, # Plot specific grid_ID
              fill = "black", alpha = 0.2) +
      geom_sf(data = points, # Plot specific trajectory as points
              colour = "red",
              size = 0.5) +
      labs(x = "Longitude", y = "Latitude") +
      theme_bw()
  
  
  
  
# Building shapefiles and helper files
  # Written by Alice Pidd (alicempidd@gmail.com)
    # Sept 2025




# Get the things we need -------------------------------------------------------

  source("Helpers.R")

  shps_fol <- make_folder(disk, "", "shapefiles")
  helper_fol <- make_folder(disk, "VoCCtracers", "_helpers")
  stats_fol <- make_folder(disk, "VoCCtracers", "5_pairs_mpa_stats")
  

  
    
# Rasterize the MPAs to match the res of the trajectories (0.125°, ~150km^2) ---------

  values(base_r) <- 1 # Give the base raster made in the Helpers.R values of 1
    

  ## Rasterize the mpa shapefile to the base raster ----------

    m_rast <- rasterize(mpa_shp, base_r, 
                        field = "MPA_ID", # Makes the values of the raster the MPA_ID
                        touches = TRUE)
    m_rast
    plot(m_rast, main = "rasterized to coarse res 0.125°") # Check iyt
    saveRDS(m_rast, paste0(shps_fol, "/mpas_spatrast_MPA_ID.RDS"))
    
  
    
  ## Save rasterized mpas as polygons (sf) ----------

    # m_rast <- readRDS(paste0(con_shps_fol, "/mpas_spatrast.RDS"))
    m_rast <- readRDS(paste0(shps_fol, "/mpas_spatrast_MPA_ID.RDS"))
    m_rast
    m_sf <- as.polygons(m_rast, dissolve = FALSE) %>% # make them polygons
      st_as_sf() #%>% # Make it an sf object
      # select(-layer)
    m_sf
    saveRDS(m_sf, paste0(shps_fol, "/mpas_polygonsf_MPA_ID.RDS"))
    
  
    
  ## Assign grid_IDs to base raster ----------
    
    base_grid <- as.polygons(base_r, dissolve = FALSE) %>% # make them polygons
      st_as_sf() # Make it an sf object
    base_grid$grid_ID <- seq(1, nrow(base_grid)) # Number them
    base_grid
    saveRDS(base_grid, paste0(con_shps_fol, "/base_grid_polygonsf_grid_ID.RDS"))
    
    
    
  ## Extract the corresponding base grid grid_IDs for each MPA cell ----------
    
    m_sf <- readRDS(paste0(shps_fol, "/mpas_polygonsf.RDS")) # Get the MPAs as the polygon form of the 0.125° grid cells
    # m_sf <- readRDS(paste0(con_shps_fol, "/mpas_polygonsf_MPA_ID.RDS")) # Get the MPAs as the polygon form of the 0.125° grid cells
    m_sf_centroids <- st_centroid(m_sf) # Get centroids of each cell/polygon
    matched <- st_join(m_sf_centroids, base_grid[, "grid_ID"], join = st_within, left = TRUE) # Find the matching grid_ID corresponding to the base_grid
    m_sf$grid_ID <- matched$grid_ID # Add these on.
    m_sf # Check
    sum(is.na(m_sf$grid_ID)) # Should be 0 NAs
    plot(m_sf$geometry) # Check visually that it is the MPA grid cells/polygons
    nrow(m_sf) # 13249
    nrow(matched) # 13249, they match
    
    
    
  ## Join the mpa_shp details to each row of the mpa grid which now has a corresponding grid_ID to the base grid ----------
    
    m_grid_ID <- st_join(m_sf, mpa_shp, join = st_intersects, left = TRUE) %>%
      # .[-c(1)] %>% # Get rid of layer column
      dplyr::select(grid_ID, 
                    MPA_ID, NETNAME, IUCN, AREA_KM2, recalc_AREA_KM2, OBJECTID)
    m_grid_ID
    
    tm_shape(eez_shp) +
      tm_sf(fill = "white") +
    tm_shape(aus_shp) +
      tm_sf(fill = "grey80") +
    tm_shape(m_grid_ID) +
      tm_sf(fill = "black") +
    tm_shape(m_grid_ID %>% filter(MPA_ID == 81)) +
      tm_sf(fill = "red") +
      tm_borders(col = "red")
    
    
    nrow(m_grid_ID) # 14379
    length(unique(mpa_shp$MPA_ID)) # Original number of MPAs 474
    length(unique(m_grid_ID$MPA_ID)) # Also 474, although I didn't really expect this?
    
    saveRDS(m_grid_ID, paste0(con_shps_fol, "/mpas_grid_joined.RDS"))
    
    grid_mpa_IDs <- m_grid_ID %>%
        group_by(MPA_ID) %>% 
        distinct(grid_ID) %>% 
        count() %>% 
        arrange(desc(n))
    
    grid_mpa_IDs
    saveRDS(grid_mpa_IDs, paste0(helper_fol, "/mpaID_grid_cell_count.RDS"))
    
    
  
    
# Get number range for each MPA network ----------------------------------------
    
    mpa_grid_shp <- readRDS(paste0(con_shps_fol, "/mpas_grid_joined.RDS"))
    
    network_mpas <- mpa_shp %>%
      st_drop_geometry() %>%
      select(NETNAME, MPA_ID) %>%
      distinct() %>%
      group_by(NETNAME) %>%
      summarise(MPA_IDs = list(MPA_ID)) %>%
      deframe()  # Converts to named list
    
    names(network_mpas) <- c("CoralSea", "GBR", "North", "Northwest", "Southeast", "Southwest", "TemperateEast")
    
    saveRDS(network_mpas, paste0(helper_fol, "/MPA_ID_by_network.RDS"))
    readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))
    
  

  
# Create distance matrix -------------------------------------------------------
  
  # Jase's method of distance matrices
  alb_proj <- st_transform(mpa_shp, st_crs(5070)) # Reproject to Albers equal area proj
  centroids <- st_centroid(alb_proj) # Get centroids of each MPa
  distance_matrix <- centroids %>% 
    st_distance()/1000
  
  comp_mpa_distances <- function(mpa_shp) {
    
    alb_proj <- st_transform(mpa_shp, st_crs(5070)) # Reproject to Albers equal area proj
    centroids <- st_centroid(alb_proj) # Get centroids of each MPa
    mpa_numbers <- centroids$MPA_ID
    n_mpas <- length(mpa_numbers) # 474
    
    dist_matrix_units <- st_distance(centroids, centroids)
    dist_matrix <- as.numeric(dist_matrix_units)/1000 # Convert to numeric (removes units, so had to /1000 again) and convert to regular matrix
    dim(dist_matrix) <- c(n_mpas, n_mpas) # 474 474
    
    rownames(dist_matrix) <- as.character(mpa_numbers)
    colnames(dist_matrix) <- as.character(mpa_numbers)
    return(dist_matrix)
  }
  
  mpa_dist_matrix <- comp_mpa_distances(mpa_shp)
  mpa_dist_matrix
  saveRDS(mpa_dist_matrix, paste0(stats_fol, "/mpa_distance_matrix.RDS"))
  
  
  
  
  
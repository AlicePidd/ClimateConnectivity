# Building the MPA shapefiles
  # Written by Alice Pidd (alicempidd@gmail.com)
    # Sept 2025



## This script:
  # Sources a detailed Australia shapefile
  # Sources the federal MPA shapefile with all networks, cleans the columns, and filters out HIMI and IOT
  # Sources the GBR shapefile
  # Joins the federal and GBR shapefiles into one master shapefile
  # Cleans the master shapefile - excl. IUCN V and VI, and duplicate MPAs outside the contiguous coastal EEZ ( + LHI and Norfolk)
  # Numbers the MPAs in the master shapefile 1:nrow(), SAVES this as "mpas_joined_shapefile.RDS"
  # Transforms master shapefile to Australian Albers CRS, recalculates areas (km^2) (as the GBR didn't have area values) as a new field "recalc_AREA_KM2"
  # Transforms the shapefile back to EPSG:4326 (WGS 84) and saves as "mpas_joined_shapefile_newarea.RDS" 



  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/conn_data"
  
  con_shps_fol <- make_folder(source_disk, "", "shapefiles")
  helper_fol <- make_folder(source_disk, "VoCCtracers", "_helpers")
  stats_fol <- make_folder(source_disk, "VoCCtracers", "5_pairs_mpa_stats")
  


# Making an MPA shapefile with the GBR joined ----------------------------------

  e1 <- ext(105, 175, -50, -5) # Study extent
  base_r <- rast(ext = e1, res = 0.125) # Res of the fine scale trajectories
  
  
  ## Get the Aus shapefile with all islands (detailed) -----------

    # aus_shp_isl <- get_shps("/Volumes/AliceShield/shapefiles/AUS_2021_AUST_GDA94.shp") %>%
    #   dplyr::select(geometry) %>% # Select only necessary elements
    #   st_cast(., "POLYGON") # Turn the multipolygon into a polygon
    # saveRDS(aus_shp_isl, paste0(con_shps_fol, "/aus_shapefile_detailed.RDS"))

      # plot(aus_shp_isl$geometry)
      # plot(aus_shp$geometry)

    readRDS(aus_shp_isl, paste0(con_shps_fol, "/aus_shapefile_detailed.RDS"))
    

  ## Get the MPA shapefile that has all networks -----------

    mpas_all <- get_shps("/Volumes/AliceShield/shapefiles/Australian_Marine_Parks.shp") %>%
      filter(NETNAME != "Heard Island and McDonald Islands" & NETNAME != "Indian Ocean Territories") %>%
      mutate(OBJECTID = 1:nrow(.)) %>% # Make new column with rows numbered
      dplyr::rename("IUCN" = "ZONEIUCN") %>%
        dplyr::select(OBJECTID, NETNAME, IUCN, AREA_KM2, geometry) # select only these two columns
    mpas_all
    # ext(mpas_all)
    

  ## Get the GBR shapefile -----------

    GBR_shp <- st_read("/Volumes/AliceShield/shapefiles/Great_Barrier_Reef_Marine_Park_Zoning.shp") %>%
      mutate(NETNAME = "GBR") %>%
      mutate(AREA_KM2 = "Unknown") %>%
      dplyr::select(OBJECTID, NETNAME, IUCN, AREA_KM2, geometry) %>% # Select only necessary elements
      st_cast(., "POLYGON") # Turn the multipolygon into a polygon
    GBR_shp

    ext(GBR_shp) # xmin 142.5315, xmax 154.001, ymin -24.4985, ymax -10.68189, we're all good.
    GBR_shp <- sf::st_make_valid(GBR_shp) # There are some invalid values, need to make them valid
    sf::st_is_valid(GBR_shp) # Check it worked - if it did, there will be no FALSE's


  ## Join the MPA and GBR shapefiles together and save -----------

    joined_shps <- rbind(mpas_all, GBR_shp) %>%
      filter(IUCN != "V" & IUCN != "VI") %>% # Get rid of categories that allow for fishing
      mutate(IUCN = if_else(IUCN == "Ia", "IA", IUCN)) # Make naming consistent
    unique(joined_shps$NETNAME)
    
    # ggplot() +
    #   geom_sf(data = joined_shps, fill = "black", colour = "black", alpha = 0.5)
    
      # There are two big MPAs offshore in the south-east network that I don't want
    
    # Numbering the MPAs so I can ID these
    joined_shps$MPA_ID <- seq(1, nrow(joined_shps)) # Number them
    joined_shps
    nrow(joined_shps) # 476
    
    
    # Finding the MPA_NUMBERs of the southern MPAs
    # ggplot(data = joined_shps) +
    #   geom_sf() +
    #   geom_sf_text(aes(label = MPA_NUMBER, colour = NETNAME), 
    #                size = 3) +
    #   theme_minimal()
    
    # Getting rid of them.
    joined_shps <- joined_shps %>%
      filter(MPA_ID != "103" & MPA_ID != "104") # Get rid of categories that allow for fishing
    nrow(joined_shps) # 474
    
    # Checking
    # ggplot(data = joined_shps) +
    #   geom_sf() +
    #   geom_sf_text(aes(label = MPA_NUMBER, colour = NETNAME), 
    #                size = 3) +
    #   theme_minimal()
    
    # Re-numbering so the MPA_NUMBER gaps from the deleted MPAs are filled in
    joined_shps$MPA_ID <- seq(1, nrow(joined_shps)) # Number them
    joined_shps

    saveRDS(joined_shps, paste0(con_shps_fol, "/mpas_joined_shapefile.RDS")) # GBR and the federal networks joined
  

  ## Filling in missing MPA areas (the GBR is missing areas - "Unknown") -----------
    # Transform the shp to Australian Albers CRS (an equal-area projection) and recalculate the areas of each MPA
    
    ## Checking the range of areas in the original MPA shapefile --------
    range(joined_shps$AREA_KM2) # Includes "Unknown"s from the GBR
    
    joined_shps_newarea <- joined_shps %>%
      st_transform(., crs("EPSG:3577")) %>% # Used Australian Albers CRS
      dplyr::mutate(recalc_AREA_KM2 = round(st_area(.), 4)) %>% # st_area recalculation and rounding to nearest 4 decimal places
      dplyr::mutate(recalc_AREA_KM2 = as.numeric(units::set_units(recalc_AREA_KM2, "km^2"))) # Changes it to km2, so don't need to divide by 1000000
    joined_shps_newarea
    range(joined_shps_newarea$recalc_AREA_KM2) # No "Unknown"s
    
    joined_shps_newarea <- st_transform(joined_shps_newarea, crs("EPSG:4326")) %>% 
      mutate(NETNAME = str_replace_all(NETNAME, " ", ""),
             NETNAME = str_replace_all(NETNAME, "-", "")) # Make it so the NETNAMEs have no spaces in the name
    
    saveRDS(joined_shps_newarea, paste0(con_shps_fol, "/mpas_joined_shapefile_newarea.RDS"))
    
    # m <- readRDS(paste0(con_shps_fol, "/mpas_joined_shapefile_newarea.RDS"))
    # m %>% distinct(.$MPA_ID)
    
    
    
# Rasterize the MPAs to the res of the trajectories (0.125°, ~150km^2) ---------

  ## Base raster with the right res and ext ----------
    
    e1 <- ext(105, 175, -50, -5) # EXTENT WITHOUT COCOS KEELING/XMAS ISLAND
    base_r <- rast(ext = e1, res = 0.125, crs = "EPSG:4326") # Base raster with ext and res I want
    values(base_r) <- 1
    
    mpa_shp <- readRDS(paste0(con_shps_fol, "/mpas_joined_shapefile_newarea.RDS")) # Relatively detailed Australia shapefile (no islands e.g., K'Gari)
    mpa_shp
    aus_detailed_shp <- readRDS(paste0(con_shps_fol, "/aus_shapefile_detailed.RDS")) # Very detailed! Includes all islands offshore!
    eez_shp <- readRDS(paste0(con_shps_fol, "/EEZ_shapefile.RDS")) # EEZ outline
    
    
  
  ## Rasterize the mpa shapefile to the base raster ----------

    m_rast <- rasterize(mpa_shp, base_r, 
                        field = "MPA_ID", # Makes the values of the raster the MPA_ID
                        touches = TRUE)
    m_rast
    plot(m_rast, main = "rasterized to coarse res 0.125°") # Check iyt
    saveRDS(m_rast, paste0(con_shps_fol, "/mpas_spatrast_MPA_ID.RDS"))
    
  
    
  ## Save rasterized mpas as polygons (sf) ----------

    # m_rast <- readRDS(paste0(con_shps_fol, "/mpas_spatrast.RDS"))
    m_rast <- readRDS(paste0(con_shps_fol, "/mpas_spatrast_MPA_ID.RDS"))
    m_rast
    m_sf <- as.polygons(m_rast, dissolve = FALSE) %>% # make them polygons
      st_as_sf() #%>% # Make it an sf object
      # select(-layer)
    m_sf
    saveRDS(m_sf, paste0(con_shps_fol, "/mpas_polygonsf_MPA_ID.RDS"))
    
  
    
  ## Assign grid_IDs to base raster ----------
    
    base_grid <- as.polygons(base_r, dissolve = FALSE) %>% # make them polygons
      st_as_sf() # Make it an sf object
    base_grid$grid_ID <- seq(1, nrow(base_grid)) # Number them
    base_grid
    saveRDS(base_grid, paste0(con_shps_fol, "/base_grid_polygonsf_grid_ID.RDS"))
    
    
    
  ## Extract the corresponding base grid grid_IDs ffor each MPA cell ----------
    
    m_sf <- readRDS(paste0(con_shps_fol, "/mpas_polygonsf.RDS")) # Get the MPAs as the polygon form of the 0.125° grid cells
    # m_sf <- readRDS(paste0(con_shps_fol, "/mpas_polygonsf_MPA_ID.RDS")) # Get the MPAs as the polygon form of the 0.125° grid cells
    m_sf_centroids <- st_centroid(m_sf) # Get centroids of each cell/polygon
    matched <- st_join(m_sf_centroids, base_grid[, "grid_ID"], join = st_within, left = TRUE) # Find the matching grid_ID corresponding to the base_grid
    m_sf$grid_ID <- matched$grid_ID # Add these on.
    m_sf # Check
    sum(is.na(m_sf$grid_ID)) # Should be 0 NAs
    plot(m_sf$geometry) # Check visually that it is the MPA grid cells/polygons
    nrow(m_sf) # 13249
    nrow(matched) # 13249 They match!!
    
    
    
  ## Join the mpa_shp details to each row of the mpa grid which now has a corresponding grid_ID to the base grid ----------
    
    m_grid_ID <- st_join(m_sf, mpa_shp, join = st_intersects, left = TRUE) %>%
      # .[-c(1)] %>% # Get rid of layer column
      select(grid_ID, MPA_ID, NETNAME, IUCN, AREA_KM2, recalc_AREA_KM2, OBJECTID)
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
    
  
# Look at shapefiles  ----------------------------------------------------------
  
  aus_detailed_shp <- readRDS(paste0(con_shps_fol, "/aus_shapefile_detailed.RDS")) # Very detailed! Includes all islands offshore!
  aus_shp <- readRDS(paste0(con_shps_fol, "/aus_shapefile.RDS")) # Very detailed! Includes all islands offshore!
  eez_shp <- readRDS(paste0(con_shps_fol, "/EEZ_shapefile.RDS")) # EEZ outline

  tm_shape(eez_shp) +
    tm_sf(fill = "white") +
  # tm_shape(aus_shp) +
  #   tm_sf(fill = "grey80") +
  # tm_shape(m_grid_ID) +
  #   tm_sf(fill = "red") +
  #   tm_borders(col = "black") +
  # tm_shape(m_rast) +
  #   tm_raster()
  tm_shape(mpa_shp %>% filter(MPA_ID %in% network_mpas$GBR)) +
    tm_sf(fill = "orange") +
    tm_borders(col = "orange")
  

  
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
  
  
  
  
# Stanford detailed Oceania shapefile ------------------------------------------
  
  stan_shp <- st_read("/Volumes/AliceShield/clim_data/shapefiles/stanford_oceania.shp") 
  unique(stan_shp$name)
  
  stan_shp <- stan_shp %>% 
    filter(name == "AUSTRALIA" | name == "NEW ZEALAND" | name == "INDONESIA" | name == "PAPUA NEW GUINEA" | name == "VANUATU" | name == "New Caledonia (FRANCE)" | name == "SOLOMAN ISLANDS" | name == "Norfolk Island (AUSTRALIA)" | name == "Christmas Island (AUSTRALIA)" | name == "TIMOR-LESTE") %>% 
    st_crop(., ext(base_r))
  plot(stan_shp$geometry)
  saveRDS(stan_shp, paste0(con_shps_fol, "/oceania_shapefile.RDS"))
  
  
  
  
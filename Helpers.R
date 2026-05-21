# Helper functions: Climate connectivity
	# For working with climate velocity trajectories
  	# Written by Alice Pidd, Feb 2025


# renv::dependencies("/Users/alicepidd/Documents/PhD/code/ClimateConnectivity")

  disk <- "/Volumes/AliceShield/conn_data"


# Packages ---------------------------------------------------------------------

# pacman::p_load(
#   dplyr, renv, sf, units, terra, 
# )

pacman::p_load(pacman, tidyverse, purrr, furrr, ncdf4, terra, sf, tmap, beepr, tictoc, viridis, viridisLite, parallel, patchwork, ggrepel, rmapshaper, lwgeom, progressr, data.table, vegan, vctrs, gplots, patchwork, mgcv, mgcViz, glmmTMB, marginaleffects, gratia, geosphere, igraph, ggraph, parallelly, rcartocolor, scico, NatParksPalettes, ggalluvial, MoMAColors, LaCroixColoR, rcartocolor, MetBrewer, MexBrewer, ghibli, ggthemes, feathers, futurevisions, ltc, paletteer, gglm, boot, GGally, MASSExtra, visreg, MoMAColors, khroma, PNWColors, patchwork, marginaleffects, 
               glmmTMB, sdmTMB, gstat, spdep, DHARMa, performance, inlabru, modelbased # From the R workshop on spatiotemporal autocorrelation
               )


# Functions --------------------------------------------------------------------
  
  ## To make folders at start of each script --------------------
  
  	make_folder <- function(d, m, fol_dir_name) {
  	  
  	  folder_path <- file.path(paste0(d, "/", m, "/", fol_dir_name))
  	  if (!dir.exists(folder_path)) {
  	    dir.create(folder_path, recursive = TRUE)
  	    message("✅ Folder created: ", folder_path)
  	  } else {
  	    message("📂 Folder already exists: ", folder_path)
  	  }
  	  return(folder_path)
  	}
	

  # Shapefiles - get, transform projection, crop --------------------

  	get_shps <- function(shp_dir){
  	  shp <- st_read(shp_dir) %>%
  	    sf::st_transform(4326) %>%
  	    sf::st_crop(ext(base_r))
  	}



  
# Source folders, shapefiles, and helper files ---------------------------------

  ## Folders --------------------
  	
    shps_fol <- make_folder(disk, "", "shapefiles")
    helper_fol <- make_folder(disk, "VoCCtracers", "_helpers")

    
  ## Shapefiles --------------------
    
    e1 <- ext(105, 175, -50, -5) # EXTENT WITHOUT COCOS KEELING/XMAS ISLAND
    base_r <- rast(ext = e1, res = 0.125, crs = "EPSG:4326") 
    
    aus_detailed_shp <- readRDS(paste0(shps_fol, "/aus_shapefile_detailed.RDS")) # Very detailed! Includes all islands offshore!
    oceania_stanford_shp <- readRDS(paste0(shps_fol, "/oceania_shapefile.RDS"))
    eez_shp <- readRDS(paste0(shps_fol, "/EEZ_shapefile.RDS"))
    mpa_shp <- readRDS(paste0(shps_fol, "/mpas_joined_shapefile_newarea.RDS"))
    
    reez <- terra::rasterize(eez_shp, base_r) # Base raster for EEZ, land not masked out
    rmpa <- terra::rasterize(mpa_shp, base_r, touches = TRUE) # Base raster for all MPAs, including GBR, land masked out
    # raus <- terra::rasterize(aus_detailed_shp, base_r, touches = TRUE)
    
    
  ## Helper files --------------------
    
    base_grid <- readRDS(paste0(shps_fol, "/base_polygonsf_grid_ID.RDS"))
    mpa_ranges <- readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))
    network_mpas <- readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))
  
  
  
  
# Lists ------------------------------------------------------------------------
  
  ssp_list <- c("ssp126", "ssp245", "ssp370", "ssp585")
  term_list <- c("recent", "near", "mid", "intermediate", "long")




# Palettes ----------------------------------------------------------------

  IPCC_pal <- c("ssp126" = rgb(0, 52, 102, maxColorValue = 255), 
                "ssp245" = rgb(112, 160, 205, maxColorValue = 255), 
                "ssp370" = rgb(196, 121, 0, maxColorValue = 255), 
                "ssp585" = rgb(153, 0, 2, maxColorValue = 255))




		
		
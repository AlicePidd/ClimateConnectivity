# Helper functions: Climate connectivity
	# For working with climate velocity trajectories
  	# Written by Alice Pidd, May 2026


# renv::dependencies("/Users/alicepidd/Documents/PhD/code/ClimateConnectivity")


# Packages ---------------------------------------------------------------------

# pacman::p_load(
#   dplyr, renv, sf, units, terra, 
# )

pacman::p_load(pacman, tidyverse, purrr, furrr, ncdf4, terra, sf, tmap, beepr, tictoc, viridis, viridisLite, parallel, patchwork, ggrepel, rmapshaper, lwgeom, progressr, data.table, vegan, vctrs, gplots, patchwork, mgcv, mgcViz, glmmTMB, marginaleffects, gratia, geosphere, igraph, ggraph, parallelly, rcartocolor, scico, NatParksPalettes, ggalluvial, MoMAColors, LaCroixColoR, rcartocolor, MetBrewer, MexBrewer, ghibli, ggthemes, feathers, futurevisions, ltc, paletteer, gglm, boot, GGally, MASSExtra, visreg, MoMAColors, khroma, PNWColors, patchwork, marginaleffects, 
               glmmTMB, sdmTMB, gstat, spdep, DHARMa, performance, inlabru, modelbased # From the R workshop on spatiotemporal autocorrelation
               )



# Palettes ----------------------------------------------------------------

  IPCC_pal <- c("ssp126" = rgb(0, 52, 102, maxColorValue = 255), 
                "ssp245" = rgb(112, 160, 205, maxColorValue = 255), 
                "ssp370" = rgb(196, 121, 0, maxColorValue = 255), 
                "ssp585" = rgb(153, 0, 2, maxColorValue = 255))




# Folder function -------------------------------------------------------------
  	
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
	
	

	
# Shapefiles - get, transform projection, crop  --------------------------------

	get_shps <- function(shp_dir){
	  shp <- st_read(shp_dir) %>%
	    sf::st_transform(4326) %>%
	    sf::st_crop(ext(base_r))
	}


		
		
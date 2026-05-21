# Background data
  # Written by Alice Pidd (alicempidd@gmail.com)
    # May 2023 for Chp2, re-written December 2024 for Chp3 (climate connectivity)



# Source the helpers and necessary bits -----------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/conn_data"
  
  shps_fol <- make_folder(source_disk, "", "shapefiles")
  # con_shps_fol <- make_folder(source_disk, "", "shapefiles")
  helper_fol <- make_folder(source_disk, "VoCCtracers", "_helpers")
  
  
  
# Source data and shapefiles  --------------------------------------------------
  
  e1 <- ext(105, 175, -50, -5) # EXTENT WITHOUT COCOS KEELING/XMAS ISLAND
  base_r <- rast(ext = e1, res = 0.125) # Res of the new tracers
  
  ssp_list <- c("ssp126", "ssp245", "ssp370", "ssp585")
  term_list <- c("recent", "near", "mid", "intermediate", "long")
  
  
  
# Source shapefiles  -----------------------------------------------------------
  
  aus_detailed_shp <- readRDS(paste0(shps_fol, "/aus_shapefile_detailed.RDS")) # Very detailed! Includes all islands offshore!
  oceania_stanford_shp <- readRDS(paste0(shps_fol, "/oceania_shapefile.RDS"))
  eez_shp <- readRDS(paste0(shps_fol, "/EEZ_shapefile.RDS"))
  mpa_shp <- readRDS(paste0(shps_fol, "/mpas_joined_shapefile_newarea.RDS"))

  # raus <- terra::rasterize(aus_detailed_shp, base_r, touches = TRUE)
  reez <- terra::rasterize(eez_shp, base_r) # Base raster for just MPAs
  rmpa <- terra::rasterize(mpa_shp, base_r, touches = TRUE) # Base raster for all MPAs, including GBR

  base_grid <- readRDS(paste0(shps_fol, "/base_polygonsf_grid_ID.RDS"))
  
  mpa_ranges <- readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))
  network_mpas <- readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))


  
  
  
  

  
# Palettes for plotting with consistency ---------------------------------------
  
  # master_mpa_palette <- readRDS("/Volumes/AliceShield/conn_data/master_mpa_palette.RDS")
  # master_network_palette <- readRDS("/Volumes/AliceShield/conn_data/master_network_palette_Ohchi-ish.RDS")
  
  
  # ## Individual MPA numbers -------
  #
  # create_master_mpa_palette <- function(max_mpa) {
  #   all_colors <- purrr::map(MoMAColors::colorblind_moma_palettes,# Get all MoMA colours
  #                            ~MoMAColors::moma.colors(.x, n = 100)) %>%
  #     purrr::flatten_chr() %>%
  #     unique()
  #
  #   while(length(all_colors) < max_mpa) { # Ensures we have enough colours by repeating if necessary
  #     all_colors <- c(all_colors, all_colors)
  #   }
  #
  #   set.seed(42) # Set seed for reproducibility
  #   random_colors <- sample(all_colors, max_mpa) # Random sample without replacement to ensure all colours are different. Maximizes the difference between adjacent MPA numbers
  #   mpa_palette <- random_colors  # Make a named vector, with MPA numbers as names
  #   names(mpa_palette) <- 1:max_mpa
  #   return(mpa_palette)
  # }
  #
  # master_mpa_palette <- create_master_mpa_palette(462)
  # saveRDS(master_mpa_palette, paste0("/Volumes/AliceShield/conn_data/master_mpa_palette_random.RDS"))



  ## Networks -------

  # MoMAColors::colorblind_moma_palettes # I like ustwo, Levine2, Exter and Alkalay1 and 2 (for heatmaps), Ohchi, VanGogh,
  # [1] "Alkalay1"     "Alkalay2"     "Althoff"      "Andri"        "Connors"      "Doughton"     "Ernst"        "Exter"
  # [9] "Flash"        "Fritsch"      "Kippenberger" "Koons"        "Levine2"      "Ohchi"        "OKeeffe"      "Palermo"
  # [17] "Picabia"      "Picasso"      "Rattner"      "Sidhu"        "Smith"        "ustwo"        "VanGogh"      "vonHeyl"
  #
  # net_colours <- c( # coolourspal
  #   "#e76f51", # North west
  #   "#264653", # GBR
  #   "#2a9d8f", # South west
  #   "#8ab17d", # Temp east
  #   "#f4a261", # North
  #   "#e9c46a", # South west
  #   "#287271" # South east
  # )
  # 
  # net_colours <- c( # otherpal
  #   "#440505", # North west
  #   "#163c3c", # GBR
  #   "#A8C545", # South west
  #   "#0092B2", # Temp east
  #   "#c55c3c", # North
  #   "#989680", # South west
  #   "#E2A72E" # South east
  # )

  # net_colours <- c( # Ohchi-ish
  #   "#445467",
  #   "#B6282A",
  #   "#f4a261",
  #   "#B9C78E",
  #   "#582851",
  #   "#B3864D",
  #   "#126782"
  # )
  # 
  # ggplot() +
  #   geom_sf(data = joined_shps,
  #           aes(fill = NETNAME),
  #           lwd = 0) +
  #   scale_fill_manual(values = net_colours) +
  #   theme_minimal()
  # 
  # 
  # MPA_ranges <- readRDS(paste0(plots_fol, "/MPA_numbering_ranges_by_network.RDS"))
  # MPA_ranges
  # 
  # names(net_colours) <- unique(IUCN_MPA_shps$NETNAME)
  # saveRDS(net_colours, paste0("/Volumes/AliceShield/conn_data/master_network_palette_otherpal.RDS"))


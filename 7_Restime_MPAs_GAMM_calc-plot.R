# Plotting hexplot of observed relationship between latitude, MPA area, and residence time 
    # Written by Alice Pidd
        # Jan 2026


# Helpers ----------------------------------------------------------------------
 
  source("Helpers.R")
  metric <- "VoCCtracers"

  
  
# Folders and data -------------------------------------------------------------

  in_fol <- make_folder(disk, metric, "7_restime_esm")
  o_fol <- make_folder(disk, metric, "8_restime_plot")
  
  # Data from the calc script
  dat <- readRDS(paste0(in_fol, "/med_restime_MPA-ID_per_ssp-term-combo.RDS")) %>% 
    mutate(log10_recalc_AREA_KM2 = log10(recalc_AREA_KM2 + 1)) %>% # Plus 1 to the log to avoid negatives
    rename(rep_period = term)
  head(dat)

  
  
  
# Plot observed relationship between area * latitude * restime -----------------
  
  labels <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5") %>% 
    as.vector()
  labels

  ggplot() +
    stat_summary_hex(data = dat, 
                     aes(x = log10_recalc_AREA_KM2, y = med_restime, z = lat), 
                     fun = median, # Can also use max, but median works fine
                     bins = 50) +
    scale_fill_continuous(palette = hexpal, name = "Latitude (°S)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 14), 
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10)) +
    facet_grid(~ ssp) +
    labs(x = "MPA area (km2, log10-transformed)", y = "Residence time (months)",
         title = "Relationship between residence time, MPA area, and latitude")

  ggsave(paste0(o_fol, "/restime_area_latitude-col_hexbin_plot_rev.pdf"), width = 15, height = 4)
  
  
  
  ## As above but with axes switched --------------
  
    ggplot() +
      stat_summary_hex(data = dat, 
                       aes(x = log10_recalc_AREA_KM2, y = lat, z = med_restime), 
                       fun = median, # Can also use max, but median works fine
                       bins = 30) +
      scale_fill_continuous(palette = hexpal, name = "Residence\ntime\n(months)") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(size = 14), 
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10)) +
      facet_grid(~ ssp) +
      labs(x = "MPA area (km2, log10-transformed)", y = "Latitude (°S)",
           title = "Relationship between residence time, MPA area, and latitude")
    
    ggsave(paste0(o_fol, "/restime_latitude_area_hexbin_plot_rev.pdf"), width = 15, height = 4)
  
  
  

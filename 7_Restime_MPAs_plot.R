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

  med_dat <- dat %>% 
    group_by(ssp) %>% 
    reframe(mres = median(med_restime/12),
            q1 = quantile(med_restime/12, 0.25),
            q3 = quantile(med_restime/12, 0.75))
  
  
  
# Plot observed relationship between area * latitude * restime -----------------
  
  labels <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5") %>% 
    as.vector()
  labels

  ggplot() +
    stat_summary_hex(data = dat, 
                     aes(x = log10_recalc_AREA_KM2, y = med_restime / 12, z = lat), 
                     fun = median, # Can also use max, but median works fine
                     bins = 50) +
    geom_rect(data = med_dat,
              aes(xmin = -Inf, xmax = Inf, ymin = q1, ymax = q3),
              fill = "grey50", alpha = 0.2, inherit.aes = FALSE) +
    geom_hline(data = med_dat,
               aes(yintercept = mres), # horizontal line for median residence times per SSP
               linewidth = 0.5,
               linetype = "dashed",
               alpha = 0.8) +
    geom_text(data = med_dat,
              aes(x = Inf, y = mres, label = round(mres, 2)), # geom_hlines labelled with value
              hjust = 1.1, vjust = -0.5,
              size = 3) +
    scale_fill_continuous(palette = hexpal, name = "Latitude (°S)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 14), 
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    facet_grid(~ ssp) +
    labs(x = "MPA area (km2, log10-transformed)", y = "Residence time (years)",
         title = "Relationship between residence time, MPA area, and latitude")

  ggsave(paste0(o_fol, "/restime_area_latitude-col_hexbin_plot_rev_hline-median.pdf"), width = 15, height = 4)
  
  
  
    # ## As above but with axes switched --------------
    # 
    #   ggplot() +
    #     stat_summary_hex(data = dat, 
    #                      aes(x = log10_recalc_AREA_KM2, y = lat, z = med_restime), 
    #                      fun = median, # Can also use max, but median works fine
    #                      bins = 30) +
    #     scale_fill_continuous(palette = hexpal, name = "Residence\ntime\n(months)") +
    #     theme_minimal(base_size = 12) +
    #     theme(plot.title = element_text(size = 14), 
    #           axis.text.y = element_text(size = 10),
    #           axis.text.x = element_text(size = 10)) +
    #     facet_grid(~ ssp) +
    #     labs(x = "MPA area (km2, log10-transformed)", y = "Latitude (°S)",
    #          title = "Relationship between residence time, MPA area, and latitude")
    #   
    #   ggsave(paste0(o_fol, "/restime_latitude_area_hexbin_plot_rev.pdf"), width = 15, height = 4)
  
  
  
    
# Plot ridgeline density plot (restime under each term-SSP) --------------------
    
  ## Get data in right format --------------
  
    temporal_plot_data <- dat %>%
      filter(!is.na(rep_period), !is.na(ssp), !is.na(med_restime)) %>%
      filter(rep_period != "NA") %>%
      mutate(
        term_label = factor(rep_period,
                            levels = c("near-term", "mid-term", "intermediate-term", "long-term"),
                            labels = c("Near-term\n(2021-2040)", "Mid-term\n(2041-2060)", 
                                       "Intermediate-term\n(2061-2080)", "Long-term\n(2081-2100)")),
        # Keep original ssp for colors
        ssp = factor(ssp, levels = c("ssp585", "ssp370", "ssp245", "ssp126"))  # Reversed order so SSP585 is at back
      ) %>%
      filter(!is.na(term_label))
      
      med_temporal_plot_data <- temporal_plot_data %>% 
        group_by(term_label, ssp) %>% 
        reframe(mres = median(med_restime/12))
      med_temporal_plot_data
    
    
      
  ## Plot  --------------
      
    ggplot(temporal_plot_data, aes(x = med_restime / 12, fill = ssp, group = ssp)) +
      geom_density(adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
      geom_vline(data = med_temporal_plot_data,
                 aes(xintercept = mres, colour = ssp), # horizontal line for median residence times per SSP
                 linewidth = 0.5,
                 linetype = "dashed") +
      geom_text(data = med_temporal_plot_data,
                aes(y = Inf, x = mres, label = round(mres, 2)), # geom_hlines labelled with value
                hjust = 1.1, vjust = -0.5,
                size = 3) +
      facet_wrap(~term_label, nrow = 1) +
      scale_fill_manual(values = IPCC_pal,  # Original palette
                        labels = c("SSP5-8.5", "SSP3-7.0", "SSP2-4.5", "SSP1-2.6"),
                        name = "SSP") +
      scale_color_manual(values = IPCC_pal,  # Original palette
                        labels = c("SSP5-8.5", "SSP3-7.0", "SSP2-4.5", "SSP1-2.6"),
                        name = "SSP") +
      scale_y_continuous(expand = c(0, 0)) +
      theme_minimal(base_size = 11) +
      theme(strip.text = element_text(size = 10),
            panel.grid.minor = element_blank(),
            legend.position = "bottom") +
      labs(x = "Median residence time (years)",
           title = "Temporal patterns in trajectory residence time under climate futures")
    
    ggsave(paste0(o_fol, "/restime_density_plot_vline-median.pdf"), width = 10, height = 4, dpi = 300)
  

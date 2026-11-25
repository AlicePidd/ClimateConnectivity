# Ensemble direction of climate velocity (Chapter 3, Supp figure for E1, comment 5)
  # For each SSP-term combo, calculates the circular mean of voccAng across ESMs per 1° grid cell, plotted as arrows
  # Computes also the 0-1 measure of how closely the ESMs agree on direction in each cell
  # Computes the ensemble median climatology to plot over.
    # Written by Alice Pidd, Nov 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"
  

  
# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(disk, metric, "0_VoCC_mag_layers") 
  o_fol <- make_folder(disk, metric, "20_VoCCAng_plot")
  
  thin_every <- 4 # 4 x 0.25° = one arrow per 1°
  arrow_len  <- 0.6 # arrow length in degrees
  add_isotherms <- FALSE # set FALSE for direction only

  mag_mult <- 1 # For if need to make decadal
  
  # Labels named by term_list (recent, near, mid, intermediate, long)
  term_labels <- set_names(c("Recent past (1995–2014)",
                             "Near-term (2021–2040)",
                             "Mid-term (2041–2060)",
                             "Intermediate-term (2061–2080)",
                             "Long-term (2081–2100)"),
                           term_list)
  
  ssp_labels <- c(ssp126 = "SSP1–2.6", ssp245 = "SSP2–4.5",
                  ssp370 = "SSP3–7.0", ssp585 = "SSP5–8.5")
  
  

# File table -------------------------------------------------------------------

  file_df <- tibble(f = dir(in_fol, full.names = TRUE) %>%
                      str_subset(., "present", negate = TRUE) %>%
                      str_subset(., "CESM2-WACCM", negate = TRUE)) %>%
    mutate(base = basename(f),
           esm  = str_split_i(base, "_", 3),
           ssp  = str_split_i(base, "_", 4),
           term = str_extract(base, "recent_past|near_term|mid_term|intermediate_term|long_term") %>%
             str_remove("_past|_term")) # Short names to match term_list

  # Checking ESMs per SSP-term combo (methods say 11) and that no term is NA
  file_df %>% count(ssp, term) %>% print(n = 20) # Yep, 11. Excluding CESM2-WACCM

  
  

# Ensemble direction for a set of files ----------------------------------------
  # Angles can't be averaged directly (mean of 350° and 10° is not 180°), so have to av the sin and cos across ESMs and convert back (circular mean).
  # R (0-1) = ESM agreement in direction: 1 = all ESMs point the same way.
  
  ens_direction <- function(fs) {
    rs <- map(fs, ~ readRDS(.x) %>%
                crop(e1) %>%
                mask(land_mask))
    
    rad <- rast(map(rs, ~ .x[["voccAng"]] * pi / 180))
    s <- app(sin(rad), mean, na.rm = TRUE)
    c_ <- app(cos(rad), mean, na.rm = TRUE)
    ang <- (atan2(s, c_) * 180 / pi) %% 360
    R <- sqrt(s^2 + c_^2)
    clim <- app(rast(map(rs, ~ .x[["climatology"]])), median, na.rm = TRUE)
    mag <- app(rast(map(rs, ~ .x[["voccMag"]])), median, na.rm = TRUE) * mag_mult
    
    out <- c(ang, R, clim, mag)
    names(out) <- c("ang", "R", "climatology", "mag")
    out
  }
  
  


# Compute all ensembles first (so colour limits can be shared) -----------------

  # Recent past: historical runs are identical across SSPs, so use one file per ESM
  recent_files <- file_df %>%
    filter(term == "recent") %>%
    distinct(esm, .keep_all = TRUE) %>% # only need one of each, they're all the same per each ESM
    pull(f)
  
  combos <- file_df %>%
    filter(term != "recent") %>%
    distinct(ssp, term) %>%
    arrange(ssp, factor(term, levels = term_list))
  
  plot_df <- bind_rows(tibble(ssp = NA_character_, term = "recent"), combos) %>%
    mutate(files = map2(ssp, term, function(s, t) {
      if (t == "recent") recent_files else file_df %>% filter(ssp == s, term == t) %>% pull(f)
    }))
  
  tic()
  plot_df <- plot_df %>%
    mutate(r = map(files, ens_direction))
  toc() # 16 sec on Alice's machine
  
  map_int(plot_df$r, nlyr) # Should all be 4

  
  
# Shared colour limits ---------------------------------------------------------
  # Symmetric 5th-95th percentile limits across all panels, so 0 sits at the centre and colours are comparable

  q <- plot_df$r %>%
    map(~ values(.x[["mag"]], na.rm = TRUE)) %>%
    unlist() %>%
    quantile(c(0.05, 0.95))
  mag_lim <- max(abs(q))
  mag_lim # 21.69117


  
  
# Plot -------------------------------------------------------------------------

  blueyellow_pal1
  vel_pal <- c(#"#2D349F", 
               "#192E84", "#2849CC", "#2BA8DE", "#7DDEC7", "#E0E0E0", #"#C0CF55", "#FFD633", 
               "#FFDE5C", "#FFC300", "#EE9B00", "#C34E00")
  
  
  plot_direction <- function(r, title) {
    df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    
    arrows <- df %>%
      filter(round((x - min(x)) / 0.25) %% thin_every == 0,
             round((y - min(y)) / 0.25) %% thin_every == 0) %>%
      mutate(rad = ang * pi / 180,
             dx  = arrow_len * sin(rad),   # assumes angle clockwise from north (0 = N, 90 = E)
             dy  = arrow_len * cos(rad))
    
    p <- ggplot() +
      geom_tile(data = df %>% mutate(mag = pmax(pmin(mag, mag_lim), -mag_lim)),
                aes(x, y, fill = mag), width = 0.25, height = 0.25) +
      # scale_fill_gradient2(low = "dodgerblue2", mid = "#F0F0F0", high = "#B2182B",
      #                      midpoint = 0, limits = c(-mag_lim, mag_lim),
      #                      name = "Climate velocity\n(km decade-1)")
      scale_fill_gradientn(colours = vel_pal,   # 9 colours, grey (5th) sits at 0 because limits are symmetric
                           limits = c(-mag_lim, mag_lim),
                           name = "Climate velocity\n(km decade-1)")

    if (add_isotherms) {
      p <- p + geom_contour(data = df, aes(x, y, z = climatology),
                            colour = "white", binwidth = 2, linewidth = 0.25)
    }
    
    p +
      geom_sf(data = mpa_shp, fill = "grey10", linewidth = NA, alpha = 0.25) + # outlines so velocity colour stays visible inside MPAs
      # geom_sf(data = oceania_stanford_shp, fill = "#404F4B", colour = NA) +
      geom_sf(data = oceania_stanford_shp, fill = "white", colour = NA) +
      geom_sf(data = eez_shp, fill = NA, colour = "black", linewidth = 0.25) +
      geom_segment(data = arrows,
                   aes(x = x, y = y, xend = x + dx, yend = y + dy),
                   arrow = arrow(length = unit(0.06, "cm")), linewidth = 0.1) +
      coord_sf(xlim = c(105, 175), ylim = c(-50, -5)) +
      theme_void(base_size = 10) +
      labs(title = title)
  }
  



  # Run --------------------------------------------------------------------------
  
  pwalk(list(plot_df$ssp, plot_df$term, plot_df$r), function(s, t, r) {
    title <- if (t == "recent") term_labels[[t]] else paste0(ssp_labels[[s]], ", ", term_labels[[t]])
    fname <- if (t == "recent") "vocc_direction_recent.pdf" else paste0("vocc_direction_", s, "_", t, ".pdf")
    message("Plotting: ", title)
    ggsave(file.path(o_fol, fname), plot_direction(r, title), width = 7, height = 5)
  })
  
  beep(2)
  

  

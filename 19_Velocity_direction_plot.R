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
  
  thin_every <- 4                        # 4 x 0.25° = one arrow per 1°
  arrow_len  <- 0.6                      # arrow length in degrees
  add_isotherms <- TRUE                  # set FALSE for direction only



# File table -------------------------------------------------------------------

  file_df <- tibble(f = dir(in_fol, full.names = TRUE) %>% 
                      str_subset(., "present", negate = TRUE) %>% 
                      str_subset(., "CESM2-WACCM", negate = TRUE)) %>%
    mutate(base = basename(f),
           esm  = str_split_i(base, "_", 3),
           ssp  = str_split_i(base, "_", 4),
           term = str_extract(base, "recent_past|near_term|mid_term|intermediate_term|long_term"))

  # Checking ESMs per SSP-term combo (methods say 11)
  file_df %>% count(ssp, term) %>% print(n = 20) # Yep, 11. Excluding CESM2-WACCM

file_df[[1]][1]

# Ensemble direction for a set of files ----------------------------------------
  # Angles can't be averaged directly (mean of 350° and 10° is not 180°), so have to av the sin and cos across ESMs and convert back (circular mean).
  # R (0-1) = ESM agreement in direction: 1 = all ESMs point the same way.
  
  ens_direction <- function(fs) {
    rs <- map(fs, ~ readRDS(.x) 
              %>% crop(e1) %>% 
                mask(land_mask))

    rad   <- rast(map(rs, ~ .x[["voccAng"]] * pi / 180))
    s     <- app(sin(rad), mean, na.rm = TRUE)
    c_    <- app(cos(rad), mean, na.rm = TRUE)
    ang   <- (atan2(s, c_) * 180 / pi) %% 360
    R     <- sqrt(s^2 + c_^2)
    clim  <- app(rast(map(rs, ~ .x[["climatology"]])), median, na.rm = TRUE)

    out <- c(ang, R, clim)
    names(out) <- c("ang", "R", "climatology")
    out
  }




  clim_lims <- file_df$f %>%
    map(~ readRDS(.x) %>% crop(e1) %>% mask(land_mask) %>% .[["climatology"]] %>%
          minmax() %>% as.vector()) %>%
    unlist() %>% range()
  clim_lims <- c(floor(clim_lims[1]), ceiling(clim_lims[2]))




# Plot -------------------------------------------------------------------------

  plot_direction <- function(r, title) {
    df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    
    arrows <- df %>%
      filter(round((x - min(x)) / 0.25) %% thin_every == 0,
             round((y - min(y)) / 0.25) %% thin_every == 0) %>%
      mutate(rad = ang * pi / 180,
             dx  = arrow_len * sin(rad),   # assumes angle clockwise from north (0 = N, 90 = E)
             dy  = arrow_len * cos(rad))
    
    p <- ggplot() +
      geom_tile(data = df, aes(x, y, fill = climatology), width = 0.25, height = 0.25) +
      scale_fill_gradient(low = "#FFF5EB", high = "orange",
                          limits = clim_lims, name = "SST (°C)")
    
    if (add_isotherms) {
      p <- p + geom_contour(data = df, aes(x, y, z = climatology),
                            colour = "white", binwidth = 2, linewidth = 0.15)
    }
    
    p +
      geom_sf(data = mpa_shp, fill = "grey10", alpha = 0.35, linewidth = NA) +
      geom_sf(data = oceania_stanford_shp, fill = "#404F4B", colour = NA) +
      geom_sf(data = eez_shp, fill = NA, colour = "black", linewidth = 0.25) +
      geom_segment(data = arrows,
                   aes(x = x, y = y, xend = x + dx, yend = y + dy),
                   arrow = arrow(length = unit(0.06, "cm")), linewidth = 0.2) +
      coord_sf(xlim = c(105, 175), ylim = c(-50, -5)) +
      theme_void(base_size = 10) +
      labs(title = title)
  }



# Run --------------------------------------------------------------------------

  # Recent past: if historical runs are identical across SSPs, use one file per ESM
  recent_files <- file_df %>%
    filter(term == "recent_past") %>%
    distinct(esm, .keep_all = TRUE) %>%
    pull(f)

  recent_r <- ens_direction(recent_files)
  ggsave(file.path(o_fol, "vocc_direction_recent_term.pdf"),
         plot_direction(recent_r, "Recent past (1995–2014)"),
         width = 7, height = 5)

  # Each SSP x future term
  combos <- file_df %>%
    filter(term != "recent_past") %>%
    distinct(ssp, term)

  pwalk(combos, function(ssp, term) {
    message("Processing: ", ssp, " ", term)
    fs <- file_df %>% filter(ssp == !!ssp, term == !!term) %>% pull(f)
    r  <- ens_direction(fs)
    ggsave(file.path(o_fol, paste0("vocc_direction_", ssp, "_", term, ".pdf")),
           plot_direction(r, paste(ssp, term)),
           width = 7, height = 5)
  })
  
  
  
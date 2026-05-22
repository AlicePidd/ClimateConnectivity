# Using graph theory indices to compute other analyses
  # Written by Alice Pidd
        # Nov 2025


# Helpers ----------------------------------------------------------------------
 
  source("Helpers.R")
  metric <- "VoCCtracers"

  
  

# Folders ----------------------------------------------------------------------
  
  sum_fol <- make_folder(source_disk, metric, "4_pairs_mpa_summed")
  GT_top_fol <- make_folder(source_disk, metric, "9_graph_theory_calc/top")
  GT_lineplot_fol <- make_folder(source_disk, metric, "10_graph_theory_plot/lineplots")

  
  
  
# Line plots for the whole shebang ---------------------------------------------

  all_top50 <- readRDS(paste0(GT_top_fol, "/GT_all_metrics_top50_MPAs_by_scenario_ALLnetworks.RDS"))
  all_top50
  dat <- all_top50
  metric_name <- "betweenness_inverse_weighted"
  metric_name <- "total_strength"

  head(dat)
  
  
  
  
# Networks corresponding to each MPA_ID in the top 5 of each metric - for the manuscript GT line plot figure -----------

  m <- all_top50 %>%
    filter(term != "recent-term") %>%
    group_by(metric_type, ssp, MPA_ID) %>%
    summarise(median_value = median(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(metric_type, ssp) %>%
    slice_max(median_value, n = 5) %>%
    ungroup() %>%
    distinct(MPA_ID) %>%
    pull(MPA_ID) %>%
    as.numeric() %>%
    sort()
  m

  all_top50 %>%
    filter(term != "recent-term") %>%
    filter(
      (metric_type == "betweenness_inverse_weighted" & value > quantile(., 0.75)) |
        (metric_type == "total_strength" & value > quantile(., 0.75))
      ) %>%
    distinct(MPA_ID) %>%
    pull(MPA_ID) %>%
    as.numeric2() %>%
    sort()
  
    m <- c(5, 29, 31, 58, 81, 
           # 109, 147, 
           132, 137, 159, 164, 190, 191, 195, 213, 215, 216, 231, 240, 267, 
           # 241, 
           330, 354, 
           # 364, 
           388, 415, 467, 473, 474 
           # 418, 471
           )

  mpa_subset <- mpa_shp %>%
    filter(MPA_ID %in% m) %>% # Get the corresponding data for the select MPA_IDs in m
    mutate(recalc_AREA_KM2 = round(recalc_AREA_KM2, 2)) %>%
    arrange(desc(recalc_AREA_KM2)) %>%
    print(n = length(m))


  
  
# Plot function ----------------------------------------------------------------
  # Top 5 MPAs with highest median index values across all time periods within each SSP
  
  plot_metric_trends <- function(dat, metric_name) {
    
    plot_data <- dat %>% 
      filter(metric_type == metric_name, 
             term != "recent-term")
    
    # Get top 5 MPAs per SSP
    top_5_mpas <- plot_data %>%
      group_by(ssp, MPA_ID) %>%
      summarise(median_value = median(value, na.rm = TRUE), .groups = "drop") %>%
      group_by(ssp) %>%
      {if(metric_name == "strength_asymmetry") {
        slice_max(., abs(median_value), n = 5)
      } else {
        slice_max(., median_value, n = 5)
      }} %>%
      ungroup()
    
    top_5_data <- plot_data %>%
      semi_join(top_5_mpas, by = c("ssp", "MPA_ID"))
    
    # Get label positions
    label_data <- top_5_data %>%
      group_by(ssp, MPA_ID) %>%
      arrange(term) %>%
      slice_tail(n = 1) %>% # Show label at end
      ungroup()
    
    # Calculate median trend
    median_trend <- plot_data %>%
      group_by(ssp, term) %>%
      summarise(median_val = median(value, na.rm = TRUE),
                Q25 = quantile(value, 0.25, na.rm = TRUE),
                Q75 = quantile(value, 0.75, na.rm = TRUE),
                P025 = quantile(value, 0.025, na.rm = TRUE),
                P975 = quantile(value, 0.975, na.rm = TRUE),
                .groups = "drop")
    
    ggplot() +
      # All individual MPA trajectories (thin grey lines)
      geom_line(data = plot_data,
                aes(x = term, y = value, group = MPA_ID),
                alpha = 0.2, color = "grey50", size = 0.3) +
      # Top 5 MPAs (bolded)
      geom_line(data = top_5_data,
                aes(x = term, y = value, group = MPA_ID),
                linewidth = 0.5, alpha = 0.8) +s
      # Top 5 labels as points
      geom_point(data = label_data,
                 aes(x = term, y = value),
                 size = 2, shape = 21, 
                 fill = "white", color = "black", stroke = 0.5) +
      {if(metric_name == "strength_asymmetry") {
        geom_hline(yintercept = 0, linetype = "dotted", color = "grey30", linewidth = 0.3)
      } else {
        NULL
      }} +
      # Labels for top 5
      geom_text_repel(data = label_data,
                      aes(x = term, y = value, label = paste("MPA", MPA_ID)),
                      size = 2.5,
                      direction = "y",
                      nudge_x = 0.8,
                      xlim = c(NA, NA),
                      segment.size = 0.2,
                      segment.color = "grey50",
                      min.segment.length = 0,
                      max.overlaps = Inf) +
      # 95% quantile ribbon - COLORED BY SSP
      geom_ribbon(data = median_trend,
                  aes(x = term, ymin = P025, ymax = P975, 
                      group = 1, # So we only need 1 ribbon for all values
                      fill = ssp), 
                  alpha = 0.1) +
      # IQR ribbon - COLORED BY SSP
      geom_ribbon(data = median_trend,
                  aes(x = term, ymin = Q25, ymax = Q75, 
                      group = 1, # As above
                      fill = ssp), 
                  alpha = 0.2) +
      # Median line - COLORED BY SSP
      geom_line(data = median_trend,
                aes(x = term, y = median_val, group = 1, color = ssp),
                linewidth = 0.5, linetype = "dashed") +
      scale_x_discrete(expand = expansion(add = c(0.1, 0))) + # Making the x axis have less hangover the edges
      # coord_cartesian(clip = "off") +
      scale_fill_manual(values = IPCC_pal, guide = "none") + # IPCC palette
      scale_color_manual(values = IPCC_pal, guide = "none") +
      facet_wrap(~ssp, ncol = 4) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            aspect.ratio = 0.8,
            panel.grid.minor.y = element_blank(), # Blank the y minor lines
            panel.grid.major.x = element_line(linewidth = 0.2), # Make major x axis lines thin
            panel.spacing.x = unit(1.5, "lines")) + # More space between facets
      labs(title = paste("Top 5 MPAs:", metric_name),
           subtitle = "Grey lines = all MPAs, Black lines = top 5 MPAs, Ribbon = median ± IQR (dark) and 95% range (light)",
           x = "Term (20-year)", y = metric_name)
  }
  
  # Create plots for each metric
  p1 <- plot_metric_trends(all_top50, "betweenness_inverse_weighted")
  p1
  p2 <- plot_metric_trends(all_top50, "total_strength")
  p2

  ggsave(paste0(GT_lineplot_fol, "/top50_betweenness-invweightednormalised_trends_finalVersion.pdf"), p1, width = 14, height = 10, dpi = 300)
  ggsave(paste0(GT_lineplot_fol, "/top50_totalstrength_trends_finalVersion.pdf"), p2, width = 14, height = 10, dpi = 300)

    
    
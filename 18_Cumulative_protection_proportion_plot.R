# Plotting the proportion of time that trajectories are protected throughout their lifetime
    # Written by Alice Pidd
        # July 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"


  

# Folders ----------------------------------------------------------------------

  propall_fol <- make_folder(disk, metric, "18_cumulative_traj_protection/all")
  propmpa_fol <- make_folder(disk, metric, "18_cumulative_traj_protection/mpa-starts")
  plot_fol <- make_folder(disk, metric, "19_cumulative_traj_protection_plots")

  
  
  
# Get proportion data ----------------------------------------------------------
  
  read_and_join <- function(f){
    d <- readRDS(f)
    return(d)
  }
  
  
  # MPA-start files
    mpa_files <- dir(propmpa_fol, full.names = TRUE, pattern = "MPAstarts")
    mpa_comb <- map(mpa_files, read_and_join) %>% 
      bind_rows()
    mpa_comb
    unique(mpa_comb$ssp)
    unique(mpa_comb$term)
  
  # All files
    all_files <- dir(propall_fol, full.names = TRUE, pattern = "all")
    all_comb <- map(all_files, read_and_join) %>% 
      bind_rows()
    all_comb
    unique(all_comb$ssp)
    unique(all_comb$term)
  
  
    
    

    plot_dat <- bind_rows(med_mpastart_prop, med_all_prop)
    
    ggplot() +
      geom_density(data = filter(plot_dat, group == "MPAstart"),
                   aes(x = med_prop, y = after_stat(density),
                       fill = ssp, group = ssp),
                   adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
      geom_density(data = filter(plot_dat, group == "Alltraj"),
                   aes(x = med_prop, y = -after_stat(density),
                       fill = ssp, group = ssp),
                   adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
      facet_wrap(~ term_label, nrow = 1) +
      scale_fill_manual(values = IPCC_pal,
                        labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                        name = "SSP") +
      theme_minimal(base_size = 11) +
      theme(strip.text = element_text(size = 10),
            panel.grid.minor = element_blank(),
            legend.position = "bottom") +
      labs(x = "Median proportion of cumulative protection",
           title = "Cumulative trajectory protection — MPA-start vs. all trajectories")
    
    
    # ECDF plot
      ggplot() +
        stat_ecdf(data = med_mpastart_prop,
                  aes(x = med_prop, colour = ssp),
                  linewidth = 0.7) +
        stat_ecdf(data = med_all_prop,
                  aes(x = med_prop, colour = ssp),
                  linewidth = 0.7,
                  linetype = "dashed") +
        scale_colour_manual(values = IPCC_pal,
                            labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                            name = "SSP") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "bottom") +
        labs(x = "Proportion of trajectory spent in MPAs",
             y = "Cumulative proportion of trajectories",
             title = "Cumulative trajectory protection — MPA-start (solid) vs. all trajectories (dashed)")

    
# MPA-starters -----------------
  
  ## Median proportion per ssp-term combo ------------
  
    mpa_comb1 <- mpa_comb %>%
      mutate(term_label = factor(term,
                                 levels = c("recent-term", "near-term", "mid-term", "intermediate-term", "long-term"),
                                 labels = c("Recent-term\n(1995-2014)", "Near-term\n(2021-2040)", 
                                            "Mid-term\n(2041-2060)", "Intermediate-term\n(2061-2080)", 
                                            "Long-term\n(2081-2100)")),
             ssp = factor(ssp, levels = c("ssp126", "ssp245", "ssp370", "ssp585")))
    
    
    med_mpastart_prop <- mpa_comb1 %>% 
      filter(total_steps == 241) %>%
      group_by(traj_ID, term_label, ssp) %>%
      reframe(med_prop = pmax(median(prop_in_MPA), 0.5), # Truncate from 0.5-1
              # q1 = quantile(prop_in_MPA, 0.25),
              # q3 = quantile(prop_in_MPA, 0.75)
              group = "MPAstart")
    med_mpastart_prop
    
    
    # meds_lines <- med_mpastart_prop %>% 
    #   group_by(term_label, ssp) %>%
    #   summarise(med = median(med_prop))
    
    
  ## Plot  --------------
    
    mpastart_plot <- ggplot() +
      geom_density(data = med_mpastart_prop, 
                   aes(x = med_prop, fill = ssp, group = ssp),
                   adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
      # geom_vline(data = meds_lines,
      #            aes(xintercept = mean(med), colour = ssp),
      #            linewidth = 0.5,
      #            linetype = "dashed") +
      # geom_text(data = med_mpastart_prop,
      #           aes(y = Inf, x = mean(med_prop), label = round(mean(med_prop), 2)),
      #           hjust = 1.1, vjust = -0.5,
      #           size = 3) +
      facet_wrap(~term_label, nrow = 1) +
      scale_fill_manual(values = IPCC_pal,
                        labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                        name = "SSP") +
      scale_color_manual(values = IPCC_pal,
                         labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                         name = "SSP") +
      # scale_y_continuous(expand = c(0, 10)) +
      theme_minimal(base_size = 11) +
      theme(
        strip.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      ) +
      labs(x = "Median proportion of cumulative protection of trajectories",
           title = "Temporal patterns in trajectory protection time under climate futures - trajectories STARTING inside MPAs")
    
    mpastart_plot
    ggsave(paste0(plot_fol, ""), mpastart_plot)
      
  ## Spatial plots ------------
    
  
    
      
    
# ALL trajectories, regardless of where they start -----------------------------
    
  ## Median proportion per ssp-term combo ------------
    
    all_comb1 <- all_comb %>%
      mutate(term_label = factor(term,
                                 levels = c("recent-term", "near-term", "mid-term", "intermediate-term", "long-term"),
                                 labels = c("Recent-term\n(1995-2014)", "Near-term\n(2021-2040)", 
                                            "Mid-term\n(2041-2060)", "Intermediate-term\n(2061-2080)", 
                                            "Long-term\n(2081-2100)")),
             ssp = factor(ssp, levels = c("ssp126", "ssp245", "ssp370", "ssp585")))
    
    
    # tic()
    med_all_prop <- all_comb1 %>% 
      filter(total_steps == 241) %>%
      group_by(traj_ID, term_label, ssp) %>%
      reframe(med_prop = pmin(median(prop_in_MPA), 0.5), # Truncate from 0-0.5
              # q1 = quantile(prop_in_MPA, 0.25),
              # q3 = quantile(prop_in_MPA, 0.75)
              group = "Alltraj")
    med_all_prop
    # toc()
    
    
    
    med_all_prop <- med_all_prop %>% 
      mutate(group = "Alltraj") %>% 
      dplyr::select(-q1, -q3)
    
    ## Plot ------------
    
    all_plot <- ggplot() +
      geom_density(data = med_all_prop, 
                   aes(x = med_prop, fill = ssp, group = ssp),
                   adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
      geom_vline(data = med_all_prop,
                 aes(xintercept = median(med_prop), colour = ssp),
                 linewidth = 0.5,
                 linetype = "dashed") +
      geom_text(data = med_all_prop,
                aes(y = Inf, x = median(med_prop), label = round(med_prop, 2)),
                hjust = 1.1, vjust = -0.5,
                size = 3) +
      facet_wrap(~term_label, nrow = 1) +
      scale_fill_manual(values = IPCC_pal,
                        labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                        name = "SSP") +
      scale_color_manual(values = IPCC_pal,
                         labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                         name = "SSP") +
      scale_x_continuous(limits = c(0, 0.5)) +
      theme_minimal(base_size = 11) +
      theme(
        strip.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      ) +
      labs(x = "Median proportion of cumulative protection of trajectories",
           title = "Temporal patterns in trajectory protection time under climate futures - ALL trajectories, regardless of starting point")
    
    
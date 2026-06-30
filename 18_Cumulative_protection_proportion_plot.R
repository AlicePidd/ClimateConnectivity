# Plotting the proportion of time that trajectories are protected throughout their lifetime
    # Written by Alice Pidd
        # July 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"


  

# Folders ----------------------------------------------------------------------

  prop_fol <- make_folder(disk, metric, "18_cumulative_traj_protection")
  propall_fol <- make_folder(disk, metric, "18_cumulative_traj_protection/all")
  propmpa_fol <- make_folder(disk, metric, "18_cumulative_traj_protection/mpa-starts")
  plot_fol <- make_folder(disk, metric, "19_cumulative_traj_protection_plots")

  
  
  
# Get proportion data ----------------------------------------------------------
  
  read_and_join <- function(f){
    d <- readRDS(f)
    return(d)
  }
  
  
  ## MPA-start files ---------
  
    mpa_files <- dir(propmpa_fol, full.names = TRUE, pattern = "MPAstarts")
    mpa_comb <- map(mpa_files, read_and_join) %>% 
      bind_rows() %>% 
      mutate(term_label = factor(term,
                                 levels = c("recent-term", "near-term", 
                                            "mid-term", "intermediate-term", 
                                            "long-term"),
                                 labels = c("Recent-term\n(1995-2014)", 
                                            "Near-term\n(2021-2040)", 
                                            "Mid-term\n(2041-2060)", 
                                            "Intermediate-term\n(2061-2080)", 
                                            "Long-term\n(2081-2100)")),
             ssp = factor(ssp, levels = c("ssp126", "ssp245", "ssp370", "ssp585")))
    mpa_comb

    med_mpastart_prop <- mpa_comb %>% 
      filter(total_steps == 241) %>%
      group_by(traj_ID, term_label, ssp) %>%
      reframe(med_prop = pmax(median(prop_in_MPA), 0.5), # Truncate from 0.5-1
              group = "MPAstart")
    med_mpastart_prop
    
    
  
  ## All files ---------
    
    all_files <- dir(propall_fol, full.names = TRUE, pattern = "all")
    all_comb <- map(all_files, read_and_join) %>% 
      bind_rows() %>% 
      mutate(term_label = factor(term,
                                 levels = c("recent-term", "near-term", 
                                            "mid-term", "intermediate-term", 
                                            "long-term"),
                                 labels = c("Recent-term\n(1995-2014)", 
                                            "Near-term\n(2021-2040)", 
                                            "Mid-term\n(2041-2060)", 
                                            "Intermediate-term\n(2061-2080)", 
                                            "Long-term\n(2081-2100)")),
             ssp = factor(ssp, levels = c("ssp126", "ssp245", "ssp370", "ssp585")))
    all_comb

    tic()
    med_all_prop <- all_comb %>% 
      filter(total_steps == 241) %>%
      group_by(traj_ID, term_label, ssp) %>%
      reframe(med_prop = pmin(median(prop_in_MPA), 0.5), # Truncate from 0-0.5
              group = "Alltraj")
    med_all_prop
    toc()
    
    
  
  # ## Plotting both back to back ------------
  #   
  #   plot_dat <- bind_rows(med_mpastart_prop, med_all_prop)
  #   
  #   ggplot() +
  #     geom_density(data = filter(plot_dat, group == "MPAstart"),
  #                  aes(x = med_prop, y = after_stat(density),
  #                      fill = ssp, group = ssp),
  #                  adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
  #     geom_density(data = filter(plot_dat, group == "Alltraj"),
  #                  aes(x = med_prop, y = -after_stat(density),
  #                      fill = ssp, group = ssp),
  #                  adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
  #     facet_wrap(~ term_label, nrow = 1) +
  #     scale_fill_manual(values = IPCC_pal,
  #                       labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
  #                       name = "SSP") +
  #     theme_minimal(base_size = 11) +
  #     theme(strip.text = element_text(size = 10),
  #           panel.grid.minor = element_blank(),
  #           legend.position = "bottom") +
  #     labs(x = "Median proportion of cumulative protection",
  #          title = "Cumulative trajectory protection — MPA-start vs. all trajectories")
  #   
  #   
  #   
# Attaching starting trajectory geometries to the proportion data --------------
    
  start_points_mpas <- readRDS(paste0(prop_fol, "/trajectory_MPA-start_point_geometries.RDS"))
  start_points_all <- readRDS(paste0(prop_fol, "/trajectory_all-trajs_point_geometries.RDS"))
    
  ## Join to the prop data --------- 
    start_points_prop_mpas <- med_mpastart_prop %>%
      left_join(start_points_mpas, by = "traj_ID") %>%
      st_as_sf()

    start_points_prop_all <- med_all_prop %>%
      left_join(start_points_all, by = "traj_ID") %>%
      st_as_sf()

    
  ## Aggregated by SSP --------- 
    # MPA-starters --------- 
      start_points_ssp_mpas <- start_points_prop_mpas %>%
        group_by(traj_ID, ssp, geometry) %>%
        summarise(med_prop = median(med_prop), .groups = "drop") %>%
        st_as_sf()
      
      # ggplot() +
      #   geom_sf(data = aus_detailed_shp, fill = "grey80", colour = NA) +
      #   geom_sf(data = start_points_ssp_mpas,
      #           aes(colour = med_prop),
      #           size = 0.5, alpha = 0.7) +
      #   scale_colour_viridis_c(name = "Median cumulative\nprotection",
      #                          option = "viridis") +
      #   facet_wrap(~ ssp, nrow = 2) +
      #   theme_minimal() +
      #   labs(title = "Cumulative trajectory protection by source MPA")
      # ggsave(paste0(plot_fol, "/cumulative_trajectory_protection_by_source_MPA.pdf"), width = 10, height = 8)
    
    
    # All trajectories --------- 
      start_points_ssp_all <- start_points_prop_all %>%
        group_by(traj_ID, ssp, geometry) %>%
        summarise(med_prop = median(med_prop), .groups = "drop") %>%
        st_as_sf()
      
      # ggplot() +
      #   # geom_sf(data = aus_detailed_shp, fill = "grey80", colour = NA) +
      #   geom_sf(data = start_points_ssp_all,
      #           aes(colour = med_prop),
      #           size = 0.5, alpha = 0.7) +
      #   scale_colour_viridis_c(name = "Median cumulative\nprotection",
      #                          option = "viridis") +
      #   facet_wrap(~ ssp, nrow = 2) +
      #   theme_minimal() +
      #   labs(title = "Cumulative trajectory protection by source location")
      # ggsave(paste0(plot_fol, "/cumulative_trajectory_protection_by_source_location.pdf"), width = 10, height = 8)
      
    
    # Both plotted together ---------
      ggplot() +
        # geom_sf(data = aus_detailed_shp, fill = "grey80", colour = NA) +
        geom_sf(data = start_points_ssp_all,
                aes(colour = med_prop),
                size = 0.2, alpha = 0.5) +
        geom_sf(data = start_points_ssp_mpas,
                aes(colour = med_prop),
                size = 0.2, alpha = 0.8) +
        geom_sf(data = mpa_shp, fill = NA, colour = "white") +
        scale_colour_viridis_c(name = "Median cumulative\nprotection",
                               option = "viridis",
                               limits = c(0, 1)) +
        facet_wrap(~ ssp, nrow = 2) +
        theme_minimal() +
        labs(title = "Cumulative trajectory protection — MPA-start vs all trajectories")
      ggsave(paste0(plot_fol, "/cumulative_trajectory_protection_combined.pdf"), width = 10, height = 8)
   
      
      
      
      
##**Density plots**
       
# MPA-starters -----------------------------------------------------------------
  
  ## Median proportion per ssp-term combo ------------
    
    # meds_lines <- med_mpastart_prop %>% 
    #   group_by(term_label, ssp) %>%
    #   summarise(med = median(med_prop))
    
    
  ## Plot  --------------
    
    mpastart_plot <- ggplot() +
      geom_density(data = med_mpastart_prop, 
                   aes(x = med_prop, fill = ssp, group = ssp),
                   adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
      facet_wrap(~term_label, nrow = 1) +
      scale_fill_manual(values = IPCC_pal,
                        labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                        name = "SSP") +
      scale_color_manual(values = IPCC_pal,
                         labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                         name = "SSP") +
      theme_minimal(base_size = 11) +
      theme(
        strip.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      ) +
      labs(x = "Median proportion of cumulative protection of trajectories",
           title = "Temporal patterns in trajectory protection time under climate futures - trajectories STARTING inside MPAs")
    
    mpastart_plot
    ggsave(paste0(plot_fol, "/density_of_cumulative_proportion_of_protection_for_MPA-start.pdf"), height = 5, width = 15)
      

    
# ALL trajectories, regardless of where they start -----------------------------
    
  ## Median proportion per ssp-term combo ------------

    # med_all_prop <- med_all_prop %>% 
    #   mutate(group = "Alltraj") %>% 
    #   dplyr::select(-q1, -q3)
    
    
  ## Plot ------------
    
    all_plot <- ggplot() +
      geom_density(data = med_all_prop, 
                   aes(x = med_prop, fill = ssp, group = ssp),
                   adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
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
    ggsave(paste0(plot_fol, "/density_of_cumulative_proportion_of_protection_for_all-trajs.pdf"), height = 5, width = 15)
    
    
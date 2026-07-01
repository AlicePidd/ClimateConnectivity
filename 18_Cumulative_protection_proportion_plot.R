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

    
  
# Attaching starting trajectory geometries to the proportion data --------------
    
  med_mpastart_prop <- readRDS(paste0(prop_fol, "/median_proportions_of_protection_for_MPA-start_trajectories.RDS"))
  med_all_prop <- readRDS(paste0(prop_fol, "/median_proportions_of_protection_for_ALL_trajectories.RDS"))

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
    
    
    # All trajectories --------- 
      start_points_ssp_all <- start_points_prop_all %>%
        group_by(traj_ID, ssp, geometry) %>%
        summarise(med_prop = median(med_prop), .groups = "drop") %>%
        st_as_sf()
    
    
    
# Plot spatially ---------------------------------------------------------------

  plot_prop <- function(ssp_val) {
    
    dat_all <- start_points_ssp_all %>% 
      filter(ssp == ssp_val)
    dat_mpa <- start_points_ssp_mpas %>% 
      filter(ssp == ssp_val)

        ggplot() +
        geom_sf(data = dat_all, # Points
                aes(colour = med_prop),
                size = 0.2, alpha = 0.8) +
        geom_sf(data = dat_mpa, # Points
                aes(colour = med_prop),
                size = 0.2, alpha = 0.8) +
        # geom_spatraster(data = blended_raster) + # For when its a raster
        geom_sf(data = mpa_shp, fill = NA, colour = "white", lwd = 0.15) +
        geom_sf(data = eez_shp, fill = NA, color = "black", lwd = 0.2) +
        geom_sf(data = oceania_stanford_shp, fill = "grey80", col = NA) + #536560
        scale_colour_gradientn(colors = blueyellow_pal, name = "Median cumulative\nprotection", limits = c(0, 1)) +
        # scale_fill_gradientn(colors = blueyellow_pal, name = "Median cumulative\nprotection", limits = c(0, 1)) + # For when its a raster
        # facet_wrap(~ ssp, nrow = 2) +
        theme_void() +
        labs(title = paste0( "Proportion of cumulative trajectory protection -- ", ssp_val))

      ggsave(paste0(plot_fol, "/cumulative_trajectory_protection_combined_", ssp_val, "_points_blueyellowpal.png"), width = 10, height = 8)
      ggsave(paste0(plot_fol, "/cumulative_trajectory_protection_combined_", ssp_val, "_raster_blueyellowpal.pdf"), width = 10, height = 8)
    
  }
  
  walk(ssp_list, plot_prop)
    
     
    # ## Making it a raster in case ------
      # coords_all <- st_coordinates(dat_all)
      # df_all <- data.frame(
      #   x = coords_all[, 1],
      #   y = coords_all[, 2],
      #   z = dat_all$med_prop
      # )
      # 
      # coords_mpa <- st_coordinates(dat_mpa)
      # df_mpa <- data.frame(
      #   x = coords_mpa[, 1],
      #   y = coords_mpa[, 2],
      #   z = dat_mpa$med_prop
      # )
      # 
      # # Create an empty raster grid template based on your data spacing (0.25 degrees)
      # r_template1 <- rast(ext(st_bbox(dat_all)), resolution = 0.25, crs = "EPSG:4326")
      # r_template2 <- rast(ext(st_bbox(dat_mpa)), resolution = 0.25, crs = "EPSG:4326")
      # 
      # # Burn data into a continuous image grid to get rid of the mesh lines
      # raster_layer1 <- rasterize(as.matrix(df_all[, 1:2]), r_template1, values = df_all$z, fun = mean)
      # raster_layer2 <- rasterize(as.matrix(df_mpa[, 1:2]), r_template1, values = df_mpa$z, fun = mean)
      # blended_raster <- cover(raster_layer2, raster_layer1)
    
    
    
# Plotting as density plots ----------------------------------------------------
       
  ## MPA-starters ------------
    # Median proportion per ssp-term combo
      
      # meds_lines <- med_mpastart_prop %>% 
      #   group_by(term_label, ssp) %>%
      #   summarise(med = median(med_prop))
      
    # As a ggridges plot
      ggplot() +
        geom_density_ridges(data = med_mpastart_prop, 
                            aes(x = med_prop, y = fct_rev(factor(term_label)), 
                                fill = ssp),
                            scale = 3, colour = NA, alpha = 0.3, grid = "y") +
        labs(title = "Proportion of cumulative trajectory protection", x = "Proportion", y = "Period") +
        facet_wrap(~ssp, nrow = 4) +
        scale_fill_manual(values = IPCC_pal,
                          labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                          name = "SSP") +
        scale_color_manual(values = IPCC_pal,
                           labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                           name = "SSP") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none")
      ggsave(paste0(plot_fol, "/ggridges_of_cumulative_proportion_of_protection_for_MPA-start_allSSPs.pdf"), height = 12, width = 6)
  
  
    # # As a density plot 
    #   mpastart_density_plot <- ggplot() +
    #     geom_density(data = med_mpastart_prop, 
    #                  aes(x = med_prop, fill = ssp, group = ssp),
    #                  adjust = 1.5, alpha = 0.5, linewidth = 0.3) +
    #     facet_wrap(~term_label, nrow = 1) +
    #     scale_fill_manual(values = IPCC_pal,
    #                       labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
    #                       name = "SSP") +
    #     scale_color_manual(values = IPCC_pal,
    #                        labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
    #                        name = "SSP") +
    #     theme_minimal(base_size = 11) +
    #     theme(
    #       strip.text = element_text(size = 10),
    #       panel.grid.minor = element_blank(),
    #       legend.position = "bottom"
    #     ) +
    #     labs(x = "Median proportion of cumulative protection of trajectories",
    #          title = "Temporal patterns in trajectory protection time under climate futures - trajectories STARTING inside MPAs")
    #   mpastart_density_plot
    #   ggsave(mpastart_density_plot, paste0(plot_fol, "/density_of_cumulative_proportion_of_protection_for_MPA-start.pdf"), height = 5, width = 15)
      

      

    
    ## ALL trajectories, regardless of where they start ------------
      # Median proportion per ssp-term combo
    
      # med_all_prop <- med_all_prop %>% 
      #   mutate(group = "Alltraj") %>% 
      #   dplyr::select(-q1, -q3)
      

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
      
      
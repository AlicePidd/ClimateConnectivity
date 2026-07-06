# Plotting the proportion of time that trajectories are protected throughout their lifetime
    # Written by Alice Pidd
        # July 2026


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")
  metric <- "VoCCtracers"

  

# Folders ----------------------------------------------------------------------

  in_fol <- make_folder(disk, metric, "18_cumulative_traj_protection")
  plot_fol <- make_folder(disk, metric, "19_cumulative_traj_protection_plots")

    
  
# Attaching starting trajectory geometries to the proportion data --------------
    
  med_mpastart_prop <- readRDS(paste0(in_fol, "/median_proportions_of_protection_for_MPA-start_trajectories_trunc_50-100pct.RDS"))
  med_nonmpa_prop <- readRDS(paste0(in_fol, "/median_proportions_of_protection_for_non-MPA_trajectories_trunc_0-50pct.RDS"))
  med_all_prop <- readRDS(paste0(in_fol, "/median_proportions_of_protection_for_ALL_trajectories_notrunc.RDS"))

  start_points_mpas <- readRDS(paste0(in_fol, "/trajectory_MPA-start_point_geometries.RDS"))
  start_points_nonmpas <- readRDS(paste0(in_fol, "/trajectory_nonMPA-trajs_point_geometries.RDS"))
  start_points_all <- readRDS(paste0(in_fol, "/trajectory_all-trajs_point_geometries.RDS"))
    
  
  ## Join to the prop data --------- 
    start_points_prop_mpas <- med_mpastart_prop %>%
      left_join(start_points_mpas, by = "traj_ID") %>%
      st_as_sf()
  
    start_points_prop_nonmpas <- med_nonmpa_prop %>%
      left_join(start_points_nonmpas, by = "traj_ID") %>%
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
    
    # nonMPA-starters --------- 
      start_points_ssp_nonmpas <- start_points_prop_nonmpas %>%
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
    
      ggplot() +
      geom_sf(data = dat_all, # Points
              aes(colour = med_prop),
              size = 0.2, alpha = 0.8) +
      geom_sf(data = eez_shp, fill = NA, color = "black", lwd = 0.2) +
      geom_sf(data = mpa_shp, fill = NA, colour = "white", lwd = 0.4) +
      geom_sf(data = oceania_stanford_shp, fill = "#7C8A87", col = NA) + #536560
      scale_colour_gradientn(colors = blueyellow_pal1, name = "Median cumulative\nprotection", limits = c(0, 1)) +
      coord_sf(expand = FALSE) +
      theme_light() +
        theme(panel.grid.major = element_line(colour = "grey85", linewidth = 0.2),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.3),
              axis.title = element_blank(),         # drop "Longitude"/"Latitude" text, keep tick labels
              axis.text = element_text(size = 7)) +
      labs(title = paste0( "Proportion of cumulative trajectory protection -- ", ssp_val))

      ggsave(paste0(plot_fol, "/cumulative_trajectory_protection_all-trajs_notrunc_", ssp_val, "_points_blueyellowpal.png"), width = 10, height = 8)
      ggsave(paste0(plot_fol, "/cumulative_trajectory_protection_all-trajs_notrunc_", ssp_val, "_points_blueyellowpal.pdf"), width = 10, height = 8)
    
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
       
  ## Trajectories starting inside MPAs ------------
    # Median proportion per ssp-term combo
      
      meds_lines <- med_mpastart_prop %>%
        group_by(term_label, ssp) %>%
        summarise(med = median(med_prop))
      
    # As a ggridges plot
      ggplot() +
        geom_density_ridges(data = med_mpastart_prop, 
                            aes(x = med_prop, y = fct_rev(factor(term_label)), 
                                fill = ssp),
                            scale = 3, colour = NA, alpha = 0.3, grid = "y") +
        geom_segment(data = meds_lines, 
                     aes(x = med, xend = med, 
                         y = as.numeric(fct_rev(factor(term_label))), 
                         yend = as.numeric(fct_rev(factor(term_label))) + 0.9, 
                         colour = ssp),
                     linewidth = 0.8) +
        labs(title = "Proportion of cumulative trajectory protection - MPA-starters", 
             x = "Proportion", y = "Period") +
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

      

    ## Trajectories starting outside of MPAs ------------
      # Median proportion per ssp-term combo
      
      meds_lines_nonmpa <- med_nonmpa_prop %>%
        group_by(term_label, ssp) %>%
        summarise(med = median(med_prop))
      
      # As a ggridges plot
      ggplot() +
        geom_density_ridges(data = med_nonmpa_prop, 
                            aes(x = med_prop, y = fct_rev(factor(term_label)), 
                                fill = ssp),
                            scale = 3, colour = NA, alpha = 0.3, grid = "y") +
        geom_segment(data = meds_lines_nonmpa, 
                     aes(x = med, xend = med, 
                         y = as.numeric(fct_rev(factor(term_label))), 
                         yend = as.numeric(fct_rev(factor(term_label))) + 0.9, 
                         colour = ssp),
                     linewidth = 0.8) +
        labs(title = "Proportion of cumulative trajectory protection - trajectories starting outside MPAs", 
             x = "Proportion", y = "Period") +
        facet_wrap(~ssp, nrow = 4) +
        scale_fill_manual(values = IPCC_pal,
                          labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                          name = "SSP") +
        scale_color_manual(values = IPCC_pal,
                           labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                           name = "SSP") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none")
      ggsave(paste0(plot_fol, "/ggridges_of_cumulative_proportion_of_protection_for_nonMPA-starters_allSSPs.pdf"), height = 12, width = 6)
      

    
    ## ALL trajectories, regardless of where they start ------------
      # Median proportion per ssp-term combo
    
      meds_lines_all <- med_all_prop %>%
        group_by(term_label, ssp) %>%
        summarise(med = median(med_prop))
      
      # As a ggridges plot
      ggplot() +
        geom_density_ridges(data = med_all_prop, 
                            aes(x = med_prop, y = fct_rev(factor(term_label)), 
                                fill = ssp),
                            scale = 3, colour = NA, alpha = 0.3, grid = "y") +
        geom_segment(data = meds_lines_all, 
                     aes(x = med, xend = med, 
                         y = as.numeric(fct_rev(factor(term_label))), 
                         yend = as.numeric(fct_rev(factor(term_label))) + 0.9, 
                         colour = ssp),
                     linewidth = 0.8) +
        labs(title = "Proportion of cumulative trajectory protection - ALL trajectories regardless of starting position", 
             x = "Proportion", y = "Period") +
        facet_wrap(~ssp, nrow = 4) +
        scale_fill_manual(values = IPCC_pal,
                          labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                          name = "SSP") +
        scale_color_manual(values = IPCC_pal,
                           labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
                           name = "SSP") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none")
      ggsave(paste0(plot_fol, "/ggridges_of_cumulative_proportion_of_protection_for_all-trajs_allSSPs.pdf"), height = 12, width = 6)
      


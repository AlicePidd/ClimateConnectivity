# Using graph theory indices for other analyses
  # Written by Alice Pidd
        # Jan 2025

# Metrics:
  # Betweenness
  # Total strength



# Helpers ----------------------------------------------------------------------
 
  source("Helpers.R")
  metric <- "VoCCtracers"

  
  

# Folders ----------------------------------------------------------------------
  
  GT_node_fol <- make_folder(disk, metric, "9_graph_theory_calc/node")
  GT_edge_fol <- make_folder(disk, metric, "9_graph_theory_calc/edge")
  GT_network_fol <- make_folder(disk, metric, "9_graph_theory_calc/network")
  GT_top_fol <- make_folder(disk, metric, "9_graph_theory_calc/top")
  
  
  
  
# ID the top 50 most important MPAs for each metric, across term and SSP -------
  
  files <- dir(GT_node_fol, full.names = TRUE)
  files
  
  # Get all the metrics for all the SSP and term combos
  all_metrics <- map_dfr(files, function(f) {
    ssp <- basename(f) %>% str_split_i(., "_", 4)
    term <- basename(f) %>% str_split_i(., "_", 5)
    readRDS(f) %>% mutate(ssp = ssp, term = term)
  }) %>%
    mutate(term = factor(term, # Reorder the terms
                         levels = c("recent-term", "near-term", "mid-term", "intermediate-term", "long-term")))
  
  head(all_metrics)
  saveRDS(all_metrics, paste0(GT_top_fol, "/GT_all_metrics_per_MPA_ID_by_scenario.RDS"))
  

  
  
# Get the top 50 MPA_IDs across Australia, per metric, term, and SSP -----------------------------
  
  all_metrics <- readRDS(paste0(GT_top_fol, "/GT_all_metrics_per_MPA_ID_by_scenario.RDS"))
  
  ## Function to get top 50 ------------
    get_top50_by_scenario <- function(data, metric_name) {
      data %>%
        group_by(ssp, term) %>%
        slice_max(order_by = .data[[metric_name]], n = 50) %>%
        ungroup() %>%
        select(MPA_ID, ssp, term, value = all_of(metric_name)) %>%  # Rename to "value"
        mutate(metric_type = metric_name)
    }
  
  ## Get top50 ------------
    top50_betweenness <- get_top50_by_scenario(all_metrics, "betweenness_inverse_weighted")
    top50_total_strength <- get_top50_by_scenario(all_metrics, "total_strength")

  ## Bind together ------------
    all_top50 <- bind_rows(top50_betweenness,
                           top50_total_strength)
    all_top50
    saveRDS(all_top50, paste0(GT_top_fol, "/GT_all_metrics_top50_MPAs_by_scenario_ALLnetworks.RDS"))
  
  
    

# Get the top 50 MPA_IDs in the GBR, and Coral Sea per metric, term, and SSP -----------------------------
  
  ## GBR -----------------------------------------------------------------------
    ### Filter to GBR network only ------------
      gbr_metrics <- all_metrics %>%
        filter(MPA_ID %in% network_mpas$GBR)
      head(gbr_metrics)
    
    ### Get top 50 for GBR ------------
      top50_betweenness_gbr <- get_top50_by_scenario(gbr_metrics, "betweenness_inverse_weighted")
      top50_total_strength_gbr <- get_top50_by_scenario(gbr_metrics, "total_strength")

    ### Bind together ------------
      all_top50_gbr <- bind_rows(top50_betweenness_gbr,
                                 top50_total_strength_gbr)
      all_top50_gbr
      saveRDS(all_top50_gbr, paste0(GT_top_fol, "/GT_all_metrics_top50_MPAs_by_scenario_GBR.RDS"))
  

  ## Coral Sea -----------------------------------------------------------------
    ### Filter to Coral Sea only ------------
      coralsea_metrics <- all_metrics %>%
        filter(MPA_ID %in% network_mpas$CoralSea) 
      head(coralsea_metrics)  
  
    ### Get top 50 for Coral Sea  ------------
      top50_betweenness_cs <- get_top50_by_scenario(coralsea_metrics, "betweenness_inverse_weighted")
      top50_total_strength_cs <- get_top50_by_scenario(coralsea_metrics, "total_strength")

    ### Bind together ------------
    all_top50_coralsea <- bind_rows(top50_betweenness_cs,
                                    top50_total_strength_cs)
    all_top50_coralsea
    saveRDS(all_top50_coralsea, paste0(GT_top_fol, "/GT_all_metrics_top50_MPAs_by_scenario_CoralSea.RDS"))


  ## GBR and Coral Sea ---------------------------------------------------------
    ### Filter to Coral Sea only ------------
      CSGBR_metrics <- all_metrics %>%
        filter(MPA_ID %in% network_mpas$CoralSea | MPA_ID %in% network_mpas$GBR)
      head(CSGBR_metrics)  

    ### Get top 50 for GBR and Coral Sea ------------
      top50_betweenness_CSGBR <- get_top50_by_scenario(CSGBR_metrics, "betweenness_inverse_weighted")
      top50_total_strength_CSGBR <- get_top50_by_scenario(CSGBR_metrics, "total_strength")

      
    ### Bind together ------------
      all_top50_CSGBR <- bind_rows(top50_betweenness_CSGBR,
                                   top50_total_strength_CSGBR)
      all_top50_CSGBR
      saveRDS(all_top50_coralsea, paste0(GT_top_fol, "/GT_all_metrics_top50_MPAs_by_scenario_GBRCoralSea.RDS"))
      
      
      
  
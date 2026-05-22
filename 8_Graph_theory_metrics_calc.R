# Computing graph theory metrics under each SSP-term combination
  # Written by Alice Pidd
      # Nov 2025


# Helpers ----------------------------------------------------------------------
 
  source("Helpers.R")
  metric <- "VoCCtracers"

  
  

# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(disk, metric, "4_pairs_mpa_summed")
  GT_node_fol <- make_folder(disk, metric, "9_graph_theory_calc/node")
  GT_edge_fol <- make_folder(disk, metric, "9_graph_theory_calc/edge")
  GT_network_fol <- make_folder(disk, metric, "9_graph_theory_calc/network")

  
  
  
# Compute network analaysis metrics --------------------------------------------
  
  calc_GT_metrics <- function(f){
    
    ssp <- basename(f) %>%
      str_split_i(., "_", 5)
    term <- basename(f) %>%
      str_split_i(., "_", 6) %>% 
      str_remove(., ".RDS")
    p <- readRDS(f) %>%
      filter(!is.na(from), !is.na(to))
    
    g <- graph_from_data_frame(p, directed = TRUE)
    
    # Create adjacency matrix
      adj_matrix <- as_adjacency_matrix(g, 
                                        attr = "n",
                                        sparse = FALSE) 
      
    # Node metrics
      in_strength <- colSums(adj_matrix)
      out_strength <- rowSums(adj_matrix)
      
      node_metrics <- data.frame(
        MPA_ID = rownames(adj_matrix),
        in_strength = in_strength,
        out_strength = out_strength,
        total_strength = in_strength + out_strength,
        strength_asymmetry = (in_strength - out_strength) / (in_strength + out_strength),
        betweenness_inverse_weighted_norm = betweenness(g, directed = TRUE, # DIRECTED
                                                        weights = 1/E(g)$n, # Weights inversely, so that high strength (n) = low cost = high betweenness
                                                        normalized = TRUE),
        betweenness_inverse_weighted = betweenness(g, directed = TRUE, # DIRECTED
                                                   weights = 1/E(g)$n, # Weights inversely, so that high strength (n) = low cost = high betweenness
                                                   normalized = FALSE)
        )
      
    # Handle NaN values (if both in and out are zero)
      node_metrics$strength_asymmetry[is.nan(node_metrics$strength_asymmetry)] <- 0
    
    # Edge metrics
      edge_df <- p %>%
        mutate(
          # Create consistent pair ID (smaller ID first)
          mpa_1 = pmin(from, to),
          mpa_2 = pmax(from, to)
        ) %>%
        group_by(mpa_1, mpa_2) %>%
        summarise(
          total_exchange = sum(n), # Total links both directions
          flow_1_to_2 = sum(n[from == mpa_1]),
          flow_2_to_1 = sum(n[from == mpa_2]),
          edge_asymmetry = (flow_1_to_2 - flow_2_to_1) / total_exchange, #**Didn't use, but leaving it here** 
            # Asymmetry of this specific edge (which direction dominates?)
          .groups = "drop"
        )
      
    # Network-level metrics
      network_df <- data.frame(
        ssp = ssp,
        term = term,
        n_edges = ecount(g),
        edge_density = edge_density(g),
        median_strength = median(E(g)$n),
        q25_strength = quantile(E(g)$n, 0.25),
        q75_strength = quantile(E(g)$n, 0.75),
        total_trajectories = sum(E(g)$n)
      ) 

    saveRDS(node_metrics, paste0(GT_node_fol, "/GT_node_metrics_", ssp, "_", term, "_invweightedbetweenness.RDS"))
    saveRDS(edge_df, paste0(GT_edge_fol, "/GT_edge_metrics_", ssp, "_", term, ".RDS"))
    saveRDS(network_df, paste0(GT_network_fol, "/GT_network_metrics_", ssp, "_", term, ".RDS"))

  }
  
  files <- dir(in_fol, full.names = TRUE)
  files
  
  tic()
  walk(files, calc_GT_metrics) 
  toc() # 2 sec

  
  
  
# Stack the network metrics and save -------------------------------------------
  
  files <- dir(GT_network_fol, full.names = TRUE, pattern = "ssp")
  list <- map(files, readRDS) %>% 
    bind_rows() %>% 
    as_tibble() %>% 
    filter(term != "recent-term") %>% 
    mutate(term = fct_relevel(term, "near-term", "mid-term", "intermediate-term", "long-term")) %>%
    arrange(ssp, term)
  list
  saveRDS(list, paste0(GT_network_fol, "/GT_network_metrics_all.RDS"))
    

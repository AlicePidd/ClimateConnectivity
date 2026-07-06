# Plotting connectivity matrix heatmaps - top x number of links, only between dif MPAs (no same MPAs or non-MPAs)
  # Written by Alice Pidd
        # Nov 2025



# Helpers ----------------------------------------------------------------------
 
  source("Helpers.R")
  metric <- "VoCCtracers"

  
  

# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(disk, metric, "4_pairs_mpa_summed")
  pairstats_fol <- make_folder(disk, metric, "5_pairs_mpa_stats")
  o_fol <- make_folder(disk, metric, "6_heatmap")
  helper_fol <- make_folder(disk, metric, "_helpers")
  
  mpa_lookup <- readRDS(paste0(helper_fol, "/MPA_ID_by_network.RDS"))
  

  
  
# For ALL networks -------------------------------------------------------------
  
    ## Calculate a threshold to truncate data by so scales are the same -------------
        # Calculated based on the 90th percentile across all n's 
      
      calc_threshold <- function(fol, perc) {
        dir(fol, full.names = TRUE) %>%
          map_dfr(readRDS) %>%
          pull(n) %>%
          quantile(perc)
        }
      thresh <- calc_threshold(in_fol, 0.9) %>% 
        as.numeric()
      thresh # 8467 = 90th percentile, 519.5 = 75th percentile
      
      # The number of MPAs to use here (i.e., top x number)
      cut_off <- 50
  
    
    ## Plot -------------
        
      plot_heatmaps_proximity <- function(f, cut_off, thresh) {
        
        p <- readRDS(f) %>%
          filter(from != to) %>% # No self links (A to A, B to B)
          slice_max(n, n = cut_off, # Get n number of rows
                    with_ties = TRUE) %>% # If multiple rows share the same total, bring them too, even if over n number of rows.
          mutate(same = ifelse(from == to, "TRUE", "FALSE"),
                 n_truncated = pmin(n, thresh))  # Truncate values
        
        file_mpas <- unique(c(p$from, p$to)) # Get unique MPAs in this file
        file_mpas_char <- as.character(file_mpas)
        
        available_mpas <- intersect(file_mpas_char, rownames(dist_matrix)) # Find which MPAs exist in both the file and distance matrix
        
        if (length(available_mpas) >= 2) {
          file_dist_matrix <- dist_matrix[available_mpas, available_mpas] # Use distance matrix to determine order
          
          hc <- hclust(as.dist(file_dist_matrix), method = "ward.D2") # Hierarchical clustering based on distances
          ordered_mpas <- available_mpas[hc$order]
          
          remaining_mpas <- setdiff(file_mpas_char, available_mpas) # Add any MPAs not in distance matrix at the end (sorted)
          if (length(remaining_mpas) > 0) {
            ordered_mpas <- c(ordered_mpas, as.character(sort(as.numeric(remaining_mpas))))
          }
        } else {
          ordered_mpas <- as.character(sort(as.numeric(file_mpas))) # Fallback to original sorting if not enough MPAs in distance matrix
        }
        
        p$from <- factor(p$from, levels = ordered_mpas) # Apply the proximity ordering (instead of the original sorting)
        p$to <- factor(p$to, levels = ordered_mpas)
        
        # Create matrices with new ordering
        mat <- xtabs(p$n_truncated ~ p$from + p$to)
        mat_log <- ifelse(mat == 0, 0, log10(pmin(mat, thresh) + 1)) # + 1 to get around log 1
        
        ssp <- basename(f) %>% str_split_i(., "_", 5)
        term <- basename(f) %>% str_split_i(., "_", 6) %>% str_remove(., ".RDS")
        hm_nm <- basename(f) %>% # Output names
          str_replace(., "traj_pairs", paste0("heatmap_proximity_top", cut_off, "_trunc")) %>%
          str_replace(., ".RDS", ".pdf")
        hmlog_nm <- basename(f) %>%
          str_replace(., "traj_pairs", paste0("heatmap-log_proximity_top", cut_off, "_trunc")) %>%
          str_replace(., ".RDS", ".pdf")
        
        message(paste0("🕒 Processing ", basename(f), " with proximity ordering..."))
        
        # Regular heatmap with fixed scale
        pdf(paste0(o_fol, "/", hm_nm), width = 12, height = 12)
        heatmap.2(mat,
                  Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none",
                  col = distmatrix_pal,
                  key = TRUE, key.title = "", keysize = 0.75, margins = c(4, 4),
                  density.info = "none",
                  breaks = seq(0, thresh, length.out = 101),  # Fixed scale to threshold
                  main = paste0("Proximity-ordered top ", cut_off, ": summed links (trunc at ", round(thresh), ") \n for all ESMs under ", ssp, " ", term),
                  cex.main = 0.5,
                  cexRow = 1.3, # Axis size
                  cexCol = 1.3)
        dev.off()
        
        # Log10-transformed heatmap with fixed scale
        pdf(paste0(o_fol, "/", hmlog_nm), width = 12, height = 12)
        heatmap.2(mat_log,
                  Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none",
                  col = distmatrix_pal,
                  key = TRUE, key.title = "", keysize = 0.75, margins = c(4, 4),
                  density.info = "none",
                  breaks = seq(0, log10(thresh + 1), length.out = 101),  # Fixed log scale
                  main = paste0("Proximity-ordered log-transformed top ", cut_off, " (trunc at ", round(thresh), "): \n summed links for all ESMs under ", ssp, " ", term, "-term"),
                  cex.main = 0.5,
                  cexRow = 1.3, # Axis size
                  cexCol = 1.3)
        dev.off()
      }
      
      dist_matrix <- readRDS(paste0(pairstats_fol, "/mpa_distance_matrix.RDS"))  
      files <- dir(in_fol, full.names = TRUE)
      files
      
      walk(files, ~plot_heatmaps_proximity(.x, cut_off = cut_off, thresh = thresh))
  
      # These plots show the direction (strength of A to B, and strength of B to A)
  

    
    
# For JUST the GBR -------------------------------------------------------------

  ## Calculate a threshold to truncate data by so scales are the same -------------
    # Calculated based on the 90th percentile across all n's in 
    
    calc_threshold_GBRCS <- function(fol, perc) {
      dir(fol, full.names = TRUE) %>%
        map_dfr(readRDS) %>%
        filter(from %in% mpa_lookup$GBR & to %in% mpa_lookup$GBR) %>%
        # filter(from %in% mpa_lookup$GBR | from %in% mpa_lookup$CoralSea & to %in% mpa_lookup$GBR | to %in% mpa_lookup$CoralSea) %>%
        pull(n) %>%
        quantile(perc)
      }
    thresh <- calc_threshold_GBRCS(in_fol, 0.9) %>% 
      as.numeric()
    thresh # 3822.5 = 90th percentile for GBR, 4155.1 = 90th percentile for GBR-CoralSea
    
    # The number of MPAs to use here (i.e., top x number)
    cut_off <- 50


  ## Plot -------------
      
    plot_heatmaps_proximity_bynetwork <- function(f, cut_off, thresh) {
      
      p <- readRDS(f) %>%
        filter(from != to) %>% # No self links (A to A, B to B)
        # filter(from %in% mpa_lookup$GBR | from %in% mpa_lookup$CoralSea & to %in% mpa_lookup$GBR | to %in% mpa_lookup$CoralSea) %>%
        filter(from %in% mpa_lookup$GBR & to %in% mpa_lookup$GBR) %>%
        # filter(from %in% mpa_lookup[[network]] & to %in% mpa_lookup[[network]]) %>% 
        slice_max(n, n = cut_off, # Get n number of rows
                  with_ties = TRUE) %>% # If multiple rows share the same total, bring them too, even if over n numebr of rows.
        mutate(same = ifelse(from == to, "TRUE", "FALSE"),
               n_truncated = pmin(n, thresh))  # Truncate values
      
      # Get unique MPAs in this file
      file_mpas <- unique(c(p$from, p$to))
      file_mpas_char <- as.character(file_mpas)
      
      # Find which MPAs exist in both the file and distance matrix
      available_mpas <- intersect(file_mpas_char, rownames(dist_matrix))
      
      if (length(available_mpas) >= 2) {
        file_dist_matrix <- dist_matrix[available_mpas, available_mpas] # Use distance matrix to determine order
        
        hc <- hclust(as.dist(file_dist_matrix), method = "ward.D2") # Hierarchical clustering based on distances
        ordered_mpas <- available_mpas[hc$order]
        
        remaining_mpas <- setdiff(file_mpas_char, available_mpas) # Add any MPAs not in distance matrix at the end (sorted)
        if (length(remaining_mpas) > 0) {
          ordered_mpas <- c(ordered_mpas, as.character(sort(as.numeric(remaining_mpas))))
        }
      } else {
        ordered_mpas <- as.character(sort(as.numeric(file_mpas))) # Fallback to original sorting if not enough MPAs in distance matrix
      }
      
      # Apply the proximity ordering (instead of the original sorting)
      p$from <- factor(p$from, levels = ordered_mpas)
      p$to <- factor(p$to, levels = ordered_mpas)
      
      # Create matrices with new ordering
      mat <- xtabs(p$n_truncated ~ p$from + p$to)
      mat_log <- ifelse(mat == 0, 0, log10(pmin(mat, thresh) + 1)) # + 1 to get around log 1
      
      # Name the outputs
      ssp <- basename(f) %>% str_split_i(., "_", 5)
      term <- basename(f) %>% str_split_i(., "_", 6) %>% str_remove(., ".RDS")
      hm_nm <- basename(f) %>%
        # str_replace(., "traj_pairs", paste0("heatmap_proximity_GBR-CoralSea_top", cut_off, "_trunc")) %>%
        str_replace(., "traj_pairs", paste0("heatmap_proximity_GBR_top", cut_off, "_trunc")) %>%
        str_replace(., ".RDS", ".pdf")
      hmlog_nm <- basename(f) %>%
        # str_replace(., "traj_pairs", paste0("heatmap-log_proximity_GBR-CoralSea_top", cut_off, "_trunc")) %>%
        str_replace(., "traj_pairs", paste0("heatmap-log_proximity_GBR_top", cut_off, "_trunc")) %>%
        str_replace(., ".RDS", ".pdf")
      
      message(paste0("🕒 Processing ", basename(f), " with proximity ordering..."))
      
      # Regular heatmap with fixed scale
      pdf(paste0(o_fol, "/", hm_nm), width = 12, height = 12)
      heatmap.2(mat,
                Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none",
                col = distmatrix_pal,
                key = TRUE, key.title = "", keysize = 0.75, margins = c(4, 4),
                density.info = "none",
                breaks = seq(0, thresh, length.out = 101),  # Fixed scale to threshold
                # main = paste0("Proximity-ordered GBR-CoralSea only, top ", cut_off, ": summed links (trunc at ", round(thresh), ") \n for all ESMs under ", ssp, " ", term),
                main = paste0("Proximity-ordered GBR only, top ", cut_off, ": summed links (trunc at ", round(thresh), ") \n for all ESMs under ", ssp, " ", term),
                cex.main = 0.5,
                cexRow = 1.3, # Axis size
                cexCol = 1.3)
      dev.off()
      
      # Log-transformed heatmap with fixed scale <- 
      pdf(paste0(o_fol, "/", hmlog_nm), width = 12, height = 12)
      heatmap.2(mat_log,
                Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none",
                col = distmatrix_pal,
                key = TRUE, key.title = "", keysize = 0.75, margins = c(4, 4),
                density.info = "none",
                breaks = seq(0, log10(thresh + 1), length.out = 101),  # Fixed log scale
                # main = paste0("Proximity-ordered GBR-CoralSea only, log-transformed top ", cut_off, " (trunc at ", round(thresh), "): \n summed links for all ESMs under ", ssp, " ", term, "-term"),
                main = paste0("Proximity-ordered GBR only, log-transformed top ", cut_off, " (trunc at ", round(thresh), "): \n summed links for all ESMs under ", ssp, " ", term, "-term"),
                
                cex.main = 0.5,
                cexRow = 1.3, # Axis size
                cexCol = 1.3)
      dev.off()
      
    }
    
    dist_matrix <- readRDS(paste0(pairstats_fol, "/mpa_distance_matrix.RDS"))  
    files <- dir(in_fol, full.names = TRUE)
    files
    
    walk(files, ~plot_heatmaps_proximity_bynetwork(.x, cut_off = cut_off, thresh = thresh))

    
    

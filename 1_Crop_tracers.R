# Load tracers as row-bound sf objects, and crop to general extent
    # Written by Alice Pidd, Feb 2025


# Helpers ----------------------------------------------------------------------

  source("Helpers.R")


  
  
# Folders ----------------------------------------------------------------------
  
  in_fol <- make_folder(disk, "VoCCtracers", "0_VoCC_tracers") 
  o_fol <- make_folder(disk, "VoCCtracers", "1_VoCC_tracers_cropped")
  
  
  
  
# Load and crop tracers --------------------------------------------------------

  load_crop_files <- function(in_dir, out_dir) {
    
    files <- dir(in_dir, full.names = TRUE)
    
    load_crop_and_save <- function(f) {
      
      out_file <- file.path(out_dir, paste0("cropped_", basename(f)))
      
      if(file.exists(out_file)) { # Check if file already exists
        message(paste0("⏭️  Skipping (already exists): ", basename(out_file)))
        return(invisible(NULL)) # If TRUE, it exits early without processing. Only does the rest if file.exists = FALSE i.e., the file doesn't exist.
      }
      
      message(paste0("🕒 Processing: ", basename(f)))
      
      traj <- readRDS(f) %>%
        bind_rows() %>%
        filter(lon >= e1[1],
               lon <= e1[2],
               lat >= e1[3],
               lat <= e1[4])
      
      saveRDS(traj, out_file)
      message(paste0("✅ Saved: ", basename(out_file)))
    }
    
    plan(multisession, workers = detectCores() - 1)
    tic()
    walk(rev(files), load_crop_and_save)
    toc()
    plan(sequential)
  }

  tic()
  load_crop_files(in_fol, o_fol)
  toc() # 4.7 hours on Alice's machine (220 files)
  beep(2)
  
  
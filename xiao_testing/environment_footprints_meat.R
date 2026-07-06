  meat_env_tbl <- data.table( # From IFEU, cradle-to-retail
    meat  = c("red_meat", "processed_meat", "white_meat", "fish"),
    ghg   = c(18.7, 9.7, 5.7, 7.1),
    water = c(3.8, 2.2, 1.6, 0.5),
    land  = c(12.3, 6.2, 4.2, 1.6)
  )
  
  write_fst(meat_env_tbl,"./xiao_testing/environment_footprints_meat.fst", compress = 100)

  # Just in case if we do want to use a function to avoid copy-paste the same codes for _delta_xps
  # # ---- Helper: calculate environmental outcomes from meat intake ----
  # add_meat_environment_outcomes <- function(sp) {
  #   
  #   meat_env_tbl <- data.table( # From IFEU, cradle-to-retail
  #     meat  = c("red_meat", "processed_meat", "white_meat", "fish"),
  #     ghg   = c(18.7, 9.7, 5.7, 7.1),
  #     water = c(3.8, 2.2, 1.6, 0.5),
  #     land  = c(12.3, 6.2, 4.2, 1.6)
  #   )
  #   
  #   # Convert g/day to kg/year
  #   gday_to_kgyear <- 365 / 1000
  #   
  #   init_year <- IMPACTncd$design$sim_prm$init_year_intv - 2000
  #   
  #   for (i in seq_len(nrow(meat_env_tbl))) {
  #     
  #     meat <- meat_env_tbl$meat[i]
  #     
  #     curr_col  <- paste0(meat, "_curr_xps")
  #     delta_col <- paste0(meat, "_delta_xps")
  #     
  #     ghg_col   <- paste0(meat, "_ghg")
  #     water_col <- paste0(meat, "_water")
  #     land_col  <- paste0(meat, "_land")
  #     
  #     delta_ghg_col   <- paste0(meat, "_delta_ghg")
  #     delta_water_col <- paste0(meat, "_delta_water")
  #     delta_land_col  <- paste0(meat, "_delta_land")
  #     
  #     if (!curr_col %in% names(sp$pop)) {
  #       stop(paste0("Column '", curr_col, "' is missing from sp$pop."))
  #     }
  #     
  #     # Scenario-specific current annual environmental footprint
  #     sp$pop[, (ghg_col) :=
  #              get(curr_col) * gday_to_kgyear * meat_env_tbl$ghg[i]]
  #     
  #     sp$pop[, (water_col) :=
  #              get(curr_col) * gday_to_kgyear * meat_env_tbl$water[i]]
  #     
  #     sp$pop[, (land_col) :=
  #              get(curr_col) * gday_to_kgyear * meat_env_tbl$land[i]]
  #     
  #     # Environmental change based on delta_xps.
  #     # Positive delta = reduction; negative delta = increase/substitution.
  #     if (delta_col %in% names(sp$pop)) {
  #       
  #       sp$pop[, (delta_ghg_col) :=
  #                fifelse(year > init_year,
  #                        get(delta_col) * gday_to_kgyear * meat_env_tbl$ghg[i],
  #                        0)]
  #       
  #       sp$pop[, (delta_water_col) :=
  #                fifelse(year > init_year,
  #                        get(delta_col) * gday_to_kgyear * meat_env_tbl$water[i],
  #                        0)]
  #       
  #       sp$pop[, (delta_land_col) :=
  #                fifelse(year > init_year,
  #                        get(delta_col) * gday_to_kgyear * meat_env_tbl$land[i],
  #                        0)]
  #     }
  #   }
  #   
  #   invisible(sp)
  # }

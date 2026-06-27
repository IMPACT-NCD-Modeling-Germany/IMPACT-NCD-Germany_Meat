  #---------------------------------------------------------------------------
  # Purpose
  # Policy scenarios set-up
  # Steps:
  # 1. Create meat price elasticity table with uncertainty: "/xiao_testing/uncertainty_price_elasticity_meat"
  # 2. Create price change table
  # 3. Create tax pass-through table with uncertainty: used Karls SSB pass-through
  # 4. Implement and calculate new consumption and consumption changes
  #---------------------------------------------------------------------------

  #### Scenarios for German meat tax modelling ------------------------------------
  
  scenario_0_fn <- function(sp) {
    
    sp$pop[, c("rm_delta_xps", "pm_delta_xps",
               "wm_delta_xps", "fish_delta_xps"
               ) := 0] 
    
  }
  
  ### Scenario 1 - VAT increase from 7% to 19% on red and processed meat combined ----
  
  scenario_A_vat_meat_fn <- function(sp) {
    
    old_vat <- 0.07
    new_vat <- 0.19
    products <- c("red_meat",
                 "processed_meat",
                 "white_meat",
                 "fish")
    
    # --- 1. Read pass-through ---
    pth_tbl <- read_fst(
      "./xiao_testing/tax_pass_through.fst",
      as.data.table = TRUE
    )
    
    pass_through <- as.numeric(
      pth_tbl[mc == sp$mc_aggr, tax_pth]
    )

    # --- 2. Calculate VAT-induced consumer-price change ---
    vat_price_change <- ((1 + new_vat) / (1 + old_vat) - 1) * pass_through
    
    price_change_vec <- c(
      red_meat = vat_price_change,
      processed_meat = vat_price_change
    )
    
    # --- 3. Read elasticity table ---
    elasticity_mc <- read_fst(
      "./xiao_testing/meat_price_elasticities_mc.fst",
      as.data.table = TRUE
    )
    
    # --- 4. Calculate consumption changes ---
    price_change_tbl <- data.table(
      price = names(price_change_vec),
      price_change = as.numeric(price_change_vec)
    )
    
    x <- elasticity_mc[
      mc == sp$mc_aggr &
        quantity %in% products &
        price %in% names(price_change_vec)
    ]
    
    x <- price_change_tbl[x, on = "price"]
    
    if (x[, anyNA(elasticity_mc)] || x[, anyNA(price_change)]) {
      stop("Missing elasticity or price-change value.")
    }
    
    demand_change_tbl <- x[, .(rel_change = mean(elasticity_mc * price_change)), by = quantity]
    
    ## Convert to named vector for convenient lookup
    rel_change <- setNames(
      demand_change_tbl$rel_change,
      demand_change_tbl$quantity
    )
    
    # --- 5. Calculate new consumption ---
    sp$pop[, red_meat_new_xps :=
             pmax(0, red_meat_curr_xps * (1 + rel_change["red_meat"]))]
    
    sp$pop[, processed_meat_new_xps :=
             pmax(0, processed_meat_curr_xps * (1 + rel_change["processed_meat"]))]
    
    sp$pop[, white_meat_new_xps :=
             pmax(0, white_meat_curr_xps * (1 + rel_change["white_meat"]))]
    
    sp$pop[, fish_new_xps :=
             pmax(0, fish_curr_xps * (1 + rel_change["fish"]))]
    
    ## Positive delta = reduction; negative delta = increase
    # Xiao: Why not new_xps - curr_xps, so that negative means clearly reduction?
    
    sp$pop[, red_meat_delta_xps := red_meat_curr_xps - red_meat_new_xps]
    sp$pop[, processed_meat_delta_xps := processed_meat_curr_xps - processed_meat_new_xps]
    sp$pop[, white_meat_delta_xps := white_meat_curr_xps - white_meat_new_xps]
    sp$pop[, fish_delta_xps := fish_curr_xps - fish_new_xps]
    
    int_year <- IMPACTncd$design$sim_prm$init_year_intv - 2000
    
    sp$pop[year > int_year,`:=`(
      red_meat_curr_xps = red_meat_new_xps,
      processed_meat_curr_xps = processed_meat_new_xps,
      white_meat_curr_xps = white_meat_new_xps,
      fish_curr_xps = fish_new_xps)
      ]
    
    sp$pop[, c("red_meat_new_xps",
               "processed_meat_new_xps",
               "white_meat_new_xps",
               "fish_new_xps") := NULL]
  }

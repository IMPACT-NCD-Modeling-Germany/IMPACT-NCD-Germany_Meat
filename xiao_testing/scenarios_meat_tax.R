  #---------------------------------------------------------------------------
  # Purpose
  # Policy scenario set-up for German meat-tax modelling
  #
  # Steps:
  # 1. Create meat price elasticity table without uncertainty: "/xiao_testing/evs_price_elasticity_table.fst"
  # 2. Read tax pass-through table with uncertainty: used Karls SSB pass-through
  # 3. Create price change table and demand change table: red meat based on weighted average of beef and pork
  # 4. Calculate consumption changes delta_xps
  # 5. Calculate changes in environmental outcomes: GHGEs, water and land use
  #
  # Scenarios:
  #   sc0: No  intervention
  #   sc1: VAT increase from 7% to 19% on red and processed meat
  #   sc2: GHGE-based excise tax using EUR 345/t CO2eq
  #   sc3: GHGE-based excise tax using EUR 55/t CO2eq
  #
  # XIAO's Questions
  # Q1). Assuming immediate change + stable & sustained relative consumption over time 
  # --> good? Or apply e.g., no effect after year 5?
  # Q2). delta_xps as curr_xps - new_xps, where negative means increase
  # --> Why not new_xps - curr_xps so that negative = reduction?
  # Q3). Decided to use EVS 2018 PE table which has no SE
  # --> how to deal with mc?
  # Q4). Did not use new_xps but created directly delta_xps and replaced curr_xps after year 0 with curr_xps - delta_xps
  #---------------------------------------------------------------------------
  
  library(data.table)
  library(fst)
  
  
  #### Scenarios for German meat-tax modelling ---------------------------------
  
  scenario_0_fn <- function(sp) {
    
    sp$pop[, c("red_meat_delta_xps",
               "processed_meat_delta_xps",
               "white_meat_delta_xps",
               "fish_delta_xps") := 0]

    # Current annual environmental footprints
    meat_env_tbl <- read_fst("./xiao_testing/environment_footprints_meat.fst", as.data.table = TRUE)
    gday_to_kgyear <- 365 / 1000
    
    sp$pop[, red_meat_curr_ghg :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", ghg]]
    
    sp$pop[, processed_meat_curr_ghg :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", ghg]]
    
    sp$pop[, white_meat_curr_ghg :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", ghg]]
    
    sp$pop[, fish_curr_ghg :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", ghg]]
    
    
    sp$pop[, red_meat_curr_water :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", water]]
    
    sp$pop[, processed_meat_curr_water :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", water]]
    
    sp$pop[, white_meat_curr_water :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", water]]
    
    sp$pop[, fish_curr_water :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", water]]
    
    
    sp$pop[, red_meat_curr_land :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", land]]
    
    sp$pop[, processed_meat_curr_land :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", land]]
    
    sp$pop[, white_meat_curr_land :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", land]]
    
    sp$pop[, fish_curr_land :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", land]]
    
    sp$pop[, c("red_meat_delta_ghg",
               "processed_meat_delta_ghg",
               "white_meat_delta_ghg",
               "fish_delta_ghg",
               "red_meat_delta_water",
               "processed_meat_delta_water",
               "white_meat_delta_water",
               "fish_delta_water",
               "red_meat_delta_land",
               "processed_meat_delta_land",
               "white_meat_delta_land",
               "fish_delta_land") := 0]
    }
  
  ### Scenario 1 - VAT increase from 7% to 19% on red and processed meat --------
  
  scenario_1_fn <- function(sp) {
    
    # Set scenario variables
    old_vat <- 0.07
    new_vat <- 0.19
    
    # NAKO weighted red-meat composition
    prop_beef <- 0.513
    prop_pork <- 0.487
    
    # Define tax targets and substitutes of interest
    products <- c("beef", "pork", "processed_meat", "white_meat", "fish")
    
    tbl <- read_fst("./xiao_testing/tax_pass_through.fst", as.data.table = TRUE)
    pass_through <- as.numeric(tbl[mc == sp$mc_aggr, tax_pth])
    
    # VAT-induced consumer-price change
    vat_price_change <- ((1 + new_vat) / (1 + old_vat) - 1) * pass_through
    
    price_change_tbl <- data.table(
      price = c("beef", "pork", "processed_meat"),
      price_change = c(vat_price_change,
                       vat_price_change,
                       vat_price_change)
    )
    
    # Calculate demand change
    tbl <- read_fst("./xiao_testing/evs_meat_price_elasticity_table.fst", as.data.table = TRUE)
    
    x <- tbl[quantity %in% products & price %in% price_change_tbl$price]
    x <- price_change_tbl[x, on = "price"]
    
    if (x[, anyNA(elasticity)] || x[, anyNA(price_change)]) {
      stop("Missing elasticity or price-change value.")
    }
    
    demand_change_tbl <- x[, .(rel_change = sum(elasticity * price_change)), by = quantity]
    
    rel_change_evs <- setNames(
      demand_change_tbl$rel_change,
      demand_change_tbl$quantity
    )
    
    red_meat_rel_change <- prop_beef * rel_change_evs["beef"] +
      prop_pork * rel_change_evs["pork"]
    
    rel_change <- c(
      red_meat       = as.numeric(red_meat_rel_change),
      processed_meat = as.numeric(rel_change_evs["processed_meat"]),
      white_meat     = as.numeric(rel_change_evs["white_meat"]),
      fish           = as.numeric(rel_change_evs["fish"])
    )
    
    if (anyNA(rel_change)) {
      stop("Missing relative demand-change value.")
    }
    
    # Change in meat consumption after tax: negative = increase, positive = reduction!
    sp$pop[, red_meat_delta_xps :=
             red_meat_curr_xps - pmax(0, red_meat_curr_xps * (1 + rel_change["red_meat"]))]
    sp$pop[, processed_meat_delta_xps :=
             processed_meat_curr_xps - pmax(0, processed_meat_curr_xps * (1 + rel_change["processed_meat"]))]
    sp$pop[, white_meat_delta_xps :=
             white_meat_curr_xps - pmax(0, white_meat_curr_xps * (1 + rel_change["white_meat"]))]
    sp$pop[, fish_delta_xps :=
             fish_curr_xps - pmax(0, fish_curr_xps * (1 + rel_change["fish"]))]
    
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           red_meat_curr_xps := pmax(0, red_meat_curr_xps - red_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           processed_meat_curr_xps := pmax(0, processed_meat_curr_xps - processed_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           white_meat_curr_xps := pmax(0, white_meat_curr_xps - white_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           fish_curr_xps := pmax(0, fish_curr_xps - fish_delta_xps)]
    
    # Current annual environmental footprints after consumption changes
    meat_env_tbl <- read_fst("./xiao_testing/environment_footprints_meat.fst", as.data.table = TRUE)
    gday_to_kgyear <- 365 / 1000
    
    sp$pop[, red_meat_curr_ghg :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", ghg]]
    
    sp$pop[, processed_meat_curr_ghg :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", ghg]]
    
    sp$pop[, white_meat_curr_ghg :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", ghg]]
    
    sp$pop[, fish_curr_ghg :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", ghg]]
    
    
    sp$pop[, red_meat_curr_water :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", water]]
    
    sp$pop[, processed_meat_curr_water :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", water]]
    
    sp$pop[, white_meat_curr_water :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", water]]
    
    sp$pop[, fish_curr_water :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", water]]
    
    
    sp$pop[, red_meat_curr_land :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", land]]
    
    sp$pop[, processed_meat_curr_land :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", land]]
    
    sp$pop[, white_meat_curr_land :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", land]]
    
    sp$pop[, fish_curr_land :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", land]]
    
    # Environmental changes due to scenario-induced intake changes
    # Positive value = environmental reduction
    # Negative value = environmental increase, e.g. from substitution
    
    sp$pop[, red_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", ghg], 0)]
    
    sp$pop[, processed_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", ghg], 0)]
    
    sp$pop[, white_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", ghg], 0)]
    
    sp$pop[, fish_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", ghg], 0)]
    
    
    sp$pop[, red_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", water], 0)]
    
    sp$pop[, processed_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", water],0)]
    
    sp$pop[, white_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", water], 0)]
    
    sp$pop[, fish_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", water], 0)]
    
    
    sp$pop[, red_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", land], 0)]
    
    sp$pop[, processed_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", land], 0)]
    
    sp$pop[, white_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", land], 0)]
    
    sp$pop[, fish_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", land], 0)]
    
  }
  
  ### Scenario 2 - GHGE-based excise tax using EUR 345/t CO2eq ------------------
  
  scenario_2_fn <- function(sp) {
    
    # Set scenario variables (UBA 2025)
    co2_price <- 345
    
    # Carbon emissions: kg CO2eq per kg edible meat (IFEU)
    co2eq_beef <- 13.4
    co2eq_pork <- 3.9
    co2eq_pm   <- 8.0
    
    # Retail prices: EUR/kg, 2025 euros (Thünen Institut)
    retail_price_beef <- 12.65
    retail_price_pork <- 8.64
    retail_price_pm   <- 11.98
    
    # NAKO weighted red-meat composition
    prop_beef <- 0.513
    prop_pork <- 0.487
    
    # Define tax targets and substitutes of interest
    products <- c("beef", "pork", "processed_meat", "white_meat", "fish")
    
    tbl <- read_fst("./xiao_testing/tax_pass_through.fst", as.data.table = TRUE)
    pass_through <- as.numeric(tbl[mc == sp$mc_aggr, tax_pth])
    
    # Excise tax in EUR/kg product
    tax_beef <- co2eq_beef * co2_price / 1000
    tax_pork <- co2eq_pork * co2_price / 1000
    tax_pm   <- co2eq_pm   * co2_price / 1000
    
    # Tax-induced consumer-price change
    price_change_tbl <- data.table(
      price = c("beef", "pork", "processed_meat"),
      price_change = c(
        tax_beef * pass_through / retail_price_beef,
        tax_pork * pass_through / retail_price_pork,
        tax_pm   * pass_through / retail_price_pm
      )
    )
    
    # Calculate demand change
    tbl <- read_fst("./xiao_testing/evs_meat_price_elasticity_table.fst", as.data.table = TRUE)

    x <- tbl[quantity %in% products & price %in% price_change_tbl$price]
    x <- price_change_tbl[x, on = "price"]
    
    if (x[, anyNA(elasticity)] || x[, anyNA(price_change)]) {
      stop("Missing elasticity or price-change value.")
    }
    
    demand_change_tbl <- x[, .(rel_change = sum(elasticity * price_change)), by = quantity]
    
    rel_change_evs <- setNames(
      demand_change_tbl$rel_change,
      demand_change_tbl$quantity
    )
    
    red_meat_rel_change <- prop_beef * rel_change_evs["beef"] +
      prop_pork * rel_change_evs["pork"]
    
    rel_change <- c(
      red_meat       = as.numeric(red_meat_rel_change),
      processed_meat = as.numeric(rel_change_evs["processed_meat"]),
      white_meat     = as.numeric(rel_change_evs["white_meat"]),
      fish           = as.numeric(rel_change_evs["fish"])
    )
    
    if (anyNA(rel_change)) {
      stop("Missing relative demand-change value.")
    }
    
    # Change in meat consumption after tax
    sp$pop[, red_meat_delta_xps :=
             red_meat_curr_xps - pmax(0, red_meat_curr_xps * (1 + rel_change["red_meat"]))]
    sp$pop[, processed_meat_delta_xps :=
             processed_meat_curr_xps - pmax(0, processed_meat_curr_xps * (1 + rel_change["processed_meat"]))]
    sp$pop[, white_meat_delta_xps :=
             white_meat_curr_xps - pmax(0, white_meat_curr_xps * (1 + rel_change["white_meat"]))]
    sp$pop[, fish_delta_xps :=
             fish_curr_xps - pmax(0, fish_curr_xps * (1 + rel_change["fish"]))]
    
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           red_meat_curr_xps := pmax(0, red_meat_curr_xps - red_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           processed_meat_curr_xps := pmax(0, processed_meat_curr_xps - processed_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           white_meat_curr_xps := pmax(0, white_meat_curr_xps - white_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           fish_curr_xps := pmax(0, fish_curr_xps - fish_delta_xps)]
    
    # Current annual environmental footprints after consumption changes
    meat_env_tbl <- read_fst("./xiao_testing/environment_footprints_meat.fst", as.data.table = TRUE)
    gday_to_kgyear <- 365 / 1000
    
    sp$pop[, red_meat_curr_ghg :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", ghg]]
    
    sp$pop[, processed_meat_curr_ghg :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", ghg]]
    
    sp$pop[, white_meat_curr_ghg :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", ghg]]
    
    sp$pop[, fish_curr_ghg :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", ghg]]
    
    
    sp$pop[, red_meat_curr_water :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", water]]
    
    sp$pop[, processed_meat_curr_water :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", water]]
    
    sp$pop[, white_meat_curr_water :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", water]]
    
    sp$pop[, fish_curr_water :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", water]]
    
    
    sp$pop[, red_meat_curr_land :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", land]]
    
    sp$pop[, processed_meat_curr_land :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", land]]
    
    sp$pop[, white_meat_curr_land :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", land]]
    
    sp$pop[, fish_curr_land :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", land]]
    
    # Environmental changes due to scenario-induced intake changes
    # Positive value = environmental reduction
    # Negative value = environmental increase, e.g. from substitution
    
    sp$pop[, red_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", ghg], 0)]
    
    sp$pop[, processed_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", ghg], 0)]
    
    sp$pop[, white_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", ghg], 0)]
    
    sp$pop[, fish_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", ghg], 0)]
    
    
    sp$pop[, red_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", water], 0)]
    
    sp$pop[, processed_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", water],0)]
    
    sp$pop[, white_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", water], 0)]
    
    sp$pop[, fish_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", water], 0)]
    
    
    sp$pop[, red_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", land], 0)]
    
    sp$pop[, processed_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", land], 0)]
    
    sp$pop[, white_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", land], 0)]
    
    sp$pop[, fish_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", land], 0)]
    
  }
  
  ### Scenario 3 - GHGE-based excise tax using EUR 55/t CO2eq -------------------
  
  scenario_3_fn <- function(sp) {
    
    # Set scenario variables (nEHS Emissionshandelspreis Deutschland)
    co2_price <- 55
    
    # Carbon emissions: kg CO2eq per kg edible meat
    co2eq_beef <- 13.4
    co2eq_pork <- 3.9
    co2eq_pm   <- 8.0
    
    # Retail prices: EUR/kg, 2025 euros
    retail_price_beef <- 12.65
    retail_price_pork <- 8.64
    retail_price_pm   <- 11.98
    
    # NAKO weighted red-meat composition
    prop_beef <- 0.513
    prop_pork <- 0.487
    
    # Define tax targets and substitutes of interest
    products <- c("beef", "pork", "processed_meat", "white_meat", "fish")
    
    tbl <- read_fst("./xiao_testing/tax_pass_through.fst", as.data.table = TRUE)
    pass_through <- as.numeric(tbl[mc == sp$mc_aggr, tax_pth])
    
    # Excise tax in EUR/kg product
    tax_beef <- co2eq_beef * co2_price / 1000
    tax_pork <- co2eq_pork * co2_price / 1000
    tax_pm   <- co2eq_pm   * co2_price / 1000
    
    # Tax-induced consumer-price change
    price_change_tbl <- data.table(
      price = c("beef", "pork", "processed_meat"),
      price_change = c(
        tax_beef * pass_through / retail_price_beef,
        tax_pork * pass_through / retail_price_pork,
        tax_pm   * pass_through / retail_price_pm
      )
    )
    
    # Calculate demand change
    tbl <- read_fst("./xiao_testing/evs_meat_price_elasticity_table.fst", as.data.table = TRUE)
    
    x <- tbl[quantity %in% products & price %in% price_change_tbl$price]
    x <- price_change_tbl[x, on = "price"]
    
    if (x[, anyNA(elasticity)] || x[, anyNA(price_change)]) {
      stop("Missing elasticity or price-change value.")
    }
    
    demand_change_tbl <- x[, .(rel_change = sum(elasticity * price_change)), by = quantity]
    
    rel_change_evs <- setNames(
      demand_change_tbl$rel_change,
      demand_change_tbl$quantity
    )
    
    red_meat_rel_change <- prop_beef * rel_change_evs["beef"] +
      prop_pork * rel_change_evs["pork"]
    
    rel_change <- c(
      red_meat       = as.numeric(red_meat_rel_change),
      processed_meat = as.numeric(rel_change_evs["processed_meat"]),
      white_meat     = as.numeric(rel_change_evs["white_meat"]),
      fish           = as.numeric(rel_change_evs["fish"])
    )
    
    if (anyNA(rel_change)) {
      stop("Missing relative demand-change value.")
    }
    
    # Change in meat and fish consumption after tax
    sp$pop[, red_meat_delta_xps :=
             red_meat_curr_xps - pmax(0, red_meat_curr_xps * (1 + rel_change["red_meat"]))]
    sp$pop[, processed_meat_delta_xps :=
             processed_meat_curr_xps - pmax(0, processed_meat_curr_xps * (1 + rel_change["processed_meat"]))]
    sp$pop[, white_meat_delta_xps :=
             white_meat_curr_xps - pmax(0, white_meat_curr_xps * (1 + rel_change["white_meat"]))]
    sp$pop[, fish_delta_xps :=
             fish_curr_xps - pmax(0, fish_curr_xps * (1 + rel_change["fish"]))]
    
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           red_meat_curr_xps := pmax(0, red_meat_curr_xps - red_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           processed_meat_curr_xps := pmax(0, processed_meat_curr_xps - processed_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           white_meat_curr_xps := pmax(0, white_meat_curr_xps - white_meat_delta_xps)]
    sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
           fish_curr_xps := pmax(0, fish_curr_xps - fish_delta_xps)]
    
    # Current annual environmental footprints after consumption changes
    meat_env_tbl <- read_fst("./xiao_testing/environment_footprints_meat.fst", as.data.table = TRUE)
    gday_to_kgyear <- 365 / 1000
    
    sp$pop[, red_meat_curr_ghg :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", ghg]]
    
    sp$pop[, processed_meat_curr_ghg :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", ghg]]
    
    sp$pop[, white_meat_curr_ghg :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", ghg]]
    
    sp$pop[, fish_curr_ghg :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", ghg]]
    
    
    sp$pop[, red_meat_curr_water :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", water]]
    
    sp$pop[, processed_meat_curr_water :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", water]]
    
    sp$pop[, white_meat_curr_water :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", water]]
    
    sp$pop[, fish_curr_water :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", water]]
    
    
    sp$pop[, red_meat_curr_land :=
             red_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", land]]
    
    sp$pop[, processed_meat_curr_land :=
             processed_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", land]]
    
    sp$pop[, white_meat_curr_land :=
             white_meat_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", land]]
    
    sp$pop[, fish_curr_land :=
             fish_curr_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", land]]
    
    # Environmental changes due to scenario-induced intake changes
    # Positive value = environmental reduction
    # Negative value = environmental increase, e.g. from substitution
    
    sp$pop[, red_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", ghg], 0)]
    
    sp$pop[, processed_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", ghg], 0)]
    
    sp$pop[, white_meat_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", ghg], 0)]
    
    sp$pop[, fish_delta_ghg :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", ghg], 0)]
    
    
    sp$pop[, red_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", water], 0)]
    
    sp$pop[, processed_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", water],0)]
    
    sp$pop[, white_meat_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", water], 0)]
    
    sp$pop[, fish_delta_water :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", water], 0)]
    
    
    sp$pop[, red_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     red_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "red_meat", land], 0)]
    
    sp$pop[, processed_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     processed_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "processed_meat", land], 0)]
    
    sp$pop[, white_meat_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     white_meat_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "white_meat", land], 0)]
    
    sp$pop[, fish_delta_land :=
             fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000),
                     fish_delta_xps * gday_to_kgyear * meat_env_tbl[meat == "fish", land], 0)]
    
  }

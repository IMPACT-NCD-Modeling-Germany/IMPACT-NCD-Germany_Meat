
  ##===============================================================================
  ## Purpose
  ## Generate toy data with 100 people aged between 20 and 90 years,
  ## synthesize for 10 years or until they reach 100 years, and
  ## test one policy scenario: 7%-19% VAT increase on red and processed meat
  ##===============================================================================
  
  if(!require(CKutils)){
    remotes::install_github("ChristK/CKutils")
    library(CKutils)
  }
  library(data.table)
  library(fst)
  library(dqrng)
  library(gamlss)
  
  source("./Rpackage/IMPACTncd_Ger_model_pkg/R/aux_functions.R")
  source("./xiao_testing/function_gamlss_twopm_validation_table.R")
  
  #---- Basic settings ----
  
  seed <- 1337L
  new_n <- 100L
  horizon <- 10L
  init_year <- 25L
  eps <- 1e-8
  
  set.seed(seed)
  dqset.seed(seed)
  
  #---- 1. Create toy synthetic population ----
  
  dt <- data.table(
    pid = seq_len(new_n),
    age0 = sample(20:90, new_n, replace = T),
    sex = factor(
      sample(rep(c("male", "female"), length.out = new_n)),
      levels = c("male", "female")
    ),
    year0 = init_year
  )
  
  summary(dt)

  cm_mean <- as.matrix(
    read_fst("./xiao_testing/exposure_corr_meat.fst", # for meat
             as.data.table = TRUE), rownames = "rn")
  
  rank_mtx <- generate_corr_unifs(new_n, cm_mean)
  
  summary(rank_mtx)
  
  # Avoid exact 0 or 1 for inverse CDF qfun()
  rank_mtx <- pmin(pmax(rank_mtx, eps), 1 - eps)
  rank_mtx <- as.data.table(rank_mtx)
  
  
  dt[, c("rank_processed_meat", "rank_red_meat", "rank_white_meat", "rank_fish") := rank_mtx]
  
  rm(rank_mtx)
  
  # Add non-correlated random numbers: WHY?
  rank_cols <- c("rankstat_white_meat", "rankstat_fish")
  for(nam in rank_cols)set(dt, NULL, nam, dqrunif(new_n))
  
  
  #---- 3. Expand dt by simulation years: 2025-2035 ----
  dt <- dt[, .(sim_year = 0 : horizon), 
           by = .(pid, age0, sex, year0, rank_processed_meat, rank_red_meat, 
                  rank_white_meat, 
                  rank_fish)]
  
  dt[, `:=`(age = age0 + sim_year,
            year = year0 + sim_year)]
  
  # 4.1. Generate processed meat (qBCPE, cut-off: 3) ----
  tbl <- read_fst("./xiao_testing/processed_meat_twopart_parameter_table.fst", as.data.table = TRUE)
  
  # Get parameter names for two-part model
  col_nam <- setdiff(names(tbl), intersect(names(dt), names(tbl)))
  
  # Join parameters with dt
  dt <- absorb_dt(dt, tbl)
  
  # Xiao: New function to get intake values based on ranks for two-part model
  dt[ , processed_meat_curr_xps := get_twopm_quantile( # Xiao: Naming fine or better "processed_meat"?
    p = rank_processed_meat,
    param_dt = .SD,
    qfun_nam = "qBCPE",
    pos_pars = setdiff(col_nam, "xi0"), # parameters for positive distribution only
    xi0_col = "xi0",
    cut_con = 3L,
    eps = eps), .SDcols = col_nam]
  
  dt[, (col_nam) := NULL]
  dt[, rank_processed_meat := NULL]
  
  
  # 4.2. Generate red meat (qBCTo, cut-off: 3) ----
  tbl <- read_fst("./xiao_testing/red_meat_twopart_parameter_table.fst", as.data.table = TRUE)
  
  # Get parameter names for two-part model
  col_nam <- setdiff(names(tbl), intersect(names(dt), names(tbl)))
  
  # Join parameters with dt
  dt <- absorb_dt(dt, tbl)
  
  # Xiao: New function to get intake values based on ranks for two-part model
  dt[ , red_meat_curr_xps := get_twopm_quantile(
    p = rank_red_meat,
    param_dt = .SD,
    qfun_nam = "qBCTo",
    pos_pars = setdiff(col_nam, "xi0"),
    xi0_col = "xi0",
    cut_con = 3L,
    eps = eps), .SDcols = col_nam]
  
  dt[, (col_nam) := NULL]
  dt[, rank_red_meat := NULL]
  
  
  # 4.3. Generate white meat (qGG, cut-off: 12) ----
  tbl <- read_fst("./xiao_testing/white_meat_twopart_parameter_table.fst", as.data.table = TRUE)
  
  # Get parameter names for two-part model
  col_nam <- setdiff(names(tbl), intersect(names(dt), names(tbl)))
  
  # Join parameters with dt
  dt <- absorb_dt(dt, tbl)
  
  # Xiao: New function to get intake values based on ranks for two-part model
  dt[ , white_meat_curr_xps := get_twopm_quantile(
    p = rank_white_meat,
    param_dt = .SD,
    qfun_nam = "qGG",
    pos_pars = setdiff(col_nam, "xi0"),
    xi0_col = "xi0",
    cut_con = 12L,
    eps = eps), .SDcols = col_nam]
  
  dt[, (col_nam) := NULL]
  dt[, rank_white_meat := NULL]
  
  # 4.4. Generate fish (qBCPE, cut-off: 5) ----
  tbl <- read_fst("./xiao_testing/fish_twopart_parameter_table.fst", as.data.table = TRUE)
  
  # Get parameter names for two-part model
  col_nam <- setdiff(names(tbl), intersect(names(dt), names(tbl)))
  
  # Join parameters with dt
  dt <- absorb_dt(dt, tbl)
  
  # Xiao: New function to get intake values based on ranks for two-part model
  dt[ , fish_curr_xps := get_twopm_quantile(
    p = rank_fish,
    param_dt = .SD,
    qfun_nam = "qBCPE",
    pos_pars = setdiff(col_nam, "xi0"),
    xi0_col = "xi0",
    cut_con = 5L,
    eps = eps), .SDcols = col_nam]
  
  dt[, (col_nam) := NULL]
  dt[, rank_fish := NULL]
  
  summary(dt)
  
  #### Scenarios for German SSB tax modelling ------------------------------------
    
  ### Scenario 1 - VAT increase from 7% to 19% on red and processed meat combined ----

    old_vat <- 0.07
    new_vat <- 0.19
    
    # Define tax targets and substitutes of interests
    products <- c("red_meat", "processed_meat", "white_meat", "fish")

    # --- Read pass-through ---
    pth_tbl <- read_fst(
      "./xiao_testing/tax_pass_through.fst",
      as.data.table = TRUE
    )
    
    pass_through <- as.numeric(
      pth_tbl[mc == 50L, tax_pth]
    )

    
    # --- VAT-induced consumer-price change ---
    vat_price_change <- ((1 + new_vat) / (1 + old_vat) - 1) * pass_through
    
    price_change_vec <- c(
      red_meat = vat_price_change,
      processed_meat = vat_price_change
    )

    # --- Read elasticity table ---
    elasticity_mc <- read_fst(
      "./xiao_testing/meat_price_elasticities_mc.fst",
      as.data.table = TRUE
    )
    
    # --- Calculate consumption changes ---
    price_change_tbl <- data.table(
      price = names(price_change_vec),
      price_change = as.numeric(price_change_vec)
    )
    
    x <- elasticity_mc[
      mc == 50L &
        quantity %in% products &
        price %in% names(price_change_vec)
    ]
    
    x <- price_change_tbl[x, on = "price"]
    
    if (x[, anyNA(elasticity_mc)] || x[, anyNA(price_change)]) {
      stop("Missing elasticity or price-change value.")
    }
    
    demand_change_tbl <- x[, .(rel_change = sum(elasticity_mc * price_change)), by = quantity]
    
    # Convert to named vector for convenient lookup
    rel_change <- setNames(
      demand_change_tbl$rel_change,
      demand_change_tbl$quantity
    )
    
    # --- Calculate new consumption ---
    dt[, red_meat_new_xps :=
             pmax(0, red_meat_curr_xps * (1 + rel_change["red_meat"]))]
    
    dt[, processed_meat_new_xps :=
             pmax(0, processed_meat_curr_xps * (1 + rel_change["processed_meat"]))]
    
    dt[, white_meat_new_xps :=
             pmax(0, white_meat_curr_xps * (1 + rel_change["white_meat"]))]
    
    dt[, fish_new_xps :=
             pmax(0, fish_curr_xps * (1 + rel_change["fish"]))]
    
    # Positive delta = reduction; negative delta = increase
    # Xiao: Why not new_xps - curr_xps, so that negative means clearly reduction?
    
    dt[, red_meat_delta_xps := red_meat_curr_xps - red_meat_new_xps]
    dt[, processed_meat_delta_xps := processed_meat_curr_xps - processed_meat_new_xps]
    dt[, white_meat_delta_xps := white_meat_curr_xps - white_meat_new_xps]
    dt[, fish_delta_xps := fish_curr_xps - fish_new_xps]
    
    int_year <- 2025 - 2000
    
    dt[year > int_year,`:=`(
      red_meat_curr_xps = red_meat_new_xps,
      processed_meat_curr_xps = processed_meat_new_xps,
      white_meat_curr_xps = white_meat_new_xps,
      fish_curr_xps = fish_new_xps)
    ]
    
    dt[, c("red_meat_new_xps",
               "processed_meat_new_xps",
               "white_meat_new_xps",
               "fish_new_xps") := NULL]
    
    summary(dt)
    
    
    # Mean reduction red meat 0.61 g/day --> 0.61*365 = 223g/year (very low to have any meaningful impact)



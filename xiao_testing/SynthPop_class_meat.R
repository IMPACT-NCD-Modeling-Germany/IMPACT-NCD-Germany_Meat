###=========================================================== 
# EXAMPLE DATASET
  # n_row_toy <- 100L
  # horizon <- 10L
  # init_year <- 25L
  # set.seed(1337L)
  # dqset.seed(1337L)
  # 
  # dt <- data.table(
  #   pid = seq_len(n_row_toy),
  #   age0 = sample(20:90, n_row_toy, replace = T),
  #   sex = factor(
  #     sample(rep(c("male", "female"), length.out = n_row_toy)),
  #     levels = c("male", "female")
  #   ),
  #   year0 = init_year
  # )

###===========================================================


  # Generate synthpops with sociodemographic and exposures information.
  
  dt <- self$gen_synthpop_demog(design_, month = "July")
  
  # NOTE!! from now on year in the short form i.e. 13 not 2013
  dt[, `:=`(pid  = .I)]
  new_n <- nrow(dt)
  
  
  # Generate correlated ranks for the individuals ----
  if (design_$sim_prm$logs)
    message("Generate correlated ranks for the individuals")
  
  cm_mean <- as.matrix(
    read_fst("./xiao_testing/exposure_corr_meat.fst",
             as.data.table = TRUE), rownames = "rn")

  if (design_$sim_prm$logs) message("generate correlated uniforms")
  rank_mtx <- generate_corr_unifs(new_n, cm_mean)
  

  # Avoid exact 0 or 1 for inverse CDF qfun()
  rank_mtx <- pmin(pmax(rank_mtx, 1e-8), 1 - 1e-8)
  

  if (design_$sim_prm$logs) message("correlated ranks matrix to data.table")
  rank_mtx <- data.table(rank_mtx)
  
  dt[, c("rank_processed_meat", "rank_red_meat", "rank_white_meat", "rank_fish") := rank_mtx]
  
  rm(rank_mtx)
  
  # NOTE rankstat_* is unaffected by the RW. Stay constant through the lifecourse
  rank_cols <-
    c(
      "rankstat_white_meat",     # Xiao: Not sure about why we need rankstat, only created for substitutes
      "rankstat_fish"
    )
  
  for (nam in rank_cols)
    set(dt, NULL, nam, dqrunif(new_n)) # NOTE do not replace with generate_rns function.
  
  # Project forward for simulation and back project for lags  ----
  
  # Xiao: I didn't change the following code for projection!
  
  if (design_$sim_prm$logs) message("Project forward and back project")
  
  dt <-
    clone_dt(dt,
             design_$sim_prm$sim_horizon_max +
               design_$sim_prm$maxlag + 1L)
  
  dt[.id <= design_$sim_prm$maxlag, `:=` (age  = age  - .id,
                                          year = year - .id)]
  dt[.id > design_$sim_prm$maxlag, `:=` (
    age  = age  + .id - design_$sim_prm$maxlag - 1L,
    year = year + .id - design_$sim_prm$maxlag - 1L
  )]
  # dt <-
  #   dt[between(age, design_$sim_prm$ageL - design_$sim_prm$maxlag, design_$sim_prm$ageH)]
  # delete unnecessary ages
  del_dt_rows(
    dt,
    !between(
      dt$age,
      design_$sim_prm$ageL - design_$sim_prm$maxlag,
      design_$sim_prm$ageH
    ),
    environment()
  )
  
  dt[, `:=` (.id = NULL)]
  
  if (max(dt$age) > 90L) {
    dt[, age100 := age]
    dt[age > 90L, age := 90L]
  }
  
  # to_agegrp(dt, 20L, 85L, "age", "agegrp20", to_factor = TRUE)
  # to_agegrp(dt, 10L, 85L, "age", "agegrp10", to_factor = TRUE)
  # to_agegrp(dt,  5L, 85L, "age", "agegrp5" , to_factor = TRUE)
  
  # Simulate exposures -----
  
  # Random walk for ranks ----
  if (design_$sim_prm$logs) message("Random walk for ranks")
  
  setkeyv(dt, c("pid", "year"))
  setindexv(dt, c("age", "sex")) #STRATA
  
  dt[, pid_mrk := mk_new_simulant_markers(pid)]
  
  dt[, lapply(.SD,
              fscramble_trajectories,
              pid_mrk,
              design_$sim_prm$jumpiness),
     .SDcols = patterns("^rank_")]
  # ggplot2::qplot(year, rank_ssb, data = dt[pid %in% sample(1e1, 1)], ylim = c(0,1))
  
  
  # Generate processed meat (qBCPE, cut-off: 3) ----
  if (design_$sim_prm$logs) message("Generate processed meat")
  
  tbl <-
    read_fst("./inputs/exposure_distributions/processed_meat_twopart_parameter_table.fst", as.data.table = TRUE)
  
  col_nam <-
    setdiff(names(tbl), intersect(names(dt), names(tbl)))
  #if (Sys.info()["sysname"] == "Linux") {
  #lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE)
  #} else {
  dt <- absorb_dt(dt, tbl)
  #}
  #dt <- merge(dt, tbl, by = c(intersect(names(dt), names(tbl))))
  
  # Xiao: New function to get intake values based on ranks for two-part model
  dt[ , processed_meat_curr_xps := get_twopm_quantile( # Xiao: naming fine? Or "processed_meat"?
    p = rank_processed_meat,
    param_dt = .SD,
    qfun_nam = "qBCPE",
    pos_pars = setdiff(col_nam, "xi0"), # parameters for positive distribution only
    xi0_col = "xi0", # parameter for binary model (prob of below cut-off: yes/no)
    cut_con = 3L), .SDcols = col_nam]
  
  dt[, (col_nam) := NULL]
  dt[, rank_processed_meat := NULL]

  
  # Generate red meat (qBCTo, cut-off: 3) ----
  if (design_$sim_prm$logs) message("Generate red meat")
  tbl <-
    read_fst("./inputs/exposure_distributions/red_meat_twopart_parameter_table.fst", as.data.table = TRUE)
  
  col_nam <-
    setdiff(names(tbl), intersect(names(dt), names(tbl)))
  #if (Sys.info()["sysname"] == "Linux") {
  #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE)
  #} else {
  dt <- absorb_dt(dt, tbl)
  #}
  
  dt[ , red_meat_curr_xps := get_twopm_quantile(
    p = rank_red_meat,
    param_dt = .SD,
    qfun_nam = "qBCTo",
    pos_pars = setdiff(col_nam, "xi0"),
    xi0_col = "xi0",
    cut_con = 3L), .SDcols = col_nam]
  
  dt[, (col_nam) := NULL]
  dt[, rank_red_meat := NULL]
  
  
  # Generate white meat (qGG, cut-off: 12) ----
  if (design_$sim_prm$logs) message("Generate white meat")
  
  tbl <-
    read_fst("./inputs/exposure_distributions/white_meat_twopart_parameter_table.fst",
             as.data.table = TRUE)
  col_nam <-
    setdiff(names(tbl), intersect(names(dt), names(tbl)))
  #if (Sys.info()["sysname"] == "Linux") {
  #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
  #} else {
  dt <- absorb_dt(dt, tbl)
  #}
  
  dt[ , white_meat_curr_xps := get_twopm_quantile(
    p = rank_white_meat,
    param_dt = .SD,
    qfun_nam = "qGG",
    pos_pars = setdiff(col_nam, "xi0"),
    xi0_col = "xi0",
    cut_con = 12L), .SDcols = col_nam]
  
  dt[, (col_nam) := NULL]
  dt[, rank_white_meat := NULL]
  
  
  
  # Generate fish (qBCPE, cut-off: 5) ----
  if (design_$sim_prm$logs) message("Generate fish")
  
  tbl <-
    read_fst("./inputs/exposure_distributions/fish_twopart_parameter_table.fst",
             as.data.table = TRUE)
  col_nam <-
    setdiff(names(tbl), intersect(names(dt), names(tbl)))
  #if (Sys.info()["sysname"] == "Linux") {
  #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
  #} else {
  dt <- absorb_dt(dt, tbl)
  #}
  
  dt[ , fish_curr_xps := get_twopm_quantile(
    p = rank_fish,
    param_dt = .SD,
    qfun_nam = "qBCPE",
    pos_pars = setdiff(col_nam, "xi0"),
    xi0_col = "xi0",
    cut_con = 5L), .SDcols = col_nam]
  
  dt[, (col_nam) := NULL]
  dt[, rank_fish := NULL]
  
  ###################################################################################################################

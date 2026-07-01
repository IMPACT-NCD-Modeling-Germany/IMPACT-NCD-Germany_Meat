  ###---- Helper functions for manual two-part GAMLSS validation ----
  ### Purpose:
  ###   Part I:  p <= xi0 -> assigned to zero using random assignment dqrunif
  ###   Part II: p > xi0 -> generated from positive-consumer GAMLSS

  # Generate correct quantiles for cut-off based two-part model
  get_twopm_quantile <- function(p,
                                 param_dt,
                                 qfun_nam,
                                 pos_pars,
                                 xi0_col = "xi0",
                                 cut_con = NULL,
                                 eps = 1e-8) {
    stopifnot(is.data.table(param_dt))
    
    if (!xi0_col %in% names(param_dt)) {
      stop(paste0("Column '", xi0_col, "' is not present in param_dt."))
    }
    if (!all(pos_pars %in% names(param_dt))) {
      stop("Not all pos_pars are present in param_dt.")
    }
    if (length(p) != nrow(param_dt)) {
      stop("Length of p must equal nrow(param_dt).")
    }
    
    qfun <- get(qfun_nam, mode = "function")
    if (!is.null(cut_con)) {
      pfun_nam <- sub("^q", "p", qfun_nam)
      pfun <- get(pfun_nam, mode = "function")
    }
    
    # Avoid exact 0/1 because some q-functions are unstable at the boundaries.
    p <- pmin(pmax(p, eps), 1 - eps)
    xi0 <- pmin(pmax(param_dt[[xi0_col]], eps), 1 - eps)
    
    out <- numeric(length(p))
    
    # Below-cut-off part: assigned to zero.
    zero_idx <- p <= xi0
    out[zero_idx] <- 0
    
    # Positive part: conditional percentile within the positive-consumer part.
    pos_idx <- !zero_idx
    
    if (any(pos_idx)) {
      # Step 1: Map to the positive distribution
      p_pos <- (p[pos_idx] - xi0[pos_idx]) / (1 - xi0[pos_idx])
      p_pos <- pmin(pmax(p_pos, eps), 1 - eps)
      
      args <- as.list(param_dt[pos_idx, ..pos_pars])
      
      # Step 2: If cut_con is provided, rescaling to prevent values from being lower than the percentile of cut-off.
      if (!is.null(cut_con)) {
        args_cut <- args
        args_cut$q <- rep(cut_con, sum(pos_idx))
        p_cut <- do.call(pfun, args_cut) # percentile of a value <= cut_con
        p_cut <- pmin(pmax(p_cut, 0), 1 - eps)
        
        p_pos <- p_cut + p_pos * (1 - p_cut) # remove values that are below cut_con = rescale the percentiles conditional on being above the cut-off
        p_pos <- pmin(pmax(p_pos, eps), 1 - eps) # avoid exact 0/1
      }
      
      args$p <- p_pos
      out[pos_idx] <- do.call(qfun, args)
      
      # Last numerical guard. qfun should already respect this after truncation.
      if (!is.null(cut_con)) {
        out[pos_idx] <- pmax(out[pos_idx], cut_con)
      }
    }
    
    out
  }
  
  # Build validation plot with observed and modeled values based on quantile function
  validate_twopart_gamlss_tbl <- function(dt,
                                          twopart_tbl,
                                          mc = 10L,
                                          colname,
                                          distr_nam = NULL,
                                          qfun_nam = NULL,
                                          pos_pars = NULL,
                                          by_vars = c("age", "sex"), # Keep that for future new variables like year or SES
                                          xi0_col = "xi0",
                                          cut_con = NULL,
                                          wt_col = "pop_weight",
                                          eps = 1e-8) {
    stopifnot(is.data.table(dt), is.data.table(twopart_tbl), mc >= 1)
    
    # Keep compatibility with the previous argument name qfun_nam.
    if (is.null(distr_nam)) {
      if (!is.null(qfun_nam)) {
        distr_nam <- qfun_nam
      } else {
        stop("Please provide distr_nam, e.g. paste0('q', pm_final_gamlss_model$family[1]).")
      }
    }
    
    if (!colname %in% names(dt)) {
      stop(
        paste0(
          "Column '", colname, "' is not present in dt. ",
          "Create the observed threshold-adjusted variable before validation, e.g. ",
          "dt[, ", colname, " := fifelse(pm_hi == 1L, processed_meat, 0)]."
        )
      )
    }
    if (!all(by_vars %in% names(dt))) {
      stop("Not all by_vars are present in dt.")
    }
    if (!all(by_vars %in% names(twopart_tbl))) {
      stop("Not all by_vars are present in twopart_tbl.")
    }
    if (!xi0_col %in% names(twopart_tbl)) {
      stop(paste0("Column '", xi0_col, "' is not present in twopart_tbl."))
    }
    
    if (is.null(pos_pars)) {
      pos_pars <- intersect(c("mu", "sigma", "nu", "tau"), names(twopart_tbl))
    }
    if (!all(pos_pars %in% names(twopart_tbl))) {
      stop("Not all pos_pars are present in twopart_tbl.")
    }

      
    # Only join required parameter columns; avoid accidental joins or duplicate columns.
    keep_cols <- unique(c(by_vars, pos_pars, xi0_col))
    keep_cols <- intersect(keep_cols, names(twopart_tbl))
    
    val_dt <- twopart_tbl[, .SD, .SDcols = keep_cols][
      dt,
      on = by_vars
    ]
    
    needed_cols <- c(pos_pars, xi0_col)
    if (val_dt[, anyNA(.SD), .SDcols = needed_cols]) {
      stop("Some rows in dt could not be matched to the two-part parameter table.")
    }
    if (val_dt[, any(get(xi0_col) < 0 | get(xi0_col) > 1)]) {
      stop(paste0("Column '", xi0_col, "' contains values outside [0, 1]."))
    }
    
    # Observed part: one copy of observed threshold-adjusted values.
    obs <- copy(val_dt)
    obs[, type := "Observed"]
    
    # Modelled part: mc generated values per observed row.
    mod <- val_dt[rep(seq_len(.N), times = mc)]
    mod[, type := "Modelled"]
    mod[, p := dqrng::dqrunif(.N, eps, 1 - eps)]
    
    mod[
      ,
      (colname) := get_twopm_quantile(
        p = p,
        param_dt = .SD,
        qfun_nam = distr_nam,
        pos_pars = pos_pars,
        xi0_col = xi0_col,
        cut_con = cut_con,
        eps = eps
      ),
      .SDcols = c("p", xi0_col, pos_pars)
    ]
    
    # Drop helper/model parameter columns
    drop_cols <- c("p", pos_pars, "xi0")
    mod[, (drop_cols) := NULL]
    obs[, (setdiff(drop_cols, "p")) := NULL]
    
    out <- rbind(obs, mod, use.names = TRUE, fill = TRUE)
    out[]
}
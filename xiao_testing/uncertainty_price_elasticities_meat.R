  library(data.table)
  library(fst)
  library(dqrng)
  
  # ---- 1. Create price elasticity table based on Schönbach et al. (2019) ----
  # Tab 1. Marshallian unconditional oPE and cPE using GfK consumer panel 2011
  
  products <- c("red_meat", "white_meat", "fish", "processed_meat")
  
  est_mat <- matrix(
    c(
      -0.694, -0.128, -0.020,  0.031,
      -0.179, -0.487, -0.031,  0.027,
      0.059, -0.023, -0.477, -0.161,
      0.251,  0.083, -0.255, -0.699
    ),
    nrow = 4,
    byrow = T,
    dimnames = list(products, products)
  )
  
  lcl_mat <- matrix(
    c(
      -0.722, -0.146, -0.035, 0.017,
      -0.218, -0.524, -0.054,  0.006,
      0.014, -0.045, -0.517, -0.185,
      0.189,  0.037, -0.293, -0.756
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(products, products)
  )
  
  ucl_mat <- matrix(
    c(
      -0.665, -0.110, -0.005, 0.045,
      -0.141, -0.450, -0.008,  0.048,
      0.103,  0.008, -0.438, -0.137,
      0.314,  0.128, -0.217, -0.642
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(products, products)
  )
  
  meat_elasticity_tbl <- CJ(
    quantity = products,
    price = products
  )
  
  meat_elasticity_tbl[, elasticity := est_mat[cbind(quantity, price)]]
  meat_elasticity_tbl[, lcl := lcl_mat[cbind(quantity, price)]]
  meat_elasticity_tbl[, ucl := ucl_mat[cbind(quantity, price)]]
  
  ## Approximate SE from 95% CI, assuming normality
  meat_elasticity_tbl[, se := (ucl - lcl) / (2 * 1.96)]
  
  meat_elasticity_tbl[, elasticity_type := fifelse(quantity == price, "own_price", "cross_price")]

  write_fst(meat_elasticity_tbl, "./xiao_testing/meat_price_elasticity_table.fst", compress = 100)
  
  # ---- 2. Add uncertainties following SSB PE logic----

  n_samples <- 10000L
  
  ## Ensure replicability
  set.seed(log(n_samples) + 1337) # Seed is parameter-specific!
  dqrng::dqset.seed(log(n_samples) + 1337)
  
  ## Draw quantiles
  quantiles <- runif(n_samples, min = 0, max = 1) * 0.999
  
  ## Expand each row to n_sample rows
  meat_elasticity_mc <- meat_elasticity_tbl[rep(seq_len(.N), times = n_samples)]
  
  setorder(meat_elasticity_mc, quantity, price)
  
  ## Generate elasticity with uncertainty
  meat_elasticity_mc[,  `:=` (
    mc = rep(seq_len(n_samples), times = nrow(meat_elasticity_tbl)),
    elasticity_mc = qnorm(
    p = rep(quantiles, times = nrow(meat_elasticity_tbl)),
    mean = elasticity,
    sd = se
  ))]
  
  write_fst(meat_elasticity_mc,"./xiao_testing/meat_price_elasticities_mc.fst", compress = 100)

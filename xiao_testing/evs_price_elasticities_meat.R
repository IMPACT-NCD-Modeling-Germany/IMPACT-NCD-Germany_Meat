  # Products in the new EVS 2018 elasticity matrix
# No 95% 
  products <- c("beef", "pork", "processed_meat", "white_meat", "fish")
  
  evs_elasticity_tbl <- CJ(
    quantity = products,
    price    = products
  )
  
  est_mat <- matrix(
    c(
      -0.788,  0.091, -0.092,  0.037, -0.060,
      0.102, -1.324, -0.233, -0.012,  0.055,
      -0.023, -0.087, -1.492,  0.034,  0.007,
      0.044, -0.016,  0.080, -1.118,  0.078,
      -0.071,  0.062,  0.005,  0.089, -1.020
    ),
    nrow = 5,
    byrow = TRUE,
    dimnames = list(products, products)
  )
  
  evs_elasticity_tbl[, elasticity := est_mat[cbind(quantity, price)]]
  evs_elasticity_tbl[, elasticity_type := fifelse(quantity == price, "own_price", "cross_price")]
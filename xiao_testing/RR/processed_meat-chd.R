---
  xps_name: processed meat
outcome: chd
lag: 5 # TBD! 4 used in IMPACT England for foods
distribution: lognormal
source: DOI:10.1016/S0140-6736(20)30925-9; GBD 2019
notes:
  - RR per 50g/day
- Ideal intake: 0-15 according to GBD 2021, GBD 2019, GBD 2017 + FBDG recommendations
apply_rr_extra_fn: >
  function(sp) {
    if (!inherits(sp, 'SynthPop')) stop('Argument sp needs to be a SynthPop object.')
    sp$pop[, processed_meat_rr := clamp(processed_meat_rr^((processed_meat - self$get_ideal_xps_lvl(sp$mc_aggr)) / 50), 1, 20)]
  }
ideal_xps_lvl_fn: > # Define ideal intake with uncertainty [0, 15]
  function(design_) { # from:
    if (!inherits(design_, 'Design')) stop('Argument design needs to be a Design object.')
    save.seed <- get('.Random.seed', .GlobalEnv)
    set.seed(851747L) # Same for stroke, CHD & T2DM
    res <-  sample(x = 0:15, size = design$sim_prm$iteration_n_max, replace = TRUE)
    assign('.Random.seed', save.seed, .GlobalEnv)
    res
  }
---
  agegroup,sex,rr,ci_rr
<1,men,1,1
01-04,men,1,1
05-09,men,1,1
10-14,men,1,1
15-19,men,1,1
20-24,men,1,1
25-29,men,1.17,1.31
30-34,men,1.17,1.31
35-39,men,1.17,1.31
40-44,men,1.13,1.25
45-49,men,1.13,1.24
50-54,men,1.12,1.22
55-59,men,1.10,1.18
60-64,men,1.09,1.16
65-69,men,1.08,1.14
70-74,men,1.07,1.13
75-79,men,1.06,1.12
80-84,men,1.06,1.12
85-89,men,1.06,1.12
90+,men,1.06,1.12
<1,women,1,1
01-04,women,1,1
05-09,women,1,1
10-14,women,1,1
15-19,women,1,1
20-24,women,1,1
25-29,women,1.17,1.31
30-34,women,1.17,1.31
35-39,women,1.17,1.31
40-44,women,1.13,1.25
45-49,women,1.13,1.24
50-54,women,1.12,1.22
55-59,women,1.10,1.18
60-64,women,1.09,1.16
65-69,women,1.08,1.14
70-74,women,1.07,1.13
75-79,women,1.06,1.12
80-84,women,1.06,1.12
85-89,women,1.06,1.12
90+,women,1.06,1.12

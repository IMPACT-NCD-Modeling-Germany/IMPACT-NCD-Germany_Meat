---
  xps_name: red meat
outcome: CRC
lag: 5 # TBD!
distribution: lognormal
source: https://www.wcrf.org/wp-content/uploads/2024/10/Colorectal-cancer-report.pdf; Figure 8 + Table 13
notes:
  - TBD: Sex-stratified results possible but insignificant!
  - RR per 100g/day
- Ideal intake: 0-200 according to GBD 2021 and GBD 2019
apply_rr_extra_fn: >
  function(sp) {
    if (!inherits(sp, 'SynthPop')) stop('Argument sp needs to be a SynthPop object.')
    sp$pop[, red_meat_rr := clamp(red_meat_rr^((red_meat - self$get_ideal_xps_lvl(sp$mc_aggr)) / 100), 1, 20)]
  }
ideal_xps_lvl_fn: >
  function(design_) { # from:
    if (!inherits(design_, 'Design')) stop('Argument design needs to be a Design object.')
    save.seed <- get('.Random.seed', .GlobalEnv)
    set.seed(851747L) # Same for stroke, CHD & T2DM
    res <- sample(x = 0:200, size = design$sim_prm$iteration_n_max, replace = TRUE)
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
25-29,men,1.12,1.25
30-34,men,1.12,1.25
35-39,men,1.12,1.25
40-44,men,1.12,1.25
45-49,men,1.12,1.25
50-54,men,1.12,1.25
55-59,men,1.12,1.25
60-64,men,1.12,1.25
65-69,men,1.12,1.25
70-74,men,1.12,1.25
75-79,men,1.12,1.25
80-84,men,1.12,1.25
85-89,men,1.12,1.25
90+,men,1.12,1.25
<1,women,1,1
01-04,women,1,1
05-09,women,1,1
10-14,women,1,1
15-19,women,1,1
20-24,women,1,1
25-29,women,1.12,1.25
30-34,women,1.12,1.25
35-39,women,1.12,1.25
40-44,women,1.12,1.25
45-49,women,1.12,1.25
50-54,women,1.12,1.25
55-59,women,1.12,1.25
60-64,women,1.12,1.25
65-69,women,1.12,1.25
70-74,women,1.12,1.25
75-79,women,1.12,1.25
80-84,women,1.12,1.25
85-89,women,1.12,1.25
90+,women,1.12,1.25

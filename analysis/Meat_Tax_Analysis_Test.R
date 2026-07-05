
#### Analysis script IMPACT NCD Germany GLP1 modeling ----

# Load model packages
source("./global.R")

# Load scenario and sensitivity analyses functions
# source("./auxil/scenarios_GLP_uncertain.R") # Done: split up into different files

# Initiate .Random.seed for safety
runif(10)
set.seed(1337)

# New runs?
new_runs <- TRUE
new_export <- TRUE


if(new_runs){
  
  # Create batches for batched simulation
  batch_size <- 5
  iterations <- 10
  first_iteration <- 1
  batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                   f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                    vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))
}

###################################################################################################
#-------------------------------------------------------------------------------------------------#
#----------------------- Step 1: generate the lifecourse with only sc0 ---------------------------#
#-------------------------------------------------------------------------------------------------#
###################################################################################################

analysis_name <- "Meat_Tax" 
### create a folder to store all the output of this analysis

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

# TODO load scenario script here   
source("./auxil/scenarios_GLP_uncertain_sc0.R") 

if(new_runs){
  
  for (i in batches){
    
    message("Running iteration ", i)
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0) 
    
    scenario_fn <- scenario_1_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)
    
    scenario_fn <- scenario_2_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)

  }
}


if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE) 
} 


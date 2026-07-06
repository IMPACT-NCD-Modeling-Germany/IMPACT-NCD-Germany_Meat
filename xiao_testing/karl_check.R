### Testing Xiaos Files ###

library(fst)

# Exposure files

corr <- read_fst("./xiao_testing/preparation_EUHEA/exposure_corr_meat.fst") # Check

fish_table <- read_fst("./xiao_testing/preparation_EUHEA/fish_twopart_parameter_table.fst") # Check

processed_meat_table <- read_fst("./xiao_testing/preparation_EUHEA/processed_meat_twopart_parameter_table.fst") # Check

red_meat_table <- read_fst("./xiao_testing/preparation_EUHEA/red_meat_twopart_parameter_table.fst") # Check

white_meat_table <- read_fst("./xiao_testing/preparation_EUHEA/white_meat_twopart_parameter_table.fst") # Check

# Elasticities + Price

pe <- read_fst("./xiao_testing/preparation_EUHEA/meat_price_elasticities_mc.fst") # Check

pass <- read_fst("./xiao_testing/preparation_EUHEA/tax_pass_through.fst") # Check

# RRs

## CAVE: csvy naimg, xps naming, tabs and spaces, runif

# --- Processed meat -> stroke: Check
# --- Processed meat -> chd: Check
# --- Processed meat -> t2dm: Check

# --- Red meat -> chd: Check
# --- Red meat -> stroke: Check
# --- Red meat -> t2dm: Check

library(simcompl2)
source("R/robustness_parameters.R")
big_main_simulation = 
  do.call(run_simulation, list(data_params = data_params,
                               test_params = test_params,
                               sim_params = sim_params))
library(simcompl2)
source("R/mixed_complement_parameters.R")
mixed_complement_simulation = 
  do.call(run_simulation, list(data_params = data_params,
                               test_params = test_params,
                               sim_params = sim_params))

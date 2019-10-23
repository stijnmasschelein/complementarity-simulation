library(simcompl2)
source("R/contingent_complement_parameters.R")
big_mixed_simulation = 
  do.call(run_simulation, list(data_params = data_params,
                               test_params = test_params,
                               sim_params = sim_params))

library(simcompl2)
source("R/big_parameters.R")
sim_params$nsim = 2; sim_params$mc_cores = 4

big_main_simulation = 
  do.call(run_simulation, list(data_params = data_params,
                               test_params = test_params,
                               sim_params = sim_params))
saveRDS(big_main_simulation, "simulated_data/big_main_simulation.Rds")
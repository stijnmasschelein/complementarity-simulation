library(simcompl2)
source("R/big_parameters.R")
big_main_simulation = 
  do.call(run_simulation, list(data_params = data_params,
                               test_params = test_params,
                               sim_params = sim_params))
saveRDS(big_main_simulation, "simulated_data/big_main_simulation.Rds")
pbPost("note", "Big Main Simulation", 
       "It's done. You should close the AWS instance and commit 
       and push the result from the small instance.")

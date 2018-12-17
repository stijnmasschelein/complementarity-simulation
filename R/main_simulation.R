source("R/parameters.R")
library(simcompl2)
main_simulation = do.call(run_simulation, list(data_params = data_params,
                                               test_params = test_params,
                                               sim_params = sim_params))
saveRDS(main_simulation, "simulated_data/main_simulation.Rds")
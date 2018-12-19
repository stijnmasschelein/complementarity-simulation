source("R/parameters.R")
library(simcompl2)
test_params_new = test_params[1:2]
test_params_new[[1]]$nearly_correction = TRUE
test_params_new[[2]]$nearly_correction = TRUE
nearly_simulation = do.call(simcompl2::run_simulation, 
                            list(data_params = data_params,
                                 test_params = test_params_new, 
                                 sim_params = sim_params))
saveRDS(nearly_simulation,
        file = "simulated_data/nearly_simulation.RDS")


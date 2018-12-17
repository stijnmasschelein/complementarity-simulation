source("R/parameters.R")
library(simcompl2)
data_params_new = data_params
data_params_new$g1 = data_params$g1[2:3]
data_params_new$rate = data_params$rate[c(1, 3, 5)]
data_params_new$sd = list(1, 2, 4)
noise_simulation = do.call(run_simulation, list(data_params = data_params_new,
                                                test_params = test_params,
                                                sim_params = sim_params))
saveRDS(noise_simulation, "simulated_data/noise_simulation.Rds")
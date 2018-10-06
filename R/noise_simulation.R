source("R/parameters.R")
library(simcompl)
params_new = params
params_new$g1 = params$g1[2:3]
params_new$rate = params$rate[c(1, 3, 5)]
params_new$sd = list(1, 2, 4)
noise_simulation = do.call(run_sim, params_new)
saveRDS(noise_simulation, "simulated_data/noise_simulation.Rds")
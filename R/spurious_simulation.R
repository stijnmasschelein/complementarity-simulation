source("R/parameters.R")
library(simcompl2)
data_params_new = data_params
data_params_new$g1 = data_params$g1[1]
data_params_new$g2 = list(c(.3, -.3, 0), c(.3, -.2, 0), c(.3, -.1, 0))
data_params_new$rate = data_params$rate[c(1, 3, 5)]
test_params_new <- test_params[1:2]
spurious_simulation = do.call(run_simulation, 
                              list(data_params = data_params_new,
                                   test_params = test_params_new,
                                   sim_params = sim_params))
saveRDS(spurious_simulation, "simulated_data/spurious_simulation.Rds")
source("R/parameters.R")
library(simcompl2)

data_params_new = data_params
data_params_new$rate = data_params$rate[c(1, 3, 5)]
data_params_new$g1 = data_params$g1[c(2, 3)]
test_params_new = test_params[2]
sim_params_new = sim_params 
sim_params_new$boot = TRUE
sim_params_new$bootR = 2000
boot_simulation = do.call(run_simulation, 
                          list(data_params = data_params_new,
                               test_params = test_params_new, 
                               sim_params = sim_params_new))
saveRDS(boot_simulation, "simulated_data/boot_simulation.Rds")
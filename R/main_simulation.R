source("R/parameters.R")
library(simcompl)
main_simulation = do.call(run_sim, params)
saveRDS(main_simulation, "simulated_data/main_simulation.Rds")
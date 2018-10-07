source("R/parameters.R")
library(simcompl)
params_new = params
params_new$g1 = params$g1[2:3]
params_new$rate = params$rate[c(1, 3, 5)]
params_new$d = list(c(0, 0, 0), c(.25, .25, 0))
method <- list("match", "interaction_control", "interaction_moderation",
               "interaction_moderationaugmented")
delta_simulation = do.call(run_sim, params_new)
saveRDS(delta_simulation, "simulated_data/delta_simulation.Rds")
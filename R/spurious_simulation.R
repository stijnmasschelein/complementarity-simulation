source("R/parameters.R")
library(simcompl)
params_new = params
params_new$g1 = params$g1[1]
params_new$g2 = list(c(.3, -.3, 0), c(.3, -.2, 0), c(.3, -.1, 0))
params_new$rate = params$rate[c(1, 3, 5)]

method <- list("match", "interaction_moderationaugmented")
spurious_simulation = do.call(run_sim, params_new)
saveRDS(spurious_simulation, "simulated_data/spurious_simulation.Rds")
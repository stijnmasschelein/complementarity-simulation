if (!require("remotes")){
  install.packages("remotes")
  library(remotes)
}
if (!require(simcompl2)){
  remotes::install_github("stijnmasschelein/simcompl2",
                          ref = "only_files")
  library(simcompl2)
}
source("R/big_parameters.R")
data_params_1 <- data_params
data_params_1$b1 = list(c(.5, .5, .5, 0))
data_params_1$d[[2]] = list(c(.25, .25, 0))

do.call(run_simulation, list(data_params = data_params_1,
                               test_params = test_params,
                               sim_params = sim_params))

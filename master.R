rerun_simulation = FALSE
options(stringsAsFactors = FALSE)
source("R/sample_descriptives.R")

if (rerun_simulation){
  source("R/main_simulation.R")
  source("R/noise_simulation.R")
  source("R/delta_simulation.R")
}
source("R/main_results.R")

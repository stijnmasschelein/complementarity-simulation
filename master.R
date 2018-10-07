rerun_simulation = FALSE

if (rerun_simulation){
  source("R/sample_descriptives.R")
  source("R/main_simulation.R")
  source("R/noise_simulation.R")
  source("R/delta_simulation.R")
  source("R/spurious_simulation.R")
}
source("R/main_results.R")
source("R/noise_results.R")
source("R/delta_results.R")
source("R/spurious_results.R")
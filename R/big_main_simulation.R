library(simcompl2)
source("R/big_parameters.R")
start_time = Sys.time()
big_main_simulation = 
  do.call(run_simulation, list(data_params = data_params,
                               test_params = test_params,
                               sim_params = sim_params))
end_time = Sys.time()
msg = end_time - start_time
pbPost("note", "Big Main Simulation", 
       paste("It's done. You should close the AWS instance and commit and push the result from the small instance.", 
       msg, sep = "\n"))

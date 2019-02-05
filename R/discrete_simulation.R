source("R/parameters.R")
library(simcompl2)
data_params_new = data_params
data_params_new$rate = data_params$rate[1:4]
data_params_new$xinteger = list(c(TRUE, TRUE, FALSE))
discrete_demand = I(x1 == 1) ~ x2 + z 
test_params_new = list(list(formula = x1 ~ x2 + z, 
                        variable = "x2",
                        label = "demand",
                        nearly_correction = TRUE),
                   list(formula = perf_form2,
                        variable = "x1:x2",
                        label = "performance~2",
                        nearly_correction = TRUE),
                   list(formula = discrete_demand,
                        variable = "x2",
                        family = binomial(link = "logit"),
                        label = "logit",
                        nearly_correction = FALSE),
                   list(formula = discrete_demand,
                        variable = "x2",
                        family = binomial(link = "probit"),
                        label = "probit",
                        nearly_correction = FALSE))

discrete_simulation = do.call(run_simulation, 
                              list(data_params = data_params_new,
                                   test_params = test_params_new,
                                   sim_params = sim_params))
saveRDS(discrete_simulation, "simulated_data/discrete_simulation.Rds")

source("R/big_parameters.R")
data_params_1 <- data_params
data_params_1$sd = data_params$sd[3]
data_params_1$b1 = data_params$b1[4]
data_params_2 <- data_params
data_params_2$sd = data_params$sd[3]
data_params_2$b1 = data_params$b1[3]
data_params_2$sd_eps = data_params$sd_eps[3]
data_params_3 <- data_params
data_params_3$sd = data_params$sd[3]
data_params_3$b1 = data_params$b1[3]
data_params_3$sd_eps = data_params$sd_eps[2]
data_params_3$d = data_params$d[3:4]
data_params_4<- data_params
data_params_4$sd = data_params$sd[3]
data_params_4$b1 = data_params$b1[3]
data_params_4$sd_eps = data_params$sd_eps[2]
data_params_4$d = data_params$d[2]
data_params_4$g1 = data_params$g1[3]
data_params_5<- data_params
data_params_5$sd = data_params$sd[3]
data_params_5$b1 = data_params$b1[3]
data_params_5$sd_eps = data_params$sd_eps[2]
data_params_5$d = data_params$d[2]
data_params_5$g1 = data_params$g1[2]
data_params_5$b2 = data_params$b2[3]
data_params_5$rate = data_params$rate[2:5]

do.call(run_simulation, list(data_params = data_params_1,
                               test_params = test_params,
                               sim_params = sim_params))
do.call(run_simulation, list(data_params = data_params_2,
                               test_params = test_params,
                               sim_params = sim_params))
do.call(run_simulation, list(data_params = data_params_3,
                               test_params = test_params,
                               sim_params = sim_params))
do.call(run_simulation, list(data_params = data_params_4,
                               test_params = test_params,
                               sim_params = sim_params))
do.call(run_simulation, list(data_params = data_params_5,
                               test_params = test_params,
                               sim_params = sim_params))
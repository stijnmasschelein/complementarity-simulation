nobs    = list(300)
g1      = list(c(.33, 0, 0), c(.33, .33, 0), c(.33, -.33, 0))
sd_eps  = list(c(1, 1, 0))
b2      = list(c(0, 0, 0), c(.25, 0, 0))
rate    = list(1/2, 1/4, 1/8, 1/16, 1/32, 1/64)
d       = list(c(1, 1, 0))

data_params = list(obs = nobs, rate = rate, b2 = b2, g1 = g1, 
                   d = d, sd_eps = sd_eps)

perf_form1 = y ~ x1*x2 + x1*z + x2*z + I(x1^2) + I(x2^2)
perf_form2 = y ~ x1*x2 + x1*z + x2*z 
perf_form3 = y ~ x1*x2 + z 
test_params = list(list(formula = x1 ~ x2 + z, 
                        variable = "x2",
                        label = "demand"),
                   list(formula = perf_form1,
                        variable = "x1:x2",
                        label = "performance~1"),
                   list(formula = perf_form2,
                        variable = "x1:x2",
                        label = "performance~2"),
                   list(formula = perf_form3,
                        variable = "x1:x2",
                        label = "performance~3"))

sim_params = list(nsim = 1000, boot = FALSE, mc_cores = 4) 

tint = qt(.975, df = nobs[[1]] - 5)
nobs    = list(300)
g1      = list(c(.33, .33, 0), c(.33, -.33, 0))
h1      = list(c(0, 0, 0), c(.33, 0, 0), c(-.33, 0, 0))
sd_eps  = list(c(1, 1, 0))
sd      = list(1)
b1      = list(c(0, 0, 0, 0))
b2      = list(c(0, 0, 0), c(.25, 0, 0))
rate    = list(1/2, 1/4, 1/8, 1/16, 1/32)
d       = list(c(1, 1, 0))

data_params = list(obs = nobs, rate = rate, b2 = b2, g1 = g1,
                   h1= h1, d = d, sd_eps = sd_eps, b1 = b1, 
                   sd = sd)
perf_form1 = y ~ x1*x2*z + I(x1^2) + I(x2^2)
test_params = list(list(formula = x1 ~ x2*z,
                        variable = "x2:z",
                        label = "demand",
                        nearly_correction = TRUE),
                   list(formula = perf_form1,
                        variable = "x1:x2:z",
                        label = "performance~1",
                        nearly_correction = TRUE))
sim_params = list(nsim = 1000, boot = FALSE, mc_cores = 4)

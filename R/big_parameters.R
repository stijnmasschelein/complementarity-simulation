nobs    = list(300)
g1      = list(c(.33, 0, 0), c(.33, .33, 0), c(.33, -.33, 0))
sd_eps  = list(c(.5, .5, 0), c(1, 1, 0), c(2, 2, 0))
sd      = list(1, 2, 4)
b1      = list(c(0, 0, 0, 0), c(0, 1, 1, 0), c(1, 1, 1, 0),
               c(0, -1, 1, 0))
b2      = list(c(0, 0, 0), c(.25, 0, 0), c(.5, 0, 0))
rate    = list(1/2, 1/4, 1/8, 1/16, 1/32)
d       = list(c(0, 0, 0), c(.25, .25, .25), c(1, 1, 0),
               c(1/3, 3, 0))

data_params = list(obs = nobs, rate = rate, b2 = b2, g1 = g1, 
                   d = d, sd_eps = sd_eps, b1 = b1, sd = sd)
perf_form1 = y ~ x1*x2 + x1*z + x2*z + I(x1^2) + I(x2^2)
test_params = list(list(formula = x1 ~ x2 + z, 
                        variable = "x2",
                        label = "demand",
                        nearly_correction = FALSE),
                   list(formula = perf_form1,
                        variable = "x1:x2",
                        label = "performance~1",
                        nearly_correction = FALSE))
sim_params = list(nsim = 1000, boot = FALSE, mc_cores = 32) 

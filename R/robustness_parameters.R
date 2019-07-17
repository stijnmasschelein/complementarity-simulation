nobs    = list(300)
g1      = list(c(.33, -.33, .33))
sd_eps  = list(c(.5, .5, .5), c(1, 1, 1), c(2, 2, 2))
sd      = list(1, 2, 4)
b1      = list(c(0, 0, 0, 0), c(.25, .25, .25, .25), c(1, 1, 1, 1))
b2      = list(c(0, 0, 0.25), c(.25, -.25, 0.25))
rate    = list(1/2, 1/4, 1/8, 1/16, 1/32)
d       = list(c(0, 0, 0), c(0, .25, 0), c(0, 1, 0))
x_integer = list(c(TRUE, FALSE, TRUE))

data_params = list(obs = nobs, rate = rate, b2 = b2, g1 = g1, 
                   d = d, sd_eps = sd_eps, b1 = b1, sd = sd)
perf_form1 = y ~ x1*x2 + x1*x3 + x2*x3 + x1*z + x2*z + x3*z + I(x2^2) 
test_params = list(list(formula = x1 ~ x2 + x3 + z, 
                        variable = "x2",
                        label = "demand12",
                        nearly_correction = FALSE),
                   list(formula = x1 ~ x2 + x3 + z, 
                        variable = "x3",
                        label = "demand13",
                        nearly_correction = FALSE),
                   list(formula = perf_form1,
                        variable = "x1:x2",
                        label = "performance12",
                        nearly_correction = FALSE),
                   list(formula = perf_form1,
                        variable = "x1:x3",
                        label = "performance13",
                        nearly_correction = FALSE))
sim_params = list(nsim = 1000, boot = FALSE, mc_cores = 32) 

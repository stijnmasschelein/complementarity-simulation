nobs    = list(list(30, 270), list(90, 210), list(150, 150),
               list(210, 90), list(270, 30))
g1      = list(list(c(.33, 0, 0), c(.33, 0, 0)),
               list(c(.33, .33, 0), c(.33, .33, 0)),
               list(c(.33, -.33, 0), c(.33, -.33, 0)))
sd_eps  = list(list(c(1, 1, 0), c(1, 1, 0)))
sd      = list(list(1, 1))
b1      = list(list(c(0, 0, 0, 0), c(0, 0, 0, 0)))
b2      = list(list(c(0, 0, 0), c(0, 0, 0)),
               list(c(.25, 0, 0), c(0.25, 0, 0)))
rate    = list(list(1/32, 1/2))
d       = list(list(c(1, 1, 0), c(1, 1, 0)))
 
data_params = list(obs = nobs, rate = rate, b2 = b2, g1 = g1,
                   d = d, sd_eps = sd_eps, b1 = b1, sd = sd)
perf_form1 = y ~ x1*x2 + x1*z + x2*z + I(x1^2) + I(x2^2)
perf_form2 = y ~ x1*x2 + x1*z + x2*z 
perf_form3 = y ~ x1*x2 + z
test_params = list(list(formula = x1 ~ x2 + z,
                        variable = "x2",
                        label = "demand",
                        nearly_correction = FALSE),
                   list(formula = perf_form1,
                        variable = "x1:x2",
                        label = "performance~1",
                        nearly_correction = FALSE),
                   list(formula = perf_form2,
                        variable = "x1:x2",
                        label = "performance~2",
                        nearly_correction = FALSE),
                   list(formula = perf_form3,
                        variable = "x1:x2",
                        label = "performance~3",
                        nearly_correction = FALSE))
sim_params = list(nsim = 1000, boot = FALSE, mc_cores = 4)

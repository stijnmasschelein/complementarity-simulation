nsim    = 1000
nobs    = 300
g1      = list(c(.33, 0, 0), c(.33, .33, 0), c(.33, -.33, 0))
sd_eps  = c(1, 1, 0)
b2      = list(c(0, 0, 0), c(.25, 0, 0))
rate    = list(1/2, 1/4, 1/8, 1/16, 1/32, 1/64)
d       = c(1, 1, 0)
mc_cores = 4
family_method  = list("match", "interaction_control", "interaction_moderation",
               "interaction_moderationaugmented")

params  = list(mc_cores = mc_cores, family_method = family_method, 
               nsim = nsim, rate = rate, obs = nobs,
               b2 = b2, g1 = g1, d = d, sd_eps = sd_eps)

tint = qt(.975, df = nobs - 5)
library(dplyr)
# heterogeneity ----

params_sim =  list(d = c(1, 1, 0), g1 = c(.33, 0, 0), sd = 1,
                   sd_eps = c(1, 1, 0), obs = 300, rep = 1, 
                   b1 = c(0, 0, 0, 0),
                   b2 = NULL, rate = NULL)
opt = c(1.0000000001, 2, 4, 8, 16, 32, 64)
b2 = c(0, 0.25)
nsim = 100

dat2 <- tibble()
for (comp in b2){
  params_sim$b2 = c(comp, 0, 0)
  for (optim in opt){
    for (i in 1:nsim){
      params_sim$rate = 1/optim
      sample = simcompl2::create_sample()
      sample = do.call(simcompl2::create_sample, params_sim)
      p = params_sim 
      dat2 <- mutate(sample,
             optimal1 = (p$d[2] * (p$b1[1] + p$g1[1] * z) + 
                         p$b2[1] * (p$b1[2] + p$g1[2] * z) /
                           (p$d[1] * p$d[2] - p$b2[1])),
             diff = x1 - optimal1) %>%
        summarise(sd = sd(diff), m = mean(diff), sdo = sd(optimal1),
                  sdx = sd(x1)) %>%
        mutate(b2 = comp, optim = optim) %>%
      bind_rows(dat2)    
    }
  }
}

random_sd_null = sqrt(100/12 + 0.33^2)
random_sd_effect = sqrt(100/12 + 0.33^2 / (1 - .25)^2)
group_by(dat2, optim, b2) %>%
  summarise(msd = mean(sd), mm = mean(m), msdo = mean(sdo),
            msdx = mean(sdx))

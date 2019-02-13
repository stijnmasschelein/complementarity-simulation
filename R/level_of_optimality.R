library(simcompl)
library(tidyverse)
library(cowplot)

#' ## Functions
#' ### Test parameters

d = 1;
b12 = .25;
g1 = .5; g2 = -g1;
s1 = s2 = 1;

params = list(interval = c(-5, 5), b12 = b12, d = c(d, d), g = c(g1, g2),
                s = c(s1, s2))

#' ### Calculate variances

var1 = function(interval = c(-5, 5), b12 = 0, d = c(1, 1), g = c(.5, .5),
                 s = c(.5, .5)){
  varx2 = (interval[2] - interval[1])^2 / 12;
  varz2 = (d[2] * g[1] + b12 * g[2])/(d[1] * d[2] - b12^2);
  return(varx2 + varz2)
}
do.call(var1, list(interval = c(-5, 5), b12 = .25, d = c(1, 1),
                    g = c(.5, -.5), s = c(1, 1))) # should equal 8.733..

var2 = function(interval = c(-5, 5), b12 = 0, d = c(1, 1), g = c(.5, .5),
                 s = c(s1, s2)){
  vare = (d[2] * s[1] + b12 * s[2])/(d[1] * d[2] - b12^2)
  return(vare)
}
do.call(var2, params)

#' ### Calculate the optimal level

calc_opt = function(b1 = c(0, 0), b12 = 0, d = c(1, 1), g = c(.5, .5), z = 0){
  x_star = (d[2] * b1[1] + b12 * b1[2] +  (d[2] * g[1] + b12 * g[2]) * z) /
           (d[1] * d[2] - b12^2)
  return(x_star)
}

params_z = list(b1 = c(0, 0), b12 = b12, d = c(d, d), g = c(g1, g2),
                z = c(-2, -1, 0, 1, 2))
do.call(calc_opt, params_z)

params[["obs"]] = 300; params[["rep"]] = 1; params[["rate"]] = .5;
params[["b1"]] = c(0, 0, 0);

#' ### A single simulation

sim_vars = function(interval = c(-5, 5), obs = 200, rep = 1, rate = 0.5, sd = 1,
                    s = c(0, 0), b1 = c(0, 0, 0), b12 = .5,
                    d = c(1, 1), g = c(0, 0)){
  par_vars = list(interval = interval, b12 = b12, d = d, g = g, s = s)
  par_sims = list(obs = obs, rep = rep, rate = rate, sd = sd, sd_eps = c(s, 0),
                  b1 = c(b1, 0), b2 = c(b12, 0, 0), d = c(d, 0), g1 = c(g, 0))
  sample = do.call(simcompl::create_sample, par_sims)
  sample$opt = calc_opt(b1 = b1[1:2], b12 = b12, d = d, g = g, sample$z)
  var_sample = mean((sample$x1 - sample$z)^2)
  var_random = do.call(var1, par_vars)
  var_optim  = do.call(var2, par_vars)
  r2 = (var_random - var_sample)/(var_random - var_optim)
  corxx = cor(sample$x1, sample$x2)
  corxz = cor(sample$x1, sample$z)
  return(list(var_sample = var_sample, var_random = var_random,
              var_optim = var_optim, r2 = r2, corxx = corxx,
              corxz = corxz))
}
res = do.call(sim_vars, params)

#' ## Run a simulation
#' ### Fixed parameters

params_sim =  list(interval = c(-5, 5), d = c(1, 1), g = c(.3, 0),
                   s = c(1, 1), obs = 300, rep = 1, b1 = c(0, 0, 0))

#' ### Varing parameters

b12 = c(0, .25); opt = c(2, 4, 8, 16, 32, 64)

#' ### Simulation parameters

nsim = 50

#' ### The simulation

dat = data.frame();
for (comp in b12){
  params_sim[["b12"]] = comp;
  for (optim in opt) {
    params_sim[["rate"]] = 1/optim;
    new_sim = replicate(nsim, do.call(sim_vars, params_sim), simplify = "vector")
    names = row.names(new_sim);
    new_sim = data.frame(t(apply(new_sim, 1:2, unlist)));
    colnames(new_sim) = names;
    new_sim$b12 = comp; new_sim$opt = optim;
    # new_sim$ratio = (new_sim$var_sample - new_sim$var_optim);
    dat = rbind(dat, new_sim)
    cat("done: b12 =", comp, "and opt =", optim, "\n")
  }
}

dat = tbl_df(dat) %>%
  mutate(ratio = (var_sample - var_optim) / var_sample)

plot_opt = ggplot(dat, aes(y = ratio, x = as.factor(b12))) +
  geom_point(alpha = .25) +
  facet_wrap(~ opt)
# cowplot::ggsave("figure-other/optimality.pdf", plot)

plot_xx = ggplot(dat, aes(y = corxx, x = as.factor(b12))) +
  geom_point(alpha = .25) +
  facet_wrap(~ opt)
# cowplot::ggsave("figure-other/correlation_complement.pdf", plot)

plot_xz = ggplot(dat, aes(y = corxz, x = as.factor(b12))) +
  geom_point(alpha = .25) +
  facet_wrap(~ opt)
# cowplot::ggsave("figure-other/correlation_contingency.pdf", plot)

plot_summ = cowplot::plot_grid(plot_xx, plot_xz, plot_opt, nrow = 1, labels = c("A", "B", "C"))
cowplot::save_plot("figure-latex/sample_descriptives.pdf", 
                   plot_summ, ncol = 3)


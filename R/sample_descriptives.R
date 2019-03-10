source("R/sample_descriptive_functions.R")

library(simcompl2)
library(tidyverse)
library(cowplot)

#' ## Run a simulation
#' ### Fixed parameters

params_sim =  list(interval = c(-5, 5), d = c(1, 1), g = c(.33, 0),
                   s = c(1, 1), obs = 300, rep = 1, b1 = c(0, 0, 0))

#' ### Varing parameters

b12 = c(0, .25); opt = c(2, 4, 8, 16, 32, 64)

#' ### Simulation parameters

nsim = 100
run_simulation_descriptive = rerun_simulation

#' ### The simulation

if (run_simulation_descriptive){
  dat = data.frame();
  for (comp in b12){
    params_sim[["b12"]] = comp;
    for (optim in opt) {
      params_sim[["rate"]] = 1/optim;
      new_sim = replicate(nsim, do.call(sim_vars, params_sim), 
                          simplify = "vector")
      names = row.names(new_sim);
      new_sim = data.frame(t(apply(new_sim, 1:2, unlist)));
      colnames(new_sim) = names;
      new_sim$b12 = comp; new_sim$opt = optim;
      dat = rbind(dat, new_sim)
      cat("done: b12 =", comp, "and opt =", optim, "\n")
    }
  }
  saveRDS(dat, "simulated_data/descriptives_simulation.Rds")
}

dat = tbl_df(readRDS("simulated_data/descriptives_simulation.Rds")) %>%
  mutate(ratio = (var_sample - var_optim) / var_sample)

plot_opt = ggplot(dat, aes(y = ratio, x = as.factor(b12))) +
  geom_point(alpha = .25) +
  facet_wrap(~ opt, labeller = label_bquote(O == .(opt))) + 
  labs(x = expression(beta[12]), y = "non-optimality ratio") +
  theme(strip.background = NULL)

plot_xx = ggplot(dat, aes(y = corxx, x = as.factor(b12))) +
  geom_point(alpha = .25) +
  facet_wrap(~ opt, labeller = label_bquote(O == .(opt))) + 
  labs(x = expression(beta[12]), y = expression(cor(x[1], x[2]))) +
  theme(strip.background = NULL)

plot_xz = ggplot(dat, aes(y = corxz, x = as.factor(b12))) +
  geom_point(alpha = .25) +
  facet_wrap(~ opt, labeller = label_bquote(O == .(opt))) + 
  labs(x = expression(beta[12]), y = expression(cor(x[1], z))) +
  theme(strip.background = NULL)

plot_summ = cowplot::plot_grid(plot_xx, plot_xz, plot_opt, nrow = 1, 
                               labels = c("A", "B", "C"))
cowplot::save_plot("figure-latex/sample_descriptives.pdf", 
                   plot_summ, ncol = 3)

group_by(dat, b12, opt) %>%
  summarise_at(c("corxx", "corxz", "ratio"), 
               .funs = list(av = mean, 
                            li = function(x) quantile(x, 0.05),
                            hi = function(x) quantile(x, 0.95)))


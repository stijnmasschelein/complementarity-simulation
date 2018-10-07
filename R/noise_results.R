source("R/prettify_functions.R")
source("R/parameters.R")
sim = readRDS("simulated_data/noise_simulations.Rds")
dat = make_sim_pretty(sim) 

dat_plot = 
  mutate(dat, 
         optim_fact = paste("O ==", optim),
         sd_fact = paste("sigma[nu] ==", sd),
         g1_fact = paste("gamma[2] ==", g1)) %>%
  arrange(optim) %>%
  mutate(optim_fact = fct_relevel(optim_fact, unique(optim_fact)))

#' plot
library(ggplot2) 
library(ggthemes)
library(cowplot)

plot = (ggplot(dat_plot, aes(y = stat, x = as.factor(b2)))
         + geom_tufteboxplot()
         + theme_cowplot(font_size = 12)
         + facet_grid(rows = vars(method),
                      cols = vars(optim_fact, g1_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + labs(x = expression(beta[12]), y = "t-statistic")
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

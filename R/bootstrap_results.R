source("R/parameters.R")
library(tidyverse)
library(xtable)

sim = readRDS("simulated_data/boot_simulation.Rds")
dat = tbl_df(sim) %>%
  unnest(rate, obs) %>%
  mutate(optim = 1/rate)

dat_plot = dat %>%
  mutate(optim_fact = paste("O ==", optim),
         g1_fact = paste(map(g1, 2)),
         b2_fact = paste(map(b2, 1))) %>%
  arrange(optim) %>%
  mutate(optim_fact = fct_relevel(optim_fact, unique(optim_fact)))

library(ggplot2) 
library(ggthemes)
library(cowplot)

plot = (ggplot(dat_plot, aes(y = I(coefficient/sterror), x = b2_fact))
         + geom_tufteboxplot()
         + theme_cowplot(font_size = 12)
         + facet_grid(rows = vars(g1_fact),
                      cols = vars(optim_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + labs(x = expression(beta[12]), y = "t-statistic")
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

table = dat %>%
  group_by(label, g1 = unlist(map(g1, 2)), b2 = unlist(map(b2, 1)), optim) %>%
  summarise(type1 = round(sum(sign(lower_interval) 
                              == sign(higher_interval)) / 
                            sim_params$nsim, 2),
            power = round(sum(lower_interval > 0) / 
                            sim_params$nsim, 2)) %>%
  ungroup() %>%
  mutate(percentage = ifelse(b2 != 0, power, type1),
         statistic = ifelse(b2 != 0, "power", "type I")) %>%
  select(-c(type1, power, b2)) %>%
  spread(optim, percentage) %>%
  arrange(desc(statistic), label, g1) %>%
  rename(`$\\gamma_2$` = g1,
         specification = label)
 
print(xtable(
  table,
  type = "pdf",
  label = "bootstrap-table",
  caption = "Type I error rates and power for the demand and 
  performance function approaches at different levels optimality: 
  2, 8, 32. \\gamma_2 is set at either -0.33 and 0.33. The other 
  parameters are the same as in Figure \\ref{main}."),
  size = "\\footnotesize",
  include.rownames = FALSE,
  sanitize.text.function = force,
  comment = FALSE,
  file = "tex/bootstrap_table.tex"
)
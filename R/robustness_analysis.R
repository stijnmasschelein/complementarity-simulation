library(tidyverse)
library(cowplot)
library(viridis)

# load data ----
summ <- readRDS("simulated_data/robustness_summary.RDS")

# Print average performance ----
summ %>%
  group_by(stat_type, label, optim) %>%
  summarise(mean = mean(stat_value),
            median = median(stat_value),
            sd = sd(stat_value),
            n_params = n()) %>%
  print(n = 40)

type_plot_basis <- ggplot(
  filter(summ, b1_str == "c(0, 0, 0, 0)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9)

type_plot_main <- ggplot(
  filter(summ, b1_str == "c(0.25, 0.25, 0.25, 0.25)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9)

type_plot_1corner <- ggplot(
  filter(summ, b1_str == "c(1, 1, 1, 1)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9)

# Construct power plots ----
power_plot_basis <- ggplot(
  filter(summ, b1_str == "c(0, 0, 0, 0)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str, b2_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9) +
  scale_y_continuous(breaks = c(0, .5, 1))

power_plot_main_effects <- ggplot(
  filter(summ, b1_str == "c(0.25, 0.25, 0.25, 0.25)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str, b2_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9) +
  scale_y_continuous(breaks = c(0, .5, 1))

power_plot_1corner <- ggplot(
  filter(summ, b1_str == "c(1, 1, 1, 1)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str, b2_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9) +
  scale_y_continuous(breaks = c(0, .5, 1))

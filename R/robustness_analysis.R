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

# Prepatation for plotting ----

summ <- mutate(summ,
  d_lab = recode(d_str,
    `c(0, 0, 0)` = "delta[2] == 0" ,
    `c(0, 0.25, 0)` = "delta[2] == 0.25" ,
    `c(0, 1, 0)` = "delta[2] == 1"),
  sd_lab = paste0("sigma[nu] == ", sd),
  sd_eps_lab = recode(sd_eps_str,
    `c(0.5, 0.5, 0.5)` = "sigma[epsilon] == 0.5",
    `c(1, 1, 1)` = "sigma[epsilon] == 1",
    `c(2, 2, 2)` = "sigma[epsilon] == 2")
)

# Construct Type I plots ----

type_plot_basis <- ggplot(
  filter(summ, b1_str == "c(0, 0, 0, 0)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_viridis(discrete = TRUE, end = .9) + 
  xlab("Optimality") + ylab("Type I Error Rate")

type_plot_main_effects <- ggplot(
  filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0.5)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_viridis(discrete = TRUE, end = .9) + 
  xlab("Optimality") + ylab("Type I Error Rate")

# Save Type I plots -----

save_plot("figure-latex/typeI_basis_discrete.pdf", 
          type_plot_basis, base_height = 7,
          base_aspect_ratio = 1.75)

save_plot("figure-latex/typeI_main_discrete.pdf", 
          type_plot_main_effects, base_height = 7,
          base_aspect_ratio = 1.75)

# Construct power plots ----
power_plot_basis <- ggplot(
  filter(summ, b1_str == "c(0, 0, 0, 0)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_viridis(discrete = TRUE, end = .9) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  xlab("Optimality") + ylab("Power")

power_plot_main_effects <- ggplot(
  filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0.5)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_viridis(discrete = TRUE, end = .9) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  xlab("Optimality") + ylab("Power")

save_plot("figure-latex/power_basis_discrete.pdf", 
          power_plot_basis, base_height = 7,
          base_aspect_ratio = 1.75)

save_plot("figure-latex/power_main_discrete.pdf", 
          power_plot_main_effects, base_height = 7,
          base_aspect_ratio = 1.75)


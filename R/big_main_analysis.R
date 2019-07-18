library(tidyverse)
library(cowplot)
library(viridis)
# Load data ----
read_data = FALSE
if (read_data){
 dat <- readRDS("simulated_data/big_main_simulation-15-07.Rds")
} else {
# Load summary results ----
 summ <- readRDS("simulated_data/simulation_summary.RDS")
}

# Print average performance ----
filter(summ, 
       b1_str %in% c("c(0, 0, 0, 0)", "c(0.5, 0.5, 0.5, 0)")) %>%
  group_by(b1_str, stat_type, label, optim) %>%
  summarise(mean = mean(stat_value),
            median = median(stat_value),
            sd = sd(stat_value),
            n_params = n()) %>%
  print(n = 40)

# Construct type I plots ----

type_plot_basis <- ggplot(
  filter(summ, b1_str == "c(0, 0, 0, 0)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9)

type_plot_main_effects <- ggplot(
  filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9)

type_plot_1corner <- ggplot(
  filter(summ, b1_str == "c(1, 1, 1, 0)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str),
             scales = "free_y") + 
  scale_colour_viridis(discrete = TRUE, end = .9)

# Save type I plots ----
save_plot("figure-latex/typeI_basis.pdf", 
          type_plot_basis,
          base_height = 12, base_width = 18,
          dpi = 600)
save_plot("figure-latex/typeI_main_effects.pdf", 
          type_plot_main_effects,
          base_height = 12, base_width = 18,
          dpi = 600)
save_plot("figure-latex/typeI_1corner.pdf", 
          type_plot_1corner,
          base_height = 12, base_width = 18,
          dpi = 600)

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
  filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str, b2_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9) +
  scale_y_continuous(breaks = c(0, .5, 1))

power_plot_1corner <- ggplot(
  filter(summ, b1_str == "c(1, 1, 1, 0)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd, sd_eps_str),
             rows = vars(d_str, b2_str)) + 
  scale_colour_viridis(discrete = TRUE, end = .9) +
  scale_y_continuous(breaks = c(0, .5, 1))

# Save power plots ----
save_plot("figure-latex/power_basis.pdf", 
          power_plot_basis,
          base_height = 12, base_width = 18,
          dpi = 600)
save_plot("figure-latex/power_main_effects.pdf", 
          power_plot_main_effects,
          base_height = 12, base_width = 18,
          dpi = 600)
save_plot("figure-latex/power_1corner.pdf", 
          power_plot_1corner,
          base_height = 12, base_width = 18,
          dpi = 600)

# Check for completeness ----

# sims <- dat %>%
#   group_by(optim, obs, sd, b1_str, b2_str, g1_str,
#            d_str, sd_eps_str) %>%
#   summarise(N = n())
# 
# b1s <- unique(sims$b1_str)
# group_by(filter(sims, sd == 4), b1_str) %>%
#   summarise(N = n())
# 
# group_by(filter(sims, sd == 4), b1_str, sd_eps_str) %>%
#   summarise(N = n())
# 
# filter(sims, sd == 4, b1_str == "c(1, 1, 1, 0)") %>%
#   group_by(sd_eps_str, d_str) %>%
#   summarise(N = n())
# 
# filter(sims, sd == 4, b1_str == "c(1, 1, 1, 0)",
#        sd_eps_str == "c(1, 1, 0)") %>%
#   group_by(d_str, g1_str) %>%
#   summarise(N = n())
# 
# filter(sims, sd == 4, b1_str == "c(1, 1, 1, 0)", 
#        sd_eps_str == "c(1, 1, 0)",
#        d_str == "c(0.25, 0.25, 0.25)") %>%
#   group_by(g1_str, b2_str) %>%
#   summarise(N = n())


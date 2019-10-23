library(tidyverse)
library(cowplot)
library(viridis)
theme_set(theme_cowplot())
# Load summary results ----
summ <- readRDS("simulated_data/mixed_simulation_summary.RDS") %>%
  mutate(weight_optim = case_when(
    grepl("\\(60", obs_str) ~ 1/5,
    grepl("\\(120", obs_str) ~ 2/5,
    grepl("\\(180", obs_str) ~ 3/5,
    grepl("\\(240", obs_str) ~ 4/5),
  correction = case_when(
    grepl("nearly_exact", label) ~ "nearly",
    TRUE ~ "default"
  ))

# Print average performance ----
summ %>%
  group_by(stat_type, label, weight_optim) %>%
  summarise(mean = mean(stat_value),
            median = median(stat_value),
            sd = sd(stat_value),
            n_params = n()) %>%
  print(n = 40)

# Construct type I plots ----

type_plot <- ggplot(
  filter(summ, stat_type == "type~I"),
  aes(y = stat_value, x= factor(weight_optim), 
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_wrap(~ correction) + 
  scale_colour_grey() +
  xlab("Optimality Mix") + ylab("Type I Error Rate")

# Construct power plots ----

power_plot <- ggplot(
  filter(summ,  stat_type == "power"),
  aes(y = stat_value, x= factor(weight_optim),
      group = label, col = label)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_wrap(~ correction) + 
  scale_colour_viridis(discrete = TRUE, end = .9) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  xlab("Optimality") + ylab("Power")

# # Save power plots ----
# save_plot("figure-latex/power_basis.pdf", 
#           power_plot_basis, base_height = 7,
#           base_aspect_ratio = 1.75)
# save_plot("figure-latex/power_main_effects.pdf", 
#           power_plot_main_effects, base_height = 7,
#           base_aspect_ratio = 1.75)

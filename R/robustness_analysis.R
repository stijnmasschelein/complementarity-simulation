library(tidyverse)
library(cowplot)
library(viridis)
theme_set(theme_cowplot() + 
            theme(legend.position="top",
                  legend.justification="center",
                  axis.text.x = element_text(angle=45)))

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
    `c(2, 2, 2)` = "sigma[epsilon] == 2"),
  specification = if_else(grepl("demand", label), "demand",
                          "performance 1"),
  practices = if_else(grepl("12", label), 
                      "1 discrete", "2 discrete")
  )

# Construct Type I plots ----

type_plot_basis <- ggplot(
  filter(summ, b1_str == "c(0, 0, 0, 0)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, colour = specification, 
      linetype = practices)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_grey() + 
  xlab("Optimality") + ylab("Type I Error Rate")

type_plot_main <- ggplot(
  filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0.5)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = label, colour = specification, 
      linetype = practices)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_grey() + 
  xlab("Optimality") + ylab("Type I Error Rate")

# Construct power plots ----
power_plot_basis <- ggplot(
  filter(summ, b1_str == "c(0, 0, 0, 0)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, colour = specification, 
      linetype = practices)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_grey() + 
  scale_y_continuous(breaks = c(0, .5, 1)) +
  xlab("Optimality") + ylab("Power")

power_plot_main <- ggplot(
  filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0.5)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = label, colour = specification, 
      linetype = practices)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_grey() + 
  scale_y_continuous(breaks = c(0, .5, 1)) +
  xlab("Optimality") + ylab("Power")


# Save basis plots ----
big_basis_plot = plot_grid(power_plot_basis, type_plot_basis,
                           ncol = 1, labels = "AUTO")
save_plot("figure-latex/robustness_basis.pdf", 
          big_basis_plot, base_height = 9, base_asp = 1.2)

# Save main plots ----
big_main_plot = plot_grid(power_plot_main, type_plot_main,
                          ncol = 1, labels = "AUTO")
save_plot("figure-latex/robustness_main.pdf",
          big_main_plot, base_height = 9, base_asp = 1.2)

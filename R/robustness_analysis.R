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
# Tables ----
library(kableExtra)
footnote_main = "Power and Type I error rate for the different levels of optimality (2, 4, 8, 16, 32) when $\\\\beta_1 = \\\\beta_2 = 0.5$ for the demand and performance 1 specification. Power refers to the proportion of samples with a significantly negative estimate for the interdependency between a discrete and a continuous practice ($\\\\beta_{12} = -0.25$) and a significantly positive estimate for the interdependency between two discrete practices ($\\\\beta_{13} = 0.25$). Type I refers to the proportion of samples with a significant estimate for the complementarity when $\\\\beta_{12} = \\\\beta_{13} = 0$. $\\\\beta_1$, $\\\\beta_2$, and $\\\\beta_3$ equal $0.5$. Because two of the practices are discrete $\\\\delta_1 = \\\\delta_3 = 0$."

filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0.5)") %>%
  mutate(specification = if_else(grepl("demand", label), 
                                 "demand", "performance"),
         complementarity = if_else(grepl("12", label),
                                   "1 discrete", "2 discrete")) %>%
  group_by(optim, sd, b2_str, d_str, sd_eps_str, 
           complementarity, specification, stat_type) %>%
  summarise(stat = mean(stat_value)) %>% 
  ungroup() %>%
  pivot_wider(values_from = stat,
              names_from = c(specification, optim)) %>%
  filter(!grepl("333", d_str), !grepl("0.5", b2_str), 
         sd == 1) %>%
  mutate(
    sd_eps = case_when(
      str_detect(sd_eps_str, "0.5") ~ 0.5,
      str_detect(sd_eps_str, "1") ~ 1,
      str_detect(sd_eps_str, "2") ~ 2),
    d = case_when(
      str_detect(d_str, "\\(0, 0,") ~ 0,
      str_detect(d_str, "0.25") ~ 0.25,
      str_detect(d_str, "1") ~ 1)) %>%
  arrange(complementarity, stat_type, sd_eps, d, sd) %>%
  select(sd_eps, d, sd, starts_with("demand"),
         starts_with("performance")) %>% 
  kable(format = "latex", booktabs = T, linesep = "", 
        escape = F, digits = 2,
        label = "robustness-main-table", 
        caption = "Power and Type I Error Rate with Main Effects and Discrete Practices",
        col.names = c("$\\sigma_{\\epsilon_i}$", 
                      "$\\delta_i$", "$\\sigma_{\\nu}$",
                       rep(c("2", "4", "8", "16", "32"), 2))) %>%
  pack_rows("1 Discrete Practice - Power", 1, 9, latex_align = "c") %>%
  pack_rows("1 Discrete Practice - Type I", 10, 18, latex_align = "c") %>%
  pack_rows("2 Discrete Practices - Power", 19, 27, latex_align = "c") %>%
  pack_rows("2 Discrete Practices - Type I", 28, 36, latex_align = "c") %>%
  add_header_above(c(" " = 3, "demand specification" = 5, 
                   "performance specification" = 5)) %>%
  kable_styling(font_size = 8) %>%
  footnote(
    general = footnote_main,         
    escape = FALSE, threeparttable = TRUE) %>%
  cat(., file = "tex/robustness_main_table.tex")

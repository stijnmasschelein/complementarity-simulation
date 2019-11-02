library(tidyverse)
library(cowplot)
library(viridis)
theme_set(theme_cowplot() + 
            theme(legend.position="top",
                  legend.justification="center",
                  axis.text.x = element_text(angle=45)))
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

# Prepatation for plotting ----

summ <- mutate(summ,
  d_lab = recode(d_str,
    `c(0, 0, 0)` = "delta[i] == 0" ,
    `c(0.25, 0.25, 0.25)` = "delta[i] == 0.25" ,
    `list(c(0.25, 0.25, 0))` = "delta[i] == 0.25" ,
    `c(0.333333333333333, 3, 0)` = "list(1/delta[1] == delta[2])",
    `c(1, 1, 0)` = "delta[i] == 1"),
  sd_lab = paste0("sigma[nu] == ", sd),
  sd_eps_lab = recode(sd_eps_str,
    `c(0.5, 0.5, 0)` = "sigma[epsilon] == 0.5",
    `c(1, 1, 0)` = "sigma[epsilon] == 1",
    `c(2, 2, 0)` = "sigma[epsilon] == 2"),
  b2_lab = recode(b2_str,
    `c(0.25, 0, 0)` = "beta[12] == 0.25",
    `c(0.5, 0, 0)` = "beta[12] == 0.5"),
  specification = str_replace(label, "~", " ")
)

# Construct type I plots ----

type_plot_basis <- ggplot(
  filter(summ, b1_str == "c(0, 0, 0, 0)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = specification, col = specification)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab), labeller = label_parsed) + 
  scale_colour_grey() +
  xlab("Optimality") + ylab("Type I Error Rate")

type_plot_main <- ggplot(
  filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0)", stat_type == "type~I"),
  aes(y = stat_value, x= factor(optim), 
      group = specification, col = specification)) +
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
      group = specification, col = specification)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab, b2_lab), labeller = label_parsed) + 
  scale_colour_grey() +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  xlab("Optimality") + ylab("Power") 

power_plot_main <- ggplot(
  filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0)", stat_type == "power"),
  aes(y = stat_value, x= factor(optim), 
      group = specification, col = specification)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(sd_lab, sd_eps_lab),
             rows = vars(d_lab, b2_lab), labeller = label_parsed) + 
  scale_colour_grey() +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  xlab("Optimality") + ylab("Power")


# Save basis plots ----
big_basis_plot = plot_grid(power_plot_basis, type_plot_basis,
                           rel_heights = c(7,5), ncol = 1, 
                           labels = "AUTO")
save_plot("figure-latex/big_basis.pdf", 
          big_basis_plot, base_height = 14, base_asp = .8)

# Save main plots ----
big_main_plot = plot_grid(power_plot_main, type_plot_main,
                           rel_heights = c(7,5), ncol = 1, 
                           labels = "AUTO")
save_plot("figure-latex/big_main.pdf",
          big_main_plot, base_height = 14, base_asp = .8)

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


# Tables
library(kableExtra)

footnote_main = "Power and Type I error rate for the different levels of optimality (2, 4, 8, 16, 32) when $\\\\beta_1 = \\\\beta_2 = 0.5$ for the demand and performance 1 specification. The effect of the environmental variable, $\\\\mathbf{z}$, on the second choice is either negative ($\\\\gamma_1 = .33$ and $\\\\gamma_2 = -.33$) or positive ($\\\\gamma_1 = 0.33$ and $\\\\gamma_2 = 0.33$). The results are averaged over the values of $\\\\gamma_2$."

footnote_basis = "Power and Type I error rate for the different levels of optimality (2, 4, 8, 16, 32) when $\\\\beta_1 = \\\\beta_2 = 0$ for the demand and performance 1 specification. The effect of the environmental variable, $\\\\mathbf{z}$, on the second choice is either negative ($\\\\gamma_1 = .33$ and $\\\\gamma_2 = -.33$) or positive ($\\\\gamma_1 = 0.33$ and $\\\\gamma_2 = 0.33$). The results are averaged over the values of $\\\\gamma_2$."

filter(summ, b1_str == "c(0.5, 0.5, 0.5, 0)") %>%
  group_by(optim, sd, b2_str, d_str, sd_eps_str, label, stat_type) %>%
  summarise(stat = mean(stat_value)) %>% 
  ungroup() %>%
  pivot_wider(values_from = stat,
              names_from = c(label, optim)) %>%
  filter(!grepl("333", d_str), !grepl("0.5", b2_str), 
         sd != 4) %>%
  mutate(
    sd_eps = case_when(
      str_detect(sd_eps_str, "0.5") ~ 0.5,
      str_detect(sd_eps_str, "1") ~ 1,
      str_detect(sd_eps_str, "2") ~ 2),
    d = case_when(
      str_detect(d_str, "\\(0,") ~ 0,
      str_detect(d_str, "0.25") ~ 0.25,
      str_detect(d_str, "1") ~ 1)) %>%
  arrange(stat_type, sd_eps, d, sd) %>%
  select(sd_eps, d, sd, starts_with("demand"),
         starts_with("performance")) %>%
  kable(format = "latex", booktabs = T, linesep = "", 
        escape = F, digits = 2,
        label = "big-main-table", 
        caption = "Power and Type I Error Rate without Main Effects",
        col.names = c("$\\sigma_{\\epsilon_i}$", 
                      "$\\delta_i$", "$\\sigma_{\\epsilon_i}$",
                       rep(c("2", "4", "8", "16", "32"), 2))) %>%
  pack_rows("Power", 1, 18, latex_align = "c") %>%
  pack_rows("Type I", 19, 36, latex_align = "c") %>%
  add_header_above(c(" " = 3, "demand specification" = 5, 
                   "performance specification" = 5)) %>%
  kable_styling(font_size = 8) %>%
  footnote(
    general = footnote_basis,         
    escape = FALSE, threeparttable = TRUE) %>%
  cat(., file = "tex/big_basis_table.tex")

filter(summ, b1_str == "c(0, 0, 0, 0)") %>%
  group_by(optim, sd, b2_str, d_str, sd_eps_str, label, stat_type) %>%
  summarise(stat = mean(stat_value)) %>% 
  ungroup() %>%
  pivot_wider(values_from = stat,
              names_from = c(label, optim)) %>%
  filter(!grepl("333", d_str), !grepl("0.5", b2_str), 
         sd != 4) %>%
  mutate(
    sd_eps = case_when(
      str_detect(sd_eps_str, "0.5") ~ 0.5,
      str_detect(sd_eps_str, "1") ~ 1,
      str_detect(sd_eps_str, "2") ~ 2),
    d = case_when(
      str_detect(d_str, "\\(0,") ~ 0,
      str_detect(d_str, "0.25") ~ 0.25,
      str_detect(d_str, "1") ~ 1)) %>%
  arrange(stat_type, sd_eps, d, sd) %>%
  select(sd_eps, d, sd, starts_with("demand"),
         starts_with("performance")) %>%
  kable(format = "latex", booktabs = T, linesep = "", 
        escape = F, digits = 2,
        label = "big-main-table", 
        caption = "Power and Type I Error Rate with Main Effects",
        col.names = c("$\\sigma_{\\epsilon_i}$", 
                      "$\\delta_i$", "$\\sigma_{\\epsilon_i}$",
                       rep(c("2", "4", "8", "16", "32"), 2))) %>%
  pack_rows("Power", 1, 18, latex_align = "c") %>%
  pack_rows("Type I", 19, 36, latex_align = "c") %>%
  add_header_above(c(" " = 3, "demand specification" = 5, 
                   "performance specification" = 5)) %>%
  kable_styling(font_size = 8) %>%
  footnote(
    general = footnote_main,         
    escape = FALSE, threeparttable = TRUE) %>%
  cat(., file = "tex/big_main_table.tex")


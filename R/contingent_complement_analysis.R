library(tidyverse)
library(cowplot)
library(viridis)
theme_set(theme_cowplot())
# Load summary results ----
summ <- readRDS("simulated_data/contingent_complement_summary.RDS")

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
  sd_lab = paste0("sigma[nu] == ", sd),
  sd_eps_lab = recode(sd_eps_str,
    `c(0.5, 0.5, 0)` = "sigma[epsilon] == 0.5",
    `c(1, 1, 0)` = "sigma[epsilon] == 1",
    `c(2, 2, 0)` = "sigma[epsilon] == 2"),
  b2_lab = recode(b2_str,
    `c(0.25, 0, 0)` = "beta[12] == 0.25",
    `c(0, 0, 0)` = "beta[12] == 0"),
  h1_lab = recode(h1_str,
    `c(0, 0, 0)` = "gamma[12] == 0",
    `c(-0.33, 0, 0)` = "gamma[12] == -0.33",
    `c(0.33, 0, 0)` = "gamma[12] == 0.33"),
  specification = str_replace_all(label, "~|_", " ")
)

# Construct type I plots ----

type_plot <- ggplot(
  filter(summ, stat_type == "type~I", !grepl("nearly", label)),
  aes(y = stat_value, x= factor(optim),
      group = specification, col = specification)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  facet_wrap(~ b2_lab, labeller = label_parsed) +
  scale_colour_grey() +
  xlab("Optimality") + ylab("Type I Error Rate")
 
# # Construct power plot----
power_plot <- ggplot(
  filter(summ, stat_type == "power", !grepl("nearly", label)),
  aes(y = stat_value, x= factor(optim),
      group = specification, col = specification)) +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(cols = vars(h1_lab, b2_lab), 
             labeller = label_parsed) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  scale_colour_grey() +
  xlab("Optimality") + ylab("Power")

# Save plots ----
combined = plot_grid(power_plot, type_plot, labels = "AUTO",
                     rel_widths = c(2, 1.25))
save_plot("figure-latex/contingent_complementarity.pdf",
          combined, base_height = 5,
          base_aspect_ratio = 2.5)

# Tables ----
library(kableExtra)
footnote = "Power is the proportion of samples reporting a significantly negative (when $\\\\gamma_{12} = -0.33$) or a significantly positive (when $\\\\gamma_{12} = 0.33$) contingent complementarity. Type I is the proportion of samples reporting a significant contingent complementarity when $\\\\gamma_{12} = 0$. The remaining parameters are the same as in the baseline simulation in Figure \\\\ref{main}."

summ %>%
  group_by(stat_type, label, optim, b2_str) %>%
  summarise(stat = mean(stat_value)) %>%
  pivot_wider(values_from = stat, 
              names_from = c(optim)) %>%
  ungroup() %>%
  mutate(`$\\beta_{12}$` = if_else(grepl("0.25", b2_str), 0.25, 0)) %>%
  rename(specification = label) %>%
  select(specification, `$\\beta_{12}$`, `2`, `4`, `8`, `16`, `32`) %>%
  kable(format = "latex", booktabs = T, linesep = "", 
        escape = F, digits = 2,
        label = "mix-optimality-table", 
        caption = "Power and Type I Error Rate for Contingent
                   Complementarity") %>%
  pack_rows("Power", 1, 6, latex_align = "c") %>%
  pack_rows("Type I", 7, 12, latex_align = "c") %>%
  add_header_above(c(" " = 2, "Level of Optimality" = 5)) %>%
  footnote(
    general = footnote,         
    escape = FALSE, threeparttable = TRUE) %>%
  cat(., file = "tex/contingent_complementarity_table.tex") 
 
library(tidyverse)
library(cowplot)
library(viridis)
theme_set(theme_cowplot())
# Load summary results ----
summ <- readRDS("simulated_data/mixed_simulation_summary.RDS") %>%
  mutate(weight_optim = case_when(
    grepl("\\(30", obs_str) ~ 1/10,
    grepl("\\(90", obs_str) ~ 3/10,
    grepl("\\(150", obs_str) ~ 5/10,
    grepl("\\(210", obs_str) ~ 7/10,
    grepl("\\(270", obs_str) ~ 9/10),
  correction = case_when(
    grepl("nearly_exact", label) ~ "nearly",
    TRUE ~ "default"),
  specification = str_replace_all(label, "~|_", " ")
  )

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
  filter(summ, stat_type == "type~I", !grepl("nearly", label)),
  aes(y = stat_value, x = factor(weight_optim), 
      group = specification, col = specification)) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_hline(yintercept = 0.05, colour = "darkgrey") +
  scale_colour_grey() +
  xlab("Optimality Mix") + ylab("Type I Error Rate")

# Construct power plots ----

power_plot <- ggplot(
  filter(summ,  stat_type == "power", !grepl("nearly", label)),
  aes(y = stat_value, x= factor(weight_optim),
      group = specification, col = specification)) +
  stat_summary(fun.y = mean, geom = "line") +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  scale_colour_grey() +
  xlab("Optimality Mix") + ylab("Power")


# Save power plots ----
combined = plot_grid(power_plot, type_plot, labels = "AUTO")
save_plot("figure-latex/mixed_optimality.pdf",
          combined, base_height = 5,
          base_aspect_ratio = 2.5)

# Read full results ----
full <- readRDS("simulated_data/big_mixed_simulation.RDS")

full <- mutate(full, b12 = map_dbl(map(b2, 1), 1))

ggplot(filter(full, b12 == 0, grepl("nearly", label)), 
       aes(y = stat, x = obs_str)) +
  ggthemes::geom_tufteboxplot(varwidth = TRUE) + 
  geom_hline(yintercept = 0, colour = "grey") +
  facet_grid(rows= vars(label), 
             cols =vars(g1_str)) 

# Table ----
full %>% filter(b12 == 0) %>%
  group_by(label, obs_str) %>%
  summarise(typeI = mean(I(pvalue < 0.05)),
            N = n()) %>% 
  print(n = 40)

# Plot ----
O_obs = 240
sample = simcompl2::create_mixed_sample(
  list(obs = O_obs, rate = 1/32,
       sd_eps = c(0.5, 0.5, 0.5),
       g1 = c(0.33, 0, 0),
       b2 = c(0, 0, 0)),
  list(obs = 300 - O_obs, rate = 1/2, 
       sd_eps = c(0.5, 0.5, 0.5),
       g1 = c(0.33, 0, 0),
       b2 = c(0, 0, 0))) %>%
  mutate(rn = row_number())

qplot(data = sample, y, bins = 10)

ggplot(sample, aes(y = x1, x = x2, 
                   colour = as.factor(I(rn <= O_obs)))) + 
  geom_point() +
  scale_colour_viridis(discrete = TRUE)

# Tables ----
library(kableExtra)
footnote = "The samples are constructed as a mix of a high optimality sample ($O=32$) and a low optimality sample ($O=2$). The proportion of the high optimality sample is given in the table. The remaining parameters are the same as in the baseline simulation in Figure \\\\ref{main}"

summ %>%
  filter(correction == "default", !grepl("0.33, 0, 0", g1_str)) %>%
  group_by(stat_type, specification, weight_optim) %>%
  summarise(stat = mean(stat_value)) %>%
  pivot_wider(values_from = stat, names_from = weight_optim) %>%
  ungroup() %>%
  select(2:7) %>%
  kable(format = "latex", booktabs = T, linesep = "", 
        escape = F, digits = 2,
        label = "mix-optimality-table", 
        caption = "Power and Type I Error Rate with Mixed Optimality") %>%
  pack_rows("Power", 1, 4, latex_align = "c") %>%
  pack_rows("Type I", 5, 8, latex_align = "c") %>%
  add_header_above(c(" " = 1, "Proportion High Optimality" = 5)) %>%
  kable_styling(font_size = 9) %>%
  footnote(
    general = footnote,         
    escape = FALSE, threeparttable = TRUE) %>%
  cat(., file = "tex/mix_optimality_table.tex") 
  
  
  
  
  
  
  
  
  

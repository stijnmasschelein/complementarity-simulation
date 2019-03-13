library(tidyverse)
library(xtable)
sim = tbl_df(readRDS("simulated_data/discrete_simulation.Rds"))
dat = tbl_df(sim) %>%
  unnest(rate, obs) %>%
  mutate(label = as.character(label)) %>%
  mutate(optim = 1/rate,
         label = if_else(str_detect(label, "nearly_exact"),
                         str_c(str_replace(label, "nearly_exact_", ""),
                               "corrected", sep = " "),
                         label))

table = dat %>%
  group_by(label, 
           b2 = unlist(map(b2, 1)), optim) %>%
  summarise(type1 = round(mean(pvalue < 0.05), 2),
            power = round(mean(pvalue < 0.05 & coefficient > 0), 
                          2)) %>%
  ungroup() %>%
  mutate(percentage = ifelse(b2 != 0, power, type1),
         statistic = ifelse(b2 != 0, "power", "type I")) %>%
  select(-c(type1, power, b2)) %>%
  spread(optim, percentage) %>%
  arrange(statistic, label) %>%
  rename(specification = label)
    
print(xtable(filter(table, !str_detect(specification, "corrected")),
  type = "pdf",
  label = "discrete-table",
  caption = "Type I error rates and power for the demand and 
  performance specifications at different levels optimality: 2, 
  4, 8, 16. The practices can only take two values: $1$ and $-1$.
  $\\delta_1 = \\delta_2 = 0$. The results are aggregated over 
  the parameter values of $\\gamma_2$ (-0.33, 0, 0.33)."),
      size = "\\footnotesize",
      include.rownames = FALSE,
      sanitize.text.function = force,
      comment = FALSE,
      file = "tex/discrete_table.tex"
)


source("R/parameters.R")
library(tidyverse)
library(xtable)
sim = tbl_df(readRDS("simulated_data/nearly_simulation.RDS",))
dat = tbl_df(sim) %>%
  unnest(rate, obs) %>%
  mutate(label = as.character(label)) %>%
  mutate(optim = 1/rate,
         label = if_else(str_detect(label, "nearly_exact"),
                         str_c(str_replace(label, "nearly_exact_", ""),
                               "corrected", sep = " "),
                         label))

combined = mutate(dat, 
                  specification = ifelse(str_detect(label, "corrected"),
                                         "combined corrected", 
                                         "combined"),
                  type1 = I(pvalue > 0.05),
                  power = I(pvalue < 0.05 & coefficient > 0)) %>%
  group_by(id, b2 = unlist(map(b2, 1)), optim,
           g1 = unlist(map(g1, 2)), specification) %>%
  summarise(type1 = any(pvalue < 0.05),
            power = any(pvalue < 0.05 & coefficient > 0),
            sametype1 = all(pvalue < 0.05)) %>%
  ungroup()

filter(combined, b2 == 0) %>%
  group_by(optim, specification) %>%
  summarise(sametype1 = sum(sametype1),
            type1 = sum(type1),
            ratio = sametype1/type1) %>%
  ungroup()
  
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
  arrange(desc(statistic), label) %>%
  rename(specification = label)
 
combined_table = combined %>%
  group_by(specification, b2, optim) %>%
  summarise(type1 = round(mean(type1), 2),
            power = round(mean(power), 2)) %>%
  ungroup() %>%
  mutate(percentage = ifelse(b2 != 0, power, type1),
         statistic = ifelse(b2 != 0, "power", "type I")) %>%
  select(-c(type1, power, b2)) %>%
  spread(optim, percentage) %>%
  arrange(desc(statistic))
  
print(xtable(bind_rows(table, combined_table),
             type = "pdf",
             label = "main-table",
             caption = "Type I error rates and power for the demand and
             performance function approaches at different levels optimality:
             2, 4, 8, 16, 32, 64. The parameters are ... TODO."),
      size = "\\footnotesize",
      include.rownames = FALSE,
      sanitize.text.function = force,
      comment = FALSE,
      file = "tex/nearly_table.tex"
)

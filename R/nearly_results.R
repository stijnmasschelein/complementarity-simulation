source("R/parameters.R")
library(tidyverse)
library(xtable)
sim = tbl_df(readRDS("simulated_data/nearly_simulation.RDS"))
dat = tbl_df(sim) %>%
  unnest(rate, obs) %>%
  mutate(optim = 1/rate)

table = dat %>%
  group_by(label, #g1 = unlist(map(g1, 2)), 
           b2 = unlist(map(b2, 1)), optim) %>%
  summarise(type1 = round(mean(pvalue < 0.05), 2),
            power = round(mean(pvalue < 0.05 & coefficient > 0), 
                          2)) %>%
  ungroup() %>%
  mutate(percentage = ifelse(b2 != 0, power, type1),
         statistic = ifelse(b2 != 0, "power", "type I")) %>%
  select(-c(type1, power, b2)) %>%
  spread(optim, percentage) %>%
  arrange(desc(statistic), label #, g1
          ) %>%
  rename(#`$\\gamma_2$` = g1,
         specification = label)
 
print(xtable(table,
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
# source("R/prettify_functions.R")
source("R/parameters.R")
library(tidyverse)
sim = readRDS("simulated_data/main_simulation.Rds")
dat = tbl_df(sim) %>%
  unnest(obs, rate) %>%
  mutate(optim = 1/rate)

#' plot
library(ggplot2) 
library(ggthemes)
library(cowplot)

#' table
library(xtable)

table = dat %>%
  group_by(label, g1 = unlist(map(g1, 2)), 
           b2 = unlist(map(b2, 1)), optim) %>%
    summarise(type1 = round(sum(pvalue < 0.05)/sim_params$nsim, 2),
              power = round(sum(pvalue < 0.05 & coefficient > 0)/sim_params$nsim, 2)) %>%
    ungroup() %>% 
    mutate(percentage = ifelse(b2 != 0, power, type1),
           statistic = ifelse(b2 != 0, "power", "type I")) %>%
    select(-c(type1, power, b2)) %>%
    spread(optim, percentage) %>%
    arrange(desc(statistic), label, g1) %>%
    rename(`$\\gamma_1$` = g1,
           specification = label)

print(xtable(table,
             type = "pdf",
             label = "main-table",
             caption = "Type I error rates and power for the demand 
             and performance function approaches at different
             levels optimality: 2, 4, 8, 16, 32, 64. The 
             parameters are the same as in Figure
             \\ref{main}."),
      size = "\\footnotesize",
      include.rownames = FALSE,
      sanitize.text.function = force,
      comment = FALSE,
      file = "tex/main_table.tex"
)

# New plot

dat_plot_new = 
  filter(dat,
         unlist(map(g1, 2)) != 0.33,
         label != "performance~2") %>%
  mutate(optim_fact = paste(optim),
         g1_fact = paste("gamma[2] ==", map(g1, 2)),
         b2_fact = paste(map(b2, 1))) %>%
  arrange(optim) %>%
  mutate(optim_fact = fct_relevel(optim_fact, unique(optim_fact)),
         g1_fact = fct_relevel(g1_fact, c("gamma[2] == 0")))

plot_null = (ggplot(filter(dat_plot_new, unlist(map(b2, 1)) == 0),
                    aes(y = stat, x = optim_fact))
         + geom_tufteboxplot()
         + theme_cowplot(font_size = 12)
         + facet_grid(rows = vars(label),
                      cols = vars(g1_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + ggtitle("Null Effect")
         + labs(y = "t-statistic", x = NULL)
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

plot_true = (ggplot(filter(dat_plot_new, unlist(map(b2, 1)) == 0.25),
                    aes(y = stat, x = optim_fact))
         + geom_tufteboxplot()
         + theme_cowplot(font_size = 12)
         + facet_grid(rows = vars(label),
                      cols = vars(g1_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + ggtitle("True Effect")
         + labs(x = "level of optimality", y = "t-statistic")
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

new_plot = plot_grid(plot_null, plot_true, labels = "AUTO",
                     ncol = 1)
save_plot("figure-latex/main_new_plot.pdf", plot = new_plot,
          base_height = 6, base_width = 9,
          dpi = 600)
save_plot("figure-latex/main_null_plot.pdf", plot = plot_null,
          base_height = 4, base_width = 8)
save_plot("figure-latex/main_true_plot.pdf", plot = plot_true,
          base_height = 4, base_width = 8)
                   

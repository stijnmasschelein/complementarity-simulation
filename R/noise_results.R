source("R/parameters.R")
library(tidyverse)
library(kabelExtra)
sim = readRDS("simulated_data/noise_simulation.Rds")
dat = tbl_df(sim) %>%
  mutate(rate = map_dbl(rate, ~ .x[[1]]),
         obs = map_dbl(obs, ~ .x[[1]]),
         sd = map_dbl(sd, ~ .x[[1]])) %>%
  mutate(optim = 1/rate)

# dat_plot = 
#   mutate(dat, 
#          optim_fact = paste("O ==", optim),
#          sd_fact = paste("sigma[nu] ==", sd),
#          g1_fact = paste("gamma[2] ==", map(g1, 2)),
#          b2_fact = paste(map(b2, 1))) %>%
#   arrange(optim) %>%
#   mutate(optim_fact = fct_relevel(optim_fact, unique(optim_fact)))

#' plot
library(ggplot2) 
library(ggthemes)
library(cowplot)
 
# plot = (ggplot(dat_plot, aes(y = stat, x = b2_fact))
#          + geom_tufteboxplot()
#          + theme_cowplot(font_size = 12)
#          + facet_grid(rows = vars(label),
#                       cols = vars(optim_fact, sd_fact, g1_fact),
#                       labeller = label_parsed)
#          + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
#          + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
#          + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
#          + labs(x = expression(beta[12]), y = "t-statistic")
#          + theme(strip.text.x = element_text(angle = 0, size = 8),
#                  strip.text.y = element_text(angle = 0),
#                  strip.background = NULL)
# )
# 
# save_plot("figure-latex/noise_plot.pdf", plot = plot, 
#           base_height = 6, base_width = 12, dpi = 600)
 

#' table
library(xtable)

table = dat %>%
  filter(sd == 4) %>%
  group_by(label, g1 = unlist(map(g1, 2)), 
           b2 = unlist(map(b2, 1)), optim) %>%
    summarise(type1 = round(sum(pvalue < .05)/sim_params$nsim, 2),
              power = round(sum(pvalue < .05 & coefficient > 0)/sim_params$nsim, 2)) %>%
    ungroup() %>%
    mutate(percentage = ifelse(b2 != 0, power, type1),
           statistic = ifelse(b2 != 0, "power", "type I")) %>%
    select(-c(type1, power, b2)) %>%
    spread(optim, percentage) %>%
    arrange(statistic, label, g1) %>%
    rename(`$\\gamma_2$` = g1,
           specification = label)
    
# print(xtable(table,
#              type = "pdf",
#              label = "noise-table",
#              caption = "Type I error rates and power for the demand 
#   and performance specifications at different levels optimality: 
#   2, 8, 32. The parameters are the same as in Figure \\ref{noise}.
#   Only the results for $\\sigma_{\\nu} = 4$ are reported"),
#       size = "\\footnotesize",
#       include.rownames = FALSE,
#       sanitize.text.function = force,
#       comment = FALSE,
#       file = "tex/noise_table.tex"
# )


# New table ----

footnote = "Type I error rates and power for the demand 
            and performance specifications at different
            levels optimality: 2, 8, 32. The
            parameters are the same as in Figure
            \\\\ref{noise}. Only the results for 
            $\\\\sigma_{\\\\nu} = 4$ are reported."

table %>% select(-statistic) %>%
  kable(format = "latex", booktabs = T, linesep = "", 
        escape = F, digits = 2,
        label = "noise-table", 
        caption = "Power and Type I Error Rate with Performance Variation") %>%
  pack_rows("Power", 1, 8, latex_align = "c") %>%
  pack_rows("Type I", 9, 16, latex_align = "c") %>%
  add_header_above(c(" " = 1, " "  = 1, 
                     "Level of Optimality" = 3)) %>%
  kable_styling(font_size = 9) %>%
  footnote(
    general = footnote,         
    escape = FALSE, threeparttable = TRUE) %>%
  cat(., file = "tex/noise_table.tex")   

# New plot ----
         
dat_plot_new = 
  filter(dat,
         unlist(map(g1, 2)) != 0.33) %>%
  mutate(optim_fact = paste(optim),
         g1_fact = paste("gamma[2] ==", map(g1, 2)),
         sd_fact = paste("sigma[nu] ==", sd),
         b2_fact = paste(map(b2, 1))) %>%
  arrange(optim) %>%
  mutate(optim_fact = fct_relevel(optim_fact, unique(optim_fact)))

plot_null = (ggplot(filter(dat_plot_new, unlist(map(b2, 1)) == 0),
                    aes(y = stat, x = optim_fact))
         + geom_tufteboxplot()
         + theme_cowplot(font_size = 12)
         + facet_grid(rows = vars(label),
                      cols = vars(g1_fact, sd_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + scale_y_continuous(breaks = scales::pretty_breaks(4))
         + ggtitle("Null Effect")
         + labs(x = "level of optimality", y = "t-statistic")
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

plot_true = (ggplot(filter(dat_plot_new, unlist(map(b2, 1)) == 0.25),
                    aes(y = stat, x = optim_fact))
         + geom_tufteboxplot()
         + theme_cowplot(font_size = 12)
         + facet_grid(rows = vars(label),
                      cols = vars(g1_fact, sd_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + scale_y_continuous(breaks = scales::pretty_breaks(4))
         + ggtitle("True Effect")
         + labs(y = "t-statistic", x = NULL)
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

new_plot = plot_grid(plot_true, plot_null, labels = "AUTO",
                     ncol = 1)
save_plot("figure-latex/noise_new_plot.pdf", plot = new_plot,
          base_height = 6, base_width = 9,
          dpi = 600)

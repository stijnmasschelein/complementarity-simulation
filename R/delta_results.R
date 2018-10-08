source("R/prettify_functions.R")
source("R/parameters.R")
sim = readRDS("simulated_data/delta_simulation.Rds")
main = readRDS("simulated_data/main_simulation.Rds")
dat_main = make_sim_pretty(main) %>%
  filter(g1 %in% c(.33, -.33), optim %in% c(2, 8, 32))
dat = make_sim_pretty(sim) %>% 
  bind_rows(dat_main)

dat_plot = 
  mutate(dat, 
         optim_fact = paste("O ==", optim),
         g1_fact = paste("gamma[2] ==", g1),
         d_fact = paste("delta[i] ==", d1)) %>%
  arrange(optim) %>%
  mutate(optim_fact = fct_relevel(optim_fact, unique(optim_fact)))

#' plot
library(ggplot2) 
library(ggthemes)
library(cowplot)

plot = (ggplot(dat_plot, aes(y = stat, x = as.factor(b2)))
         + geom_tufteboxplot()
         + theme_cowplot(font_size = 12)
         + facet_grid(rows = vars(method),
                      cols = vars(optim_fact, d_fact, g1_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + labs(x = expression(beta[12]), y = "t-statistic")
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

save_plot("figure-latex/delta_plot.pdf", plot = plot, 
          base_height = 6, base_width = 12, dpi = 600)


#' table
library(xtable)

table = dat %>%
  filter(method %in% c("demand", "performance~1"), g1 == .33) %>%
  group_by(method, g1, d1, b2, optim) %>%
    summarise(type1 = round(sum(abs(stat) > tint)/nsim, 2),
              power = round(sum(stat > tint)/nsim, 2)) %>%
    ungroup() %>%
    mutate(percentage = ifelse(b2 != 0, power, type1),
           statistic = ifelse(b2 != 0, "power", "type I")) %>%
    select(-c(type1, power, b2)) %>%
    spread(optim, percentage) %>%
    arrange(desc(statistic), method, g1) %>%
    rename(`$\\gamma_2$` = g1,
           `$\\delta_i$` = d1,
           specification = method)
    
print(xtable(table,
             type = "pdf",
             label = "delta-table",
             caption = "Type I error rates and power for the \\emph{demand} and
             \\emph{performance 1} specification at different levels optimality: 
             2, 8, 32. The parameters are the same as in Figure \\ref{delta}.
             Only the results for $\\gamma_{2} = 0.33$ are reported"),
      size = "\\footnotesize",
      include.rownames = FALSE,
      sanitize.text.function = force,
      comment = FALSE,
      file = "tex/delta_table.tex"
)


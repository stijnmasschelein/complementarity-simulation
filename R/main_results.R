source("R/prettify_functions.R")
source("R/parameters.R")
sim = readRDS("simulated_data/main_simulation.Rds")
dat = make_sim_pretty(sim) 

dat_plot = 
  mutate(dat, optim_fact = paste("O ==", optim),
              g1_fact = paste("gamma[2] ==", g1)) %>%
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
                      cols = vars(optim_fact, g1_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + labs(x = expression(beta[12]), y = "t-statistic")
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

save_plot("figure-latex/main_plot.pdf", plot = plot, 
          base_height = 6, base_width = 12,
          dpi = 600)

#' table
library(xtable)

table = dat %>%
  group_by(method, g1, b2, optim) %>%
    summarise(type1 = round(sum(abs(stat) > tint)/nsim, 2),
              power = round(sum(stat > tint)/nsim, 2)) %>%
    ungroup() %>%
    mutate(percentage = ifelse(b2 != 0, power, type1),
           statistic = ifelse(b2 != 0, "power", "type I")) %>%
    select(-c(type1, power, b2)) %>%
    spread(optim, percentage) %>%
    arrange(desc(statistic), method, g1) %>%
    rename(`$\\gamma_2$` = g1,
           specification = method)
    
print(xtable(table,
             type = "pdf",
             label = "basic-error",
             caption = "Type I error rates and power for the demand and
             performance function approaches at different levels optimality: 
             2, 4, 8, 16, 32, 64. The parameters are the same as in Figure 
             \\ref{basic}."),
      size = "\\footnotesize",
      include.rownames = FALSE,
      sanitize.text.function = force,
      comment = FALSE,
      file = "tex/main_table.tex"
)

source("R/prettify_functions.R")
source("R/parameters.R")
sim = readRDS("simulated_data/spurious_simulation.Rds")
dat = make_sim_pretty(sim)

dat_plot = 
  filter(dat, method %in% c("demand", "performance~1")) %>%
  mutate(optim_fact = paste("O ==", optim),
         theta_fact = paste("theta[2] ==", theta)) %>%
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
                      cols = vars(optim_fact, theta_fact),
                      labeller = label_parsed)
         + geom_hline(yintercept = tint, linetype = 3, alpha = .25)
         + geom_hline(yintercept = 0, linetype = 4, alpha = .25)
         + geom_hline(yintercept = -tint, linetype = 3, alpha = .25)
         + labs(x = expression(beta[12]), y = "t-statistic")
         + theme(strip.text.x = element_text(angle = 0, size = 8),
                 strip.text.y = element_text(angle = 0),
                 strip.background = NULL)
)

save_plot("figure-latex/spurious_plot.pdf", plot = plot, 
          base_height = 4, base_width = 8, dpi = 600)


#' table
library(xtable)

table = dat %>%
  filter(method %in% c("demand", "performance~1")) %>%
  group_by(method, theta, b2, optim) %>%
    summarise(type1 = round(sum(abs(stat) > tint)/nsim, 2),
              power = round(sum(stat > tint)/nsim, 2)) %>%
    ungroup() %>%
    mutate(percentage = ifelse(b2 != 0, power, type1),
           statistic = ifelse(b2 != 0, "power", "type I")) %>%
    select(-c(type1, power, b2)) %>%
    spread(optim, percentage) %>%
    arrange(desc(statistic), method, theta) %>%
    rename(`$\\theta_2$` = theta,
           specification = method)
    
print(xtable(table,
             type = "pdf",
             label = "spurious-table",
             caption = "Type I error rates and power for the \\emph{demand} and
             \\emph{performance 1} specification at different levels of 
             optimality: 2, 8, 32. The parameters are the same as in Figure 
             \\ref{spurious}."),
      size = "\\footnotesize",
      include.rownames = FALSE,
      sanitize.text.function = force,
      comment = FALSE,
      file = "tex/spurious_table.tex"
)



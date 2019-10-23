library(tidyverse)
tmp_dir <- "tmp_simcompl"
filenames <- list.files(tmp_dir, pattern=".RDS", full.names=TRUE)
start <- Sys.time()
# Read files in ----
ldf <- parallel::mclapply(filenames, readRDS, mc.cores = 3L)
end <- Sys.time()
print(end - start)
# Bind dfs ----
start <- Sys.time()
sim <- bind_rows(ldf)
end <- Sys.time()
print(end - start)
# Transform list variables ----
start <- Sys.time()
dat = tbl_df(sim) %>%
  mutate(
    b2_str = as.character(b2),
    b1_str = as.character(b1),
    d_str = as.character(d),
    sd_eps_str = as.character(sd_eps),
    g1_str = as.character(g1),
    obs_str = as.character(obs),
    rate_str = as.character(rate),
    sd_str = as.character(sd))
end <- Sys.time()
print(end - start)
# Save big tbl ----
start <- Sys.time()
saveRDS(dat, file = "simulated_data/mixed_complement_simulation.RDS")
end <- Sys.time()
print(end - start)
# Calculate summary statistics ----
start <- Sys.time()
summ <- dat %>%
group_by(rate_str, obs_str, sd_str, b1_str, b2_str, g1_str, 
         d_str, sd_eps_str, label) %>%
mutate(stat_type = "power") %>%
summarise(stat_type = first(stat_type),
          stat_value = if_else(stat_type == "type~I",
          mean(I(pvalue <= 0.05)),
          mean(I(pvalue < 0.05 & coefficient > 0))),
          N = n()) %>%
ungroup()
end <- Sys.time()
print(end - start)
# Write summary results to file ----
start <- Sys.time()
saveRDS(summ, "simulated_data/mixed_complement_summary.RDS")
end <- Sys.time()
print(end - start)

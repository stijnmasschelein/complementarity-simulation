library(tidyverse)
make_sim_pretty = function(sim){
  result = sim %>% tbl_df %>%
    mutate(
      method = recode(method,"matching" = "demand",
                      interaction_control = "performance~3",
                      interaction_moderation = "performance~2",
                      interaction_moderationaugmented = "performance~1"),
      optim = 1/rate,
      b2 = as.numeric(str_split(as.character(b2), ",", simplify = TRUE)[,1]),
      g1 = as.numeric(str_split(as.character(g1), ",", simplify = TRUE)[,2]),
      theta = as.numeric(str_split(as.character(g2), ",", simplify = TRUE)[,2]),
      d1 = as.numeric(str_split(as.character(d), ",", simplify = TRUE)[,1])
    )
  return(result)
}

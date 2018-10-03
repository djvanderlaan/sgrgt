
library(ggplot2)
library(dplyr)
library(tidyr)

dta <- res %>% group_by(n, p, pdrop) %>% gather(metric, score, mean_ssi1:modularity)


dta %>% filter(metric %in% c("mean_ssi1", "mean_ssi2", "modularity")) %>% 
ggplot(., aes(x = p, y = score, color = factor(pdrop))) + geom_line() + 
  facet_wrap(~metric)



# Packages --------------------------------------------------------------------
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(patchwork)) install.packages("patchwork"); library(patchwork)


# Import data from tidyverse.R ------------------------------------------------
source("R/cleaning_and_manipulation/tidyverse.R")
rm(weather, weekly_sales, month_nums, default_values, laglen, lagsum)


# Plots -----------------------------------------------------------------------
cattle
business_climate


price_avgs <- cattle %>% 
  group_by(reprod) %>% 
  filter(price < quantile(price, .9999)) %>% 
  group_by(date, reprod) %>% 
  summarize(wt_prc = weighted.mean(price, quantity),
            mn_prc = mean(price),
            med_prc = median(price)) %>% 
  pivot_longer(cols = c(wt_prc, med_prc, mn_prc),
               names_to = "metric",
               values_to = "avg")

price_avgs %>%   
  ggplot(mapping = aes(x = date, y = avg, col = metric)) +
  geom_line(size = 1.2) +
  facet_wrap(~reprod, scales = "free") +
  xlab("Date") +
  ylab("Price (cents per pound)") +
  theme_dark() +
  scale_color_manual(name = "Avg",
                     values = c("med_prc" = "pink", "mn_prc" = "blue", "wt_prc" = "orange"),
                     labels = c("Median", "Mean", "Wt.Mean")) +
  theme(legend.position = c(.95, .375))

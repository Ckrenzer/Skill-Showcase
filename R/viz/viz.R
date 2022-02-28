# Packages --------------------------------------------------------------------
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(patchwork)) install.packages("patchwork"); library(patchwork)


# Import data from tidyverse.R ------------------------------------------------
source("R/cleaning_and_manipulation/tidyverse.R")
rm(weather, weekly_sales, month_nums, default_values, laglen, lagsum)


# Functions -------------------------------------------------------------------
## My approach here doesn't work that well in R. Essentially, I take the
## previous n elements from the input x and take their mean.
sma <- function(x, prev_periods = 0L){
  prev_periods <- as.integer(prev_periods)
  stopifnot(length(prev_periods) == 1L,
            !is.na(prev_periods),
            prev_periods >= 0L,
            length(x) > prev_periods)
  
  ma <- c(rep(NA, prev_periods), double(length(x) - prev_periods))
  for(i in (prev_periods + 1):length(x)){
    ma[i] <- mean(x[(i - prev_periods):i], na.rm = TRUE)
  }
  ma
}


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
  group_by(reprod) %>% 
  mutate(ma = sma(med_prc, 10L)) %>% 
  pivot_longer(cols = c(wt_prc, med_prc, mn_prc, ma),
               names_to = "metric",
               values_to = "avg")

price_avgs %>%   
  ggplot(mapping = aes(x = date, y = avg, col = metric)) +
  geom_line() +
  facet_wrap(~reprod, scales = "free") +
  xlab("Date") +
  ylab("Price (cents per pound)") +
  theme_dark() +
  scale_color_manual(name = "Avg",
                     values = c("med_prc" = "pink",
                                "mn_prc" = "blue",
                                "wt_prc" = "orange",
                                "ma" = "black"),
                     labels = c("Median", "Mean", "Wt.Mean", "Mv.Avg")) +
  theme(legend.position = c(.95, .375))

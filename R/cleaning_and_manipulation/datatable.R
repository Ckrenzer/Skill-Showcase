# Description -----------------------------------------------------------------
# Performs the same operations as those in tidyverse.R.
# I'm a dplyr guy at heart, but I understand why people might feel compelled
# to use data.table. An often under-rated reason is its backwards compatibility.


# Packages --------------------------------------------------------------------
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(stringr)) install.packages("stringr"); library(stringr)


## Cattle market reports
cattle <- fread("data/csv/lmjr.csv",
                colClasses = c(
                  "character",
                  "character",
                  "double",
                  "character",
                  "double",
                  "double",
                  "character",
                  "character"),
                nThread = 8)

# Weather data from a facility near the sale barns in the cattle data set
# Data pulled from this link via web scraping behind the scenes:
# http://climate.colostate.edu/data_access.html
# (I took some liberties to showcase different cleaning techniques)
weather <- fread("data/csv/cheraw_1_n_weather_station_data.csv",
                 colClasses = c("character",
                                "character",
                                "character",
                                "character",
                                "character"),
                 nThread = 8)


# Formatting ------------------------------------------------------------------
# Standardizing column names and date formatting
setnames(cattle, old = colnames(cattle), str_to_lower(colnames(cattle)))
cattle[, date := as.Date(date, "%m-%d-%Y")]

# Date formatting
month_nums <- structure(seq_along(month.abb), names = month.abb)
weather[, `:=`(c("d", "m", "y"), tstrsplit(record_date, "-"))]
weather <- weather[as.integer(y) >= 2016L]
weather[, m := month_nums[m]]
weather[, record_date := as.Date(paste(d, m, y, sep = "-"), "%d-%m-%Y")]
weather[, `:=`(d = NULL, m = NULL, y = NULL)]

# According to the documentation page, http://climate.colostate.edu/readme.html,
# 'T' corresponds to 'trace' (very little precipitation)
# 'M' corresponds to 'missing'
# 'S' does not have a mapping in Colorado State's README so it becomes NA
default_values <- function(x){
  # The 'otherwise leave it alone' case
  default <- rep_len(TRUE, length(x))
  suppressWarnings(as.double(
    fcase(x == "T", "0",
          x == "M", "NA",
          x == "S", "NA",
          default, x)
  ))
}


# ALL SET UP TO THIS POINT








weather <- mutate(weather, across(where(is.character), default_values))


# Grouping --------------------------------------------------------------------
# When does the minimum price appear for each cattle reproductive status?
cattle %>% 
  group_by(reprod) %>% 
  filter(price == min(price)) %>% 
  ungroup() %>% 
  distinct(reprod, .keep_all = TRUE) %>% 
  dplyr::select(date, reprod)

# Which sale had the most buyers?
cattle %>% 
  distinct(buyer, date) %>% 
  count(date) %>% 
  arrange(desc(n))

# Which three buyers bought the most cattle at each sale?
cattle %>%
  group_by(date, buyer) %>%
  summarize(quantity = sum(quantity)) %>% 
  slice(1:3)

# Aggregate market reports by price
weekly_sales <- cattle %>% 
  group_by(date, reprod) %>% 
  summarize(avg_price = median(price), .groups = "drop")


# Pivot Operations ------------------------------------------------------------
# How many weeks did each cattle reproductive status have zero reported sales?
weekly_sales %>% 
  filter(!is.na(reprod)) %>% 
  pivot_wider(names_from = reprod, values_from = avg_price) %>% 
  summarize(
    across(
      where(~ class(.x) == "numeric"),
      ~ sum(is.na(.x))
    )
  )


# Joining ---------------------------------------------------------------------
# How was the weather on sale days?
business_climate <- left_join(weekly_sales, weather,
                              by = c("date" = "record_date"))


# Lags ------------------------------------------------------------------------
lagsum <- function(x, n = laglen){
  # Replaces NA values with zeros--this implementation is not perfect
  # and will only calculate the sum of those previous n weeks for which
  # there is data instead of returning NA when a missing value is found
  # within the 'set' of n values
  x[is.na(x)] <- 0
  cs <- cumsum(x)
  suml <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
  suml[n] <- NA
  suml
}
# I imagine that the amount of snowfall in the previous two weeks would
# affect the sales of cattle
laglen <- 2
business_climate %>% 
  mutate(price_l4 = lag(snow, n = laglen),
         sum_price_l4 = lagsum(price_l4))

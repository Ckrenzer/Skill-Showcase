# Description -----------------------------------------------------------------
# This script performs cleaning operations and simple calculations on tabular
# data.
# Performs the same operations as those in tidyverse.R.


# Notes -----------------------------------------------------------------------
# This file uses more intermediate values than are probably necessary.
# Sometimes this choice is to preserve the 80-characters-per-line convention,
# while other times the intermediate values simply make the code
# easier to read.


# Packages --------------------------------------------------------------------
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(clock)) install.packages("clock"); library(clock)


# Data import -----------------------------------------------------------------
## Cattle market reports
cattle <- fread("data/csv/ljmr.csv",
                colClasses = c(
                  "character",
                  "character",
                  "double",
                  "character",
                  "double",
                  "double",
                  "character"),
                nThread = 8)

# Weather data from a facility near the sale barns in the cattle data set
weather <- fread("data/csv/cheraw_1_n_weather_station_data.csv",
                 colClasses = c("character",
                                "character",
                                "character",
                                "character",
                                "character"),
                 nThread = 8)


# Formatting ------------------------------------------------------------------
# Standardizing columns
setnames(cattle, old = colnames(cattle), str_to_lower(colnames(cattle)))
cattle[, date := as.Date(date, "%Y-%m-%d")]

# Date formatting
month_nums <- structure(seq_along(month.abb), names = month.abb)
weather[, `:=`(c("d", "m", "y"), tstrsplit(record_date, "-"))]
weather <- weather[as.integer(y) >= 2019L]
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
string_cols <- vapply(weather, is.character, logical(1))
set(weather,
    j = which(string_cols),
    value = lapply(weather[, ..string_cols], FUN = default_values))


# Grouping --------------------------------------------------------------------
# When does the minimum price appear for each cattle reproductive status?
## (Ties are broken by taking the first index returned by .I for each group)
minprice_indexes <- cattle[,
                           .(minprice = .I[price == min(price)][[1]]),
                           by = reprod]$minprice
cattle[minprice_indexes, .(date, reprod)]

# Which sale had the most buyers?
unique(cattle[, .(buyer, date)])[, .N, by = date][order(-N)]

# For each sale, which three buyers bought the most livestock?
##                         Aggregate to find top buyers                         Sort
top_quantities <- cattle[, .(quantity = sum(quantity)), keyby = .(date, buyer)][order(date, -quantity)]
top_quantities_indexes <- top_quantities[, .(indexes = .I[1:3]), by = .(date)]$indexes
top_quantities[top_quantities_indexes]

# Aggregate market reports by price
weekly_sales <- cattle[, .(avg_price = median(price)), keyby = .(date, reprod)]

# Aggregate weeks in weather to the next Tuesday
tuesday <- weekday(clock_weekdays$tuesday)
weather[, next_tues := as_date(as_naive_time(record_date) + (tuesday - as_weekday(record_date)))]
weather <- weather[,
                   .(maxtemp = mean(maxtemp), mintemp = mean(mintemp), pcpn = sum(pcpn), snow = sum(snow)),
                   next_tues]


# Pivot Operations ------------------------------------------------------------
# How many weeks did each cattle reproductive status have zero reported sales?
numeric_cols <- unique(weekly_sales[!is.na(reprod)]$reprod)
weekly_sales_wide <- dcast(weekly_sales,
                           formula = date ~ reprod,
                           value.var = "avg_price")
vapply(weekly_sales_wide[, ..numeric_cols], function(x) sum(is.na(x)), integer(1))

# Cheap and dirty way of inserting missing values for those
# sales when a given reproductive status was not purchased
weekly_sales <- melt(weekly_sales_wide,
                     id.vars = "date",
                     measure.vars = c("bull", "cow", "hfr", "str"),
                     value.name = "avg_price")


# Joining ---------------------------------------------------------------------
# How was the weather on sale days?
business_climate <- weather[weekly_sales, on = .(next_tues = date)]

# Alternatively, we could join by reference:
weekly_sales[weather, on = .(date = next_tues), `:=`(maxtemp = maxtemp,
                                                     mintemp = mintemp,
                                                     pcpn = pcpn,
                                                     snow = snow)]


# Lags ------------------------------------------------------------------------
# The amount of snowfall in the previous two weeks likely affects cattle sales
laglen <- 2
business_climate[, snowlag_sum := frollsum(x = snow, n = laglen)]
business_climate

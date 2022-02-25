# Import data from tidyverse.R ------------------------------------------------
source("R/cleaning_and_manipulation/tidyverse.R")
rm(weather, weekly_sales, month_nums, default_values, business_climate, laglen, lagsum)


# Data Import -----------------------------------------------------------------
buyers <- cattle$buyer
cristo <- read_lines("data/txt/cristo.txt")



cristo
# To do
## Perform a calculation using a lead or lag by group
## Add a key using row numbers (row_number())



# Notes -----------------------------------------------------------------------
# The select() function is a common function name, so the namespace should
# always be specified when using it (e.g. dplyr::select(), MASS::select(), etc.)


# Packages --------------------------------------------------------------------
if(!require(readr)) install.packages("readr"); library(readr)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
if(!require(stringr)) install.packages("stringr"); library(stringr)



# Data import -----------------------------------------------------------------
## Cattle market reports
cattle <- read_csv("data/csv/lmjr.csv",
                   col_types = list(col_date(format = "%m-%d-%Y"),
                                    col_character(),
                                    col_double(),
                                    col_character(),
                                    col_double(),
                                    col_double(),
                                    col_character(),
                                    col_character()),
                   num_threads = 8,
                   lazy = FALSE) %>% 
  rename_with(str_to_lower)

# Weather data from a facility near the sale barns in the cattle data set
# Data pulled from this link via web scraping behind the scenes:
# http://climate.colostate.edu/data_access.html
# (I took some liberties to showcase different cleaning techniques)
weather <- read_csv("data/csv/cheraw_1_n_weather_station_data.csv",
                    col_types = list("c", "c", "c", "c", "c"),
                    num_threads = 8,
                    lazy = FALSE)


# Formatting ------------------------------------------------------------------
# Date formatting
month_nums <- structure(seq_along(month.abb), names = month.abb)
weather <- weather %>%
  separate(record_date, into = c("d", "m", "y")) %>% 
  mutate(m = month_nums[m]) %>% 
  filter(as.integer(y) >= 2016) %>% 
  unite(col = record_date, d, m, y, sep = "-") %>% 
  mutate(record_date = as.Date(record_date, "%d-%m-%Y"))

# According to the documentation page, http://climate.colostate.edu/readme.html,
# 'T' corresponds to 'trace' (very little precipitation)
# 'M' corresponds to 'missing'
# 'S' does not have a mapping in Colorado State's README so it becomes NA
default_values <- function(x){
  parse_number(
    case_when(x == "T" ~ "0",
              x == "M" ~ "NA",
              x == "S" ~ "NA",
              TRUE ~ x)
  )
}
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




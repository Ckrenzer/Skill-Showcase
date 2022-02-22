# To do
## add a case_when() call in there somewhere.
## Perform a calculation using a lead or lag by group
## Add a key using row numbers (row_number())
## Find something to count()
## filter for first n rows by group
## separate() and unite()
## use a dictionary
## perform a join
######myt2 <- myt %>%
######  group_by(groupcol) %>%
######  slice(1:n)
## use an arrange() call


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

# Data pulled from this link via web scraping behind the scenes:
# http://climate.colostate.edu/data_access.html
weather <- read_csv("data/csv/cheraw_1_n_weather_station_data.csv",
                    col_types = list(col_date(),
                                     col_character(),
                                     col_character(),
                                     col_character(),
                                     col_character()),
                    num_threads = 8,
                    lazy = FALSE) %>% 
  rename(weather = `CHERAW 1 N`,
         max_temp = maxt,
         min_temp = mint)


# Formatting ------------------------------------------------------------------
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
weather <- weather %>% 
  mutate(across(where(is.character), default_values))


# Grouping --------------------------------------------------------------------
# When does the minimum price appear for each cattle reproductive status?
cattle %>% 
  group_by(reprod) %>% 
  filter(price == min(price)) %>% 
  ungroup() %>% 
  distinct(reprod, .keep_all = TRUE) %>% 
  dplyr::select(date, reprod)

# Aggregate market reports
weekly_sales <- cattle %>% 
  group_by(date, reprod) %>% 
  summarize(avg_price = median(price), .groups = "drop")


# Pivot Operations ------------------------------------------------------------
# How many weeks did each cattle reproductive status have zero reported sales?
weekly_sales %>% 
  filter(!is.na(reprod)) %>% 
  pivot_wider(names_from = reprod,
              values_from = avg_price) %>% 
  summarize(
    across(
      where(~ class(.x) == "numeric"),
      ~ sum(is.na(.x))
    )
  )


# Joins -----------------------------------------------------------------------


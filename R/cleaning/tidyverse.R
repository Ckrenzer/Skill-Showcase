# To do
## add a case_when() call in there somewhere.
## Perform a calculation using a lead or lag by group
## Add a key using row numbers (row_number())
## Find something to count()
## filter for first n rows by group
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
cattle <- read_csv("data/csv/LJMR.csv",
                   col_names = TRUE,
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

## Movie reviews
reviews <- read_csv("data/csv/netflix-rotten-tomatoes-metacritic-imdb.csv",
                    col_names = TRUE,
                    col_select = list(title = Title,
                                      release_date = `Release Date`,
                                      numvotes = `IMDb Votes`),
                    col_types = list(col_character(),
                                     col_character(),
                                     col_integer()),
                    num_threads = 8,
                    lazy = TRUE)


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
## Planning to join the movie reviews to the cattle market reports
## but the movie data needs a date conversion first
month_nums <- structure(seq_along(month.abb), names = month.abb)
reviews <- reviews %>% 
  separate(col = release_date,
           into = c("d", "m", "y"),
           sep = " ",
           remove = FALSE) %>% 
  mutate(m = month_nums[m]) %>% 
  filter(as.integer(y) >= 2016) %>% 
  unite(col = formatted_date, m, d, y, sep = "-") %>% 
  mutate(formatted_date = as.Date(formatted_date, "%m-%d-%Y"))

# Which movies took place at the same time as a market report sale?
left_join(weekly_sales, reviews, by = c("date" = "formatted_date")) %>% 
  filter(!is.na(title)) %>% 
  distinct(date, title)
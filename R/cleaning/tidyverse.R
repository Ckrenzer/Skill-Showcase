# To do
## add a case_when() call in there somewhere.
## Find another source of data to join to the La Junta data.
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

# Data import -----------------------------------------------------------------
lajunta <- read_csv("data/csv/LJMR.csv",
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
                    lazy = FALSE)


# Grouping --------------------------------------------------------------------
# When does the minimum price appear for each reproductive status?
lajunta %>% 
  group_by(Reprod) %>% 
  filter(Price == min(Price))

# Aggregate market reports
weekly_sales <- lajunta %>% 
  group_by(Date, Reprod) %>% 
  summarize(avg_price = median(Price), .groups = "drop")


# Pivot Operations ------------------------------------------------------------
# How many weeks did each reproductive status have zero reported sales?
weekly_sales %>% 
  filter(!is.na(Reprod)) %>% 
  pivot_wider(names_from = Reprod,
              values_from = avg_price) %>% 
  summarize(
    across(
      where(~ class(.x) == "numeric"),
      ~ sum(is.na(.x))
    )
  )



# Joins -----------------------------------------------------------------------

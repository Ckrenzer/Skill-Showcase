# Packages --------------------------------------------------------------------
if(!require(readr)) install.packages("readr"); library(readr)


# Data Import -----------------------------------------------------------------
buyers <- unique(cattle$buyer)
cristo <- read_lines("data/txt/cristo.txt")
cristo <- cristo[cristo != ""]


cristo


str_view_all(cristo, "[a-z]*[\\.!]")
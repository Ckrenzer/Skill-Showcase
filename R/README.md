# R Directories

## [cleaning_and_manipulation/](https://github.com/Ckrenzer/Skill-Showcase/tree/main/R/cleaning_and_manipulation)
Showcases my approach to cleaning, along with common calculations on tabular data.

Each operation results in values identical to those in *tidyverse.R* except for small differences between two functions calculating the rolling sum of a lagged column. `data.table::frollsum()` in *datatable.R* and my custom `lagsum()` in *tidyverse.R* handle missing values differently but otherwise perform the same calculation.


## [metaprogramming/](https://github.com/Ckrenzer/Skill-Showcase/tree/main/R/metaprogramming)
Showcases some key skills pertaining to metaprogramming (aka tidy evaluation) and its application in data analysis.

## [reactive_programming/](https://github.com/Ckrenzer/Skill-Showcase/tree/main/R/reactive_programming)
Everyone loves a good Shiny app.

## [regex/](https://github.com/Ckrenzer/Skill-Showcase/tree/main/R/regex)
stringr reigns supreme in R's regular expression scene. Base R's string manipulation functions are tedious due to their inconsistencies. Groups? Quantifiers? Anchors? stringr makes it happen with ease.

## [viz/](https://github.com/Ckrenzer/Skill-Showcase/tree/main/R/viz)
R's data visualization capabilities are second to none. Among my friends who only know R peripherally, ggplot2 is always the thing they mention.
# Packages --------------------------------------------------------------------
if(!require(rlang)) install.packages("rlang"); library(rlang)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(bench)) install.packages("bench"); library(bench)


# The Quote and Unquote Pattern -----------------------------------------------
# (useful for problems involving indirect reference)
plot_values <- function(df, x, y, geom){
  x_expression <- enexpr(x)
  y_expression <- enexpr(y)
  geom_expression <- enexpr(geom)
  
  plot_expr <- expr(
    ggplot(df) +
      (!!geom_expression)(mapping = aes(x = !!x_expression,
                                        y = !!y_expression))
  )
  
  eval_tidy(plot_expr)
}
# Making the same graph with different geoms
funs <- exprs(geom_point, geom_line, geom_col)
my_plots <- list()
for(fun in funs){
  my_plots[[expr_text(fun)]] <- plot_values(df = iris, x = Sepal.Length, y = Sepal.Width, geom = !!fun)
}
my_plots$geom_point
my_plots$geom_line
my_plots$geom_col




# do not use this--unoriginal and plagiaristic!
general_paste <- function(fun, ...){
  fun_expr <- enexpr(fun)
  
  paste_expr <- expr((!!fun_expr)(...))
  eval_tidy(paste_expr)
}
general_paste(paste0, "hey", "hi", c("who are you?", "me?"))
general_paste(str_c, "hey", "hi", c("who are you?", "me?"))
general_paste(paste, "hey", "hi", c("who are you?", "me?"))



# yes--your own implementation of dplyr::select()!
my_select <- function(df, ...){
  selected_cols <- enexprs(...)
  my_strings <- unlist(lapply(selected_cols, expr_text))
  my_strings <- stringr::str_remove_all(my_strings, "^\"|\"$")
  df[, colnames(df) %in% my_strings, drop = FALSE]
}
my_select(iris, "Species", "Sepal.Length")
my_select(iris, Species, Sepal.Length)


# Manipulating Expressions ----------------------------------------------------


# Benchmarks ------------------------------------------------------------------
bench::mark(iterations = 15, check = FALSE, memory = TRUE, time_unit = "ms",
            
            my_select(iris, "Species", "Sepal.Length"),
            my_select(iris, Species, Sepal.Length),
            
            dplyr::select(iris, "Species", "Sepal.Length"),
            dplyr::select(iris, Species, Sepal.Length)
)

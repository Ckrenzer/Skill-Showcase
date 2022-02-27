# Who was the Fed Chairman on the Ides of March?


# Description -----------------------------------------------------------------
# Builds a regex pattern that extracts the name of the Fed Chairman from the
# March 15th, 2020 FOMC Minutes transcript.


# Packages --------------------------------------------------------------------
if(!require(readr)) install.packages("readr"); library(readr)
if(!require(stringr)) install.packages("stringr"); library(stringr)


# Data Import -----------------------------------------------------------------
fed <- read_lines("data/txt/fomcminutes20200315.txt", num_threads = 8)


# The Pattern -----------------------------------------------------------------
# The FOMC Minutes record all members present at the meeting, listing each
# participant's name and title at the start of the meeting.
# Finding the chairman should be pretty easy, then, no?
# Simply assemble a name and then use a lookahead for "chair".
name <- "[a-z]+\\.*"
middle <- "[a-z]*\\.*"

# This pattern does not capture anything with commas
pattern <- str_glue("{name}\\s*{middle}\\s+{name}")
lookahead <- "(?=[\\ \\t]+chair)"
rgexpr <- str_glue("{pattern}{lookahead}")


# Tests -----------------------------------------------------------------------
# Regular expressions are unruly things. Always test them thoroughly.
tests <- list(
  TRUE_tests = c(
    "jerome hayden powell chair",   #full name
    "j. hayden powell chair",       #first name with period
    "j hayden powell chair",        #first name no periods
    "jerome h. powell chair",       #middle name with period
    "jerome h powell chair",        #middle name no periods
    "jerome hayden p. chair",       #last name with period
    "jerome hayden p chair",        #last name no periods
    "jerome  hayden powell chair",  #two spaces after first name
    "jerome hayden  powell chair",  #two spaces after middle name
    "jerome hayden powell  chair",  #two spaces after last name
    "jerome  hayden powell chair",  #tab after first name
    "jerome hayden powell chair",   #tab after middle name
    "jerome hayden powell  chair",  #tab after last name
    "jay hayden powell chair",      #realistic nickname
    "jerome hayden powell chairman",#chairman instead of chair
    "jerome h.. powell chair",      #middle name two periods (a typo)
    "j.h. powell chairman",         #first and middle name abbreviated
    "joe mama chairman"             #only two names used ('no middle name')
  ),
  FALSE_tests = c(
    "powell chair",                 #only last name provided
    "jerome hayden powell",         #no lookahead match
    "jerome hayden powell, chair",  #comma present
    "jerome hayden powell c.",      #abbreviation for chairman with period (v1)
    "jerome hayden powell c",       #abbreviation for chairman no periods  (v1)
    "jerome hayden powell chm.",    #abbreviation for chairman with period (v2)
    "jerome hayden powell chm",     #abbreviation for chairman no periods  (v2)
    "jerome hayden powell chmn.",   #abbreviation for chairman with period (v3)
    "jerome hayden powell chmn"     #abbreviation for chairman no periods  (v3)
  )
)

results <- lapply(tests, function(x) str_detect(x, rgexpr))
passed_all_tests <- all(results$TRUE_tests) && !any(results$FALSE_tests)
if(!passed_all_tests){
  message("Tests failed--your pattern needs improvement. Press Esc to continue.")
  # I finally found a use for the repeat loop--interrupting interactive sessions
  repeat{}
}


# Usage -----------------------------------------------------------------------
# It looks like the pattern passed all of the tests! Let's find that
# Fed Chairman.
chairman <- str_extract(fed, rgexpr)[[1]]
message(str_glue("Fed Chairman, {str_to_title(chairman)}, has no idea what's coming."))

# All our pattern really did was capture two or three words preceding a word
# starting with 'chair'. In fact, if we extract all matches instead of the
# first match we can see the other matches this pattern identifies:
str_extract_all(fed, rgexpr)[[1]]
#> "jerome h. powell"           "c. williams vice"
#> "adviser to the"             "adviser to the"
#> "a result the"               "this discussion the"
#> "commentary had interpreted"


# Pattern Adjustments ---------------------------------------------------------
# We cannot guarantee that our pattern's first match will always return the
# name of the sitting Fed Chairman. I would not adjust the pattern further
# if I were doing this in a normal workflow--it would be easier to filter
# out the remaining matches.
#
# This is a script showcasing regular expressions, however...
# We'll have to add more assumptions about our data to the pattern.
# We can narrow the number of matches by adding a lookbehind.
# I assume the word 'present' will always precede the Fed Chairman's name.
lookbehind <- "(?<=present\\s)"
rgexpr <- str_glue("{lookbehind}{pattern}{lookahead}")

# Did someone say Jerome H. Powell?
str_extract_all(fed, rgexpr)[[1]]
#> "jerome h. powell"


# Bonus -----------------------------------------------------------------------
# Reverses input by pairs of words
unyoda <- function(x){
  x <- str_remove_all(x, "[,\\.!?]")
  n <- str_count(x, " ") + 1
  if(n == 0){
    return(x) 
  } else if(n == 1){
    return(str_replace(x, "([a-z]+) ([a-z]+)", "\\2 \\1"))
  } else if(n %% 2 == 1){
    stop("Supply an even number of words to this function.") 
  }
  numgroups <- n / 2
  pattern <- str_c(rep("([a-z]+ [a-z]+)", n / 2), collapse = " ")
  replacement <- c()
  for(i in seq_len(numgroups)){
    replacement <- c(str_glue("\\{i}"), replacement)
  }
  replacement <- str_c(replacement, collapse = " ")
  str_replace(x, pattern, replacement)
}
unyoda("printing money, jerome loves")

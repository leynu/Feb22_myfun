library(tidyverse)

my_data <- tribble(
  ~id, ~colA, ~colB,
  1, "A",   "A",
  2, "A",   "a",
  3, "A",   NA
)
my_data

# Issue: Does not capture the last row where ColB has a NA value
my_data %>% 
  filter(colA != colB )

# Solutions 1: Create a function
# -----------------------------
`%ne%` <- function(x, y){
  x != y | is.na(x) & !is.na(y) | !is.na(x) & is.na(y) 
}

my_data %>% 
  filter(colA %ne% colB )

# Solutions 2: Create a function
# -----------------------------

`%ne%` <- function(x, y){
  z <- x != y 
  return(is.na(z)|z)
}

my_data %>% 
  filter(colA %ne% colB )

# Solutions 3: Using case_when inside filter
# ------------------------------------------

my_data %>% 
  filter(
    case_when(
      colA == colB ~ FALSE,
      TRUE ~ TRUE))

# Solution 4: using vec_equal from vctrs package 
# ----------------------------------------------
my_data %>% 
  filter(
    !vctrs::vec_equal(colA, colB, na_equal = TRUE))

# vctrs::vec_equal("a", "a") #TRUE
# vctrs::vec_equal("a", "b") #FALSE
# vctrs::vec_equal("a", NA) #NA
# vctrs::vec_equal("a", NA, na_equal = TRUE) #FALSE

# Solution 5:  We can use Vectorize() to vectorize non-vectorized functions
# -------------------------------------------------------------------------
my_data %>% 
  filter(!Vectorize(identical)(colA, colB))

# identical("a", "a") #TRUE
# identical("a", "b") #FALSE
# identical("a", NA) #FALSE


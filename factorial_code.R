### Advanced R Programming
# Part 1: Factorial Function

# Loading packages
library(dplyr)
library(purrr)
library(bench)

# Factorial loop
factorial_loop <- function(n) {
  # Initial checks
  stopifnot(n >= 0)
  if(n == 0) return(1)
  # Loop
  m <- 1
  for (i in 1:n) {
    m = m * i
  }
  m
}

# Factorial reduce
factorial_reduce <- function(n) {
  # Initial checks
  stopifnot(n >= 0)
  if(n == 0) return(1)
  # reduce from purrr package
  reduce(1:n, ~.x * .y)
}

# Factorial func
factorial_func <- function(n) {
  # Initial checks
  stopifnot(n >= 0)
  if(n == 0) return(1)
  # Starts recursion
  n * factorial_func(n - 1)
}

# Factorial mem
fact_tbl <- rep(NA, 12) # Create vector of NA to save values
factorial_mem <- function(n) {
  # Initial checks
  stopifnot(n >= 0)
  if(n == 0) return(1)
  # Check if the value is already calculated
  if(!is.na(fact_tbl[n])) {
    fact_tbl[n]
  } else {
    fact_tbl[n - 1] <<- factorial_mem(n - 1)
    n * factorial_mem(n - 1)
  }
}

results <- bench::mark(
  factorial_loop = map(1:12, factorial_loop),
  factorial_reduce = map(1:12, factorial_reduce),
  factorial_func = map(1:12, factorial_func),
  factorial_mem = map(1:12, factorial_mem)
)

# Write results txt
map_df(results, ~as.character(.x)) %>%
  select(1:6) %>% write.table(file = "./factorial_output.txt")

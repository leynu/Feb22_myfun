### File name     : Function_UsefulTools.R
### Project       : 1250579_Roche_OR_DUS_IR_2020
### Author        : Lnunez
### Date          : 2022-02-11
### Platform      : R version 3.5.0 (2018-04-23) / RStudio / Linux
### ------------------------------------------------------------------

# useful function
first_equal_to <- function(x, value) (x == value) & (cumsum(x == value) == 1)  

last_equal_to <- function(x, value) (x == value) & (cumsum(x == value) == sum(x == value))

# took from 
# https://stackoverflow.com/questions/31126726/efficient-and-accurate-age-calculation-in-years-months-or-weeks-in-r-given-b
age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

# took from 
# https://community.rstudio.com/t/grouping-dates-together-into-periods/65044/5
create_date_periods <- function(dates, time_period = 200) {
  
  # create a vector to hold the results
  return_vector <- structure(integer(length(dates)), class = "Date")
  
  for(i in seq_along(dates)) {
    # if this is the first record, set to minimum
    if (i == 1) min_date <- dates[i]
    
    # if date less than minimum date + time period, use minimum date
    if(dates[i] <= min_date + time_period) {
      
      return_vector[i] <- min_date
      
      # otherwise update minimum date to current date
    } else {
      min_date <- dates[i]
      return_vector[i] <- min_date
    }}
  
  return(return_vector)
}

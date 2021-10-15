library(tidyverse)
##question one
column_one <- c(2,3,4,5,300)
column_two <- c("hello", "welcome", "to","Econ","145")
column_three <- c(0,0,17,NA,15)
tibble_one <- tibble(column_one,column_two,column_three)
##question two
summary_stats_column_one <- c(mean(column_one),sd(column_one),var(column_one))
summary_stats_column_three <- c(mean(column_three,na.rm = T),sd(column_three,na.rm = T),var(column_three,na.rm = T))
##question three
typeof_one <- typeof(column_one)
typeof_two <- typeof(column_two)
## double is numeric, character is letters/words
na_column_three <- is.na(column_three)
column_four <- c(median(column_one),median(column_one),median(column_one),median(column_one),median(column_one))
tibble_two <- tibble(column_one,column_two,column_three,column_four)


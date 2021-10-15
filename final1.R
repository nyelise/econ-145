PERMID <- 8045494
library(tidyverse)
library(readr)
setwd("/Users/elisenguyen/Desktop/econ\ 45")
f1_dat <- read_csv("assign_2.csv")

## 1
f1_unique <- f1_dat %>%
  unique()
total_responses <- 281

## 2
cleaned_min <- function(f1_unique){
    for(i in 1:1)
      return(min(f1_unique))
}

cleaned_max <- function(f1_unique){
  for(i in 1:1)
    return(max(f1_unique))
}

cleaned_mid <- function(f1_unique){
  for(i in 1:1)
    return(mean(f1_unique))
}

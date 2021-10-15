PERMID <- 8045494
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
install.packages("OneR")
library(OneR)
setwd("/Users/elisenguyen/Desktop/econ\ 45")
complaints <- read_csv("complaints.csv")
hotness <- read_csv("hotness.csv")

## 1
complaints <- clean_names(complaints)
hotness <- clean_names(hotness)

## 2
complaints_cleaned <- complaints %>%
  separate(date, "month", "([[:digit:]]+)", remove = FALSE) %>%
  extract(date, "month", regex = "([[:digit:]]+)", remove = FALSE) %>%
  separate(date,  "day",  "([[:digit:]]+)", remove = FALSE) %>%
  extract(date, "day", regex = "/([[:digit:]]+)/", remove = FALSE) %>%
  separate(date, "year",  "([[:digit:]]+)", remove = FALSE) %>%
  extract(date, "year", regex = "(\\d\\d\\d\\d)", remove = FALSE) %>%
  select(month, day, year, time_of_issue, form, method, issue, zip)

## 3
hotness_cleaned <- hotness %>%
  select(month_date_yyyymm, postal_code, zip_name, nielsen_hh_rank, hotness_rank, hotness_score) %>%
  separate(month_date_yyyymm, into = c("year", "month"), 4)

## 4
clean_merge <- inner_join(complaints_cleaned, hotness_cleaned, by = c("month" = "month", "year" = "year", "zip" = "postal_code"), suffix = c(".complaints_cleaned", ".hotness_cleaned"))

## 5
clean_merge %>%
  summarize(avg_hot = mean(hotness_rank))
avg_hot <- 5730.578

clean_merge %>%
  summarize(med_hot = median(hotness_rank))
med_hot <- 4682

## 6
comp_type <- clean_merge%>%
  select(issue, hotness_rank)%>%
  group_by(issue)%>%
  summarize_at(vars(hotness_rank), list(mean, median, min, max))

comp_type <- comp_type %>%
  rename(
    type = issue,
    mean = fn1,
    median = fn2,
    min = fn3,
    max = fn4
  )
comp_type$mean <- round(comp_type$mean, 1)

## 7 
meth_type <- clean_merge%>%
  select(method, hotness_rank)%>%
  group_by(method)%>%
  summarize_at(vars(hotness_rank), list(mean, median, min, max))

meth_type <- meth_type %>%
  rename(
    type = method,
    mean = fn1,
    median = fn2,
    min = fn3,
    max = fn4
  )

meth_type$mean <- round(meth_type$mean, 1)

## 8
last_tib <- clean_merge %>%
  separate(zip_name, into = c("city", "state"), sep = ",")

## 9
last_tib <- last_tib %>%
  mutate(state = trimws(state))
most <- count(last_tib, vars = state)
most_calls <- "ca"

## writeup
clean_merge2 <- clean_merge%>%
  filter(issue == "Robocalls")

clean_mergev2 <- count(clean_merge2, hotness_score)

ggplot(clean_mergev2, aes(x=hotness_score, y = n)) +
  geom_density(stat='identity')+
  ylim(0,3)+
  xlab("Hotness Score")+
  ylab("Number of Robocalls")



clean_merge3 <- clean_merge%>%
  filter(issue == "Telemarketing (including do not call and spoofing)")
clean_mergev3 <- count(clean_merge3, hotness_score)

ggplot(clean_mergev3, aes(x=hotness_score, y = n)) +
  geom_density(stat='identity')+
  ylim(0,3)+
  xlab("Hotness Score")+
  ylab("Number of Telemarketing Calls")+
  stat_smooth()
  
  
  






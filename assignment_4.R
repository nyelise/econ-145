PERMID <- 8045494
library(tidyverse)
library(readr)
library(janitor)
## question 1
setwd("/Users/elisenguyen/Desktop/econ\ 45")
calls <- read_csv("fcc.csv")
## question 2
calls <- calls %>%
  clean_names()
## question 3 
## 3a
calls <- calls %>%
  extract(col = location_center_point_of_the_zip_code, into = "state", regex = "([A-Z]+)", remove = FALSE)
## 3b
calls_zip <- calls %>%
  extract(col = caller_id_number, into= "area_code", regex = "(\\d\\d\\d)", remove = FALSE)
## 3c
calls_zip_sep <- calls %>%
  separate(col = time_of_issue, into = c("time_of_issue","am_pm"), sep = "\\ ", remove = FALSE)
## 3d
calls_zip_sep <- calls_zip_sep %>%
  mutate(am = ifelse(str_detect(calls_zip_sep$am_pm, "^a|^A"), 1,0))
## question 4
## 4a
delay <- calls %>%
  select(ticket_id, ticket_created, date_of_issue)
## 4b
delay_sep <- delay %>%
  separate(col = ticket_created, into = c("month_created", "day_created", "year_created"), sep = "/")%>%
  extract(col = year_created, into = "year_created", regex = "(\\d{4})")
## 4c
delay_sep2 <- delay_sep %>%
  separate(col = date_of_issue, into = c("month_occurred", "day_occurred", "year_occurred"), sep = "/")%>%
  extract(col = year_occurred, into = "year_occurred", regex = "(\\d{4})")
## 4d
time_elapsed <- delay_sep2 %>%
  mutate_if(is.character,as.numeric)%>%
  filter(year_occurred == year_created & month_occurred == month_created)%>%
  mutate(day_delay = (day_created - day_occurred))
## 4e
monthly_delay <- time_elapsed %>%
  group_by(month_occurred)%>%
  summarize("average_day_delay" = mean(day_delay, na.rm = TRUE))
## 4f
july_delay <- 0.79

## writeup types of calls
types_of_calls <- calls %>%
  count(issue, sort = T)

barplot(types_of_calls$n,types_of_calls$n, main = "Types of Calls", names.arg=c("Telemarketing", "Robocalls"))

## writeup location
place_of_calls <- calls_zip %>%
  count(state, sort = T)
zip_of_calls <- calls_zip %>%
  count(zip, sort = T)
barplot(place_of_calls$n, main = "States Most Affected", names= place_of_calls$state)

## writeup time
time_of_calls <- calls %>%
  count(time_of_issue, sort = T)
year_of_calls <- time_elapsed %>%
  count(month_created, sort = T)
barplot(year_of_calls$n, main = "Calls During Months", names= year_of_calls$month_created)


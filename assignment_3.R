PERMID <- 8045494
library(tidyverse)
library(readr)
setwd("/Users/elisenguyen/Desktop/econ\ 45")
## question 1
airbnb <- read_csv("assign_3.csv")
## question 2a
airbnb_properties <- airbnb %>%
  select(id, property_type, price, host_is_superhost, room_type)
## question 2b
airbnb_properties %>%
  filter(property_type == "Apartment" | property_type == "House") %>%
  count(price, sort = T)
most_common_price4 <- 95
## question 2c
airbnb_properties <- airbnb_properties %>%
  mutate(is_superhost = ifelse(host_is_superhost == "TRUE",1,0))
## question 2d
airbnb_properties %>%
  group_by(host_is_superhost)%>%
  count(host_is_superhost)
superhosts <- 218
nonsuperhosts <- 781
other <- 1
## question 2e
mean(airbnb_properties$price)
average_price <- 127.195
## question 2f
averages_by_host <- airbnb_properties %>%
  group_by(is_superhost) %>%
  summarise(average_prices = mean(price))
##question 3a
top20_neighbourhoods <- airbnb %>%
  count(neighbourhood, sort = T) %>%
  filter(!is.na(neighbourhood)) %>%
  head(21)
##question 3b
top5 <- airbnb %>%  
  group_by(neighbourhood) %>%
  summarise(average_price = mean(price))%>%
  filter(neighbourhood == "Capitol Hill"|
           neighbourhood == "Ballard"|
           neighbourhood == "Belltown"|
           neighbourhood == "Queen Anne"|
           neighbourhood == "Minor"
  )
##question 3c
airbnb <- airbnb %>%
  mutate(adjusted_price = price + cleaning_fee)
##question 3d and 3e
top5_adjusted <- airbnb %>%  
  group_by(neighbourhood) %>%
  summarise(average_price = mean(price, na.rm = T), average_adjusted_price = mean(adjusted_price, na.rm = T)) %>%
  filter(neighbourhood == "Capitol Hill"|
           neighbourhood == "Ballard"|
           neighbourhood == "Belltown"|
           neighbourhood == "Queen Anne"|
           neighbourhood == "Minor")%>%
  mutate(average_difference = abs(average_adjusted_price- average_price))

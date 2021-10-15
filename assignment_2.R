PERMID <- 8045494
library(tidyverse)
library(readr)
setwd("/Users/elisenguyen/Desktop/econ\ 45")
assign_2_dat <- read_csv("assign_2.csv")
# question one
view(assign_2_dat)
a2_unique <- assign_2_dat %>%
  distinct()
total_response <- count(a2_unique)
total_response <- 281
# question two
typeof(a2_unique$visits)
typeof(a2_unique$days_attend)
a2_unique$visits <- gsub("\\-.*", "", a2_unique$visits)
a2_unique$days_attend <- gsub("\\-.*", "", a2_unique$days_attend)
a2_unique <- type_convert(a2_unique)
# question three
a2_unique <- a2_unique %>%
  mutate(local = ifelse(zip %in% c(80305,80309,80302,80304,80303,80306),1,0))
# question four
local_perc <- a2_unique %>%
  summarize(local_perc=mean(local))
local_perc <- 0.797
# question five
female_perc <- mean(na.omit(a2_unique$gender) == "Female")
# question six
gend_age_loc <- a2_unique %>%
  select(gender,age,local) %>%
  group_by(local, gender, age) %>%
  summarise(num_obs = n()) %>%
  arrange(desc(local),age,gender)
# question seven
big_spender_zip <- a2_unique %>%
  select(spend_food_drink_total,zip)%>%
  filter(spend_food_drink_total=="$1,000-$1,999")%>%
  group_by(zip)%>%
  summarize(num_obs=n())
  
  
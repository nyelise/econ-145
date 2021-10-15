PERMID <- 8045494
library(tidyverse)
library(janitor)
## part 1, debugging
## 1a
setwd("/Users/elisenguyen/Desktop/econ\ 45")
cms <- read_csv("cms_data.csv")

## 1b
q1_b <- cms %>% 
  mutate(Start.Date = format(as.character(as.Date(cms$Start.Date, "%d/%m/%y"), "%m/%d/%Y")))
## 1c
q1_c<-cms %>%
  na_if(.,"N/A")

## 1d
q1_d <- q1_c %>%
  filter(!is.na(Excess.Readmission.Ratio))

##1e
cms <- q1_c %>%
  filter(Number.of.Readmissions != "Too Few to Report")
cms$Number.of.Readmissions <- as.numeric(cms$Number.of.Readmissions)
cms %>%
  summarise(minreadms = min(Number.of.Readmissions))
minreadms <- 11

## part 2, coding assigment
## 1
players <- read_csv("Players.csv")%>%
  clean_names()
season_stats <- read_csv("Seasons_Stats.csv")%>%
  clean_names()
player_data <- read_csv("player_data.csv") %>%
  clean_names()

## 2a
player_key <- c("x","player")

## 2b
player_data_key <- c("name", "birth_date")

##2c
season_stats_key <- c("x", "player")

## 3a
season_stats <- season_stats %>%
  mutate(season_stats, rpg = trb_2/g) %>%
  mutate(season_stats, ppg = pts/g)%>%
  mutate(season_stats, apg = ast_2/g)%>%
  mutate(season_stats, mpg = mp/g)

## 3b


# 3c
percentile_20 <- ecdf(as.numeric(season_stats$ppg))
percentile_20(20)
percentile_20 <- 0.9449318

# 3d
season_stats_over20 <- filter(season_stats,(ppg) >= 20)

## 3e
season_stats_over20 <- season_stats_over20 %>%
  mutate(year,decade = year-year%%10)

## 3f
season_stats_over20 %>%
  distinct(player, .keep_all = TRUE)%>%
  group_by(decade)%>%
  summarize(num_obs = n())
best_decade <- c("1970")

## 4a
season_stats_and_players <- left_join(season_stats, players, by = "player")

## 4b
season_stats_and_players <- season_stats_and_players %>%
  mutate(hall_of_fame =
           ifelse(str_detect(player, "\\*"),1,0
           ))

## 4c
hall_of_fame <- season_stats_and_players %>%
  select(player, tm, age, year, hall_of_fame, college)%>%
  filter(hall_of_fame == "1")

## 4d
distinct_HOFplayers <- hall_of_fame %>%
  distinct(player, .keep_all = TRUE)

## 4e
top_univ <- distinct_HOFplayers %>%
  group_by(college) %>%
  summarize(num_obs = n())

top_university = "University of California, Los Angeles"
  
  
  
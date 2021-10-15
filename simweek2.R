PERMID <- 8045494
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)

setwd("/Users/elisenguyen/Desktop/econ\ 45")
ibt_data <- read_csv("ibt_testdata.csv")
ibtclean <- ibt_data[-c(1,2),]


## exercise 1 
natnew <- ibtclean
ibtclean$nationality <- replace_na(tolower(ibtclean$nationality), 0)
ibtclean$race_ethnicity <- replace_na(tolower(ibtclean$race_ethnicity), 0)
ibtclean$race_ethnicity <- str_replace(ibtclean$race_ethnicity, "caucasian","white")
us_nationality <- "us|america|united"
china_nationality <- "china|chinese|taiwan"
asia_ethnicity <- "indian|pacific islander|asian|korean|nepalese|filipino|chinese|biracial|asian/caucasian"

## assign nationality names

ibtclean <- ibtclean%>%
  mutate(nationalities = 
           ifelse(str_detect(nationality, china_nationality) & str_detect(nationality, us_nationality, negate=TRUE),
                  1,
                  ifelse(str_detect(nationality, us_nationality, negate = TRUE) & str_detect(nationality, china_nationality, negate = TRUE),
                         4,
                         ifelse(str_detect(race_ethnicity, asia_ethnicity)
                                , 3, 2)
                  )
           )
  )

## create natnew and transpose, name columns
natnew <- ibtclean%>%
  count(nationalities)
natnew <- as_tibble(as.matrix(t(natnew[,-1])))
colnames(natnew) <- c("grchina", "usnonasian", "usasian", "other")

## writeup
writeup <- ibtclean%>%
  select(nationality, Q65_Page.Submit, Q66_Page.Submit)
writeup$nationality <- gsub("0|white|chinese/american|chinese americna| asian", "", writeup$nationality)
##removing NAs, replacing "caucasian" with "white"
writeup <- writeup%>%
  na.omit(writeup)
writeup$nationality <- str_replace(writeup$nationality, "caucasian","white")
##sorting nationalities while removing invalid responses
writeup <- writeup %>%
  mutate(Nationality =
           ifelse(str_detect(nationality, "(us)|(america)|(united)") & str_detect(nationality, "(chinese)", negate=TRUE), "US Citizen",
                  ifelse(nationality=="0" | nationality=="white" | nationality=="chinese/american" | nationality=="chinese american"| nationality=="asian" , 0, "Not US Citizen")
           )
  )
##merging Q65 and Q66, renaming cells
writeup <- writeup %>%
  pivot_longer(cols = c(Q65_Page.Submit, Q66_Page.Submit), names_to = "Test Difficulty", values_to = "Number of Seconds to Complete" )
writeup$`Test Difficulty` <- gsub("Q65_Page.Submit", "Hard", writeup$`Test Difficulty`)
writeup$`Test Difficulty` <- gsub("Q66_Page.Submit", "Easy", writeup$`Test Difficulty`)
writeup$`Number of Seconds to Complete` <- as.numeric(as.character(writeup$`Number of Seconds to Complete`))
writeup<-writeup[!(writeup$Nationality=="0"),]

## creating boxplot
ggplot(data = writeup)+
  geom_boxplot(aes(x=as.character(`Test Difficulty`), y=`Number of Seconds to Complete`, fill = Nationality))+
  xlab("Test Difficulty")+
  ylim(0,400)

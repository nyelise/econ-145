PERMID <- 8045494
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)

setwd("/Users/elisenguyen/Desktop/econ\ 45")
ibt_data <- read_csv("ibt_testdata.csv")
## exercise 1 
ibtclean <- ibt_data[-c(1,2),]

## exercise 2
##change nationality to lowercase and replace NAs with 0
ibtclean$nationality <- replace_na(tolower(ibtclean$nationality), 0)

## assign nationality names
ibtclean <- ibtclean %>%
  mutate(nationalities =
           ifelse(str_detect(nationality, "(us)|(america)|(united)") & str_detect(nationality, "(chinese)", negate=TRUE), 1,
             ifelse(nationality=="0" | nationality=="white" | nationality=="chinese/american" | nationality=="chinese american"| nationality=="asian" , 3, 2)
           )
         )
## create natgroups and transpose, name columns
natgroups <- ibtclean%>%
  count(nationalities)
natgroups <- as_tibble(as.matrix(t(natgroups[,-1])))
colnames(natgroups) <- c("US", "NotUS", "Other")

##exercise 3
means <- ibtclean %>%
  select(Q65_Page.Submit, Q66_Page.Submit)%>%
  mutate_if(is.character,as.numeric)%>%
  summarize(flowers = mean(Q66_Page.Submit, na.rm = TRUE), insects = mean(Q65_Page.Submit, na.rm = TRUE))

barplot <- data.frame(
  Image=c("Flowers", "Insects") ,  
  value=c(87.06774, 108.1427)
)
ggplot(barplot, aes(x=Image, y=value, fill=Image)) + 
  geom_bar(stat = "identity")+ 
  xlab("Image with Pleasant Words") + 
  ylab("Average Time") + 
  labs(title = "Average Time of Submissions")+
  theme_light()


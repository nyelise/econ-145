PERMID <- 8045494
library(tidyverse)
library(readr)
library(janitor)
## question 1
setwd("/Users/elisenguyen/Desktop/econ\ 45")
climate_change <- read_csv("hw_5_update.csv")
climate_change <- subset(climate_change,cause_original != -1)
## 1a
ggplot(data = climate_change)+
  geom_point(aes(x = age, y = minincome), size = 1.5, alpha = 0.05)+
  xlab("Age")+
  ylab("Income")+
  labs(title = "Relationship Between Income and Age")+
  theme_light()
## 1b
ggplot(data = climate_change)+
  geom_point(data = climate_change %>% filter(gender == "1"), aes(x = age,y=minincome), colour="red", shape = 1, alpha = 0.2)+
  geom_point(data = climate_change %>% filter(gender == "2"), aes(x = age, y=minincome), colour="blue", shape = 2,alpha = 0.2)+
  scale_shape_identity()+
  scale_colour_discrete(name="Sex", labels=c("male", "female"))+
  theme_light()
## 2a
ggplot(data = climate_change)+
  geom_histogram(aes(x=cause_original))+
  xlab("Opinion of Original Cause") + 
  ylab("Count") + 
  labs(title = "Count of Opinions of the Original Cause") +
  scale_x_continuous(labels = c("Humans", "Natural Changes", "Other", "Nonexistent"))+
  theme_light() 

## 2b 
cause_opinion<- climate_change %>% 
  group_by(cause_original) %>% 
  summarise(num_obs=n())

climate_change$cause_original <- gsub("1", "Humans", climate_change$cause_original)
climate_change$cause_original <- gsub("2", "Natural Changes", climate_change$cause_original)
climate_change$cause_original <- gsub("3", "Other", climate_change$cause_original)
climate_change$cause_original <- gsub("4", "Nonexistent", climate_change$cause_original)


ggplot(data = climate_change) +
  geom_bar(aes(x = factor(1), fill=as.factor(cause_original)))+
  xlab("Opinion of Original Cause") + 
  ylab("Count") + 
  labs(title = "Count of Opinions of the Original Cause") +
  coord_polar("y", start = 0)
  

## 3a ggplot2
ggplot(data=climate_change)+
  geom_boxplot(aes(x = as.factor(educ_category), y = minincome))+
  xlab("Education Category")+
  ylab("Income Level")+
  labs(title = "Education and Income Relationship:ggplot2")+
  scale_x_discrete(labels = c("Less than high school","High school", "Some college", "Bachelor's degree or higher"))+
  theme_light()

##3a R basic plot
boxplot(minincome~educ_category, 
        data = climate_change, 
        xlab = "Education Category", 
        ylab = "Income Level",  
        names = c("Less than high school","High school", "Some college", "Bachelor's degree+"),
        main= "Education and Income Relationship: R Basic Plots")+
  theme_light()

  
  

##3b
ggplot(data = climate_change)+
  geom_density(data = climate_change %>%
               filter(educ_category == "2"),
               aes(x=minincome),
               linetype="dashed",
               col="red")+
  geom_density(data = climate_change %>%
               filter(educ_category == "4"),
               aes(x=minincome),
               colour="blue")+
  geom_vline(xintercept = 40000,
             col = "red",
             linetype = "dashed",
             lwd = 0.3)+
  geom_vline(xintercept = 85000,
           col = "blue",
           lwd = 0.3)+
  labs(title = "Income Density & Median by Education")+
  xlab("Yearly Labor Income")+
  ylab("Density")+
  scale_color_discrete(name = "Legend", 
                       labels=c("High School", "College"))+
  labs(x = "Year",
       y = "(%)",
       color = "Legend") +
  theme_light()
library(tidyverse)
library(janitor)
library(ggplot2)
setwd("/Users/elisenguyen/Desktop/econ\ 45")
covid_survey <- read_csv("covid_survey.csv")
covid_survey <- covid_survey %>%
  filter(is.na(Q9))%>%
  filter(!is.na(`Duration (in seconds)`))
## gender demographics
ggplot(data = covid_survey)+
  geom_bar(aes(x = Q41, fill = Q41))+
  xlab("Gender")+
  ylab("Count")+
  labs(title = "Gender")+
  theme_light()

## race demographics
covid_survey[covid_survey == "American Indian/Alaskan Native,White"] <- "American Indian/Alaskan Native"
covid_survey[covid_survey == "Asian,Prefer not to say"] <- "Asian"
covid_survey[covid_survey == "Asian,White"] <- "Asian"
covid_survey[covid_survey == "Black/African American,Hispanic/Latino"] <- "Black/African American"
covid_survey[covid_survey == "Black/African American,White"] <- "Black/African American"
covid_survey[covid_survey == "Hispanic/Latino,White"] <- "Hispanic/Latino"

ggplot(data = covid_survey)+
  geom_bar(aes(x = Q2, fill = Q2))+
  xlab("Race")+
  ylab("Count")+
  labs(title = "Race")+
  theme_light()
covid_survey[covid_survey == "Work from home"| 
               covid_survey == "Remote Education,Work from home"|
               covid_survey == "Remote Education"|   
               covid_survey == "Remote Education,Work from home,Other"| 
               covid_survey == "Remote Education,Recently paused/terminated education to COVID-19 related reasons"|
               covid_survey == "Recently paused/terminated education to COVID-19 related reasons,Work from home"|
               covid_survey == "Work from home,Recently unemployed due to COVID-19"|  
               covid_survey == "Remote Education,Recently unemployed due to COVID-19"|
               covid_survey == "Remote Education,Recently paused/terminated education to COVID-19 related reasons,Work from home"|
               covid_survey == "Remote Education,Unemployed before COVID-19"|
               covid_survey == "Remote Education,Other"|
               covid_survey == "Remote Education,Unemployed before COVID-19,Other"] <-"Remote Education / Remote Work"


covid_survey[covid_survey == "Work from home,Non-remote working"|
               covid_survey =="Non-remote working"|
               covid_survey == "Remote Education,Non-remote working"|
               covid_survey =="In-person Education"|
               covid_survey == "In-person Education,Unemployed before COVID-19"|
               covid_survey =="Remote Education,In-person Education,Recently paused/terminated education to COVID-19 related reasons,Work from home"|
               covid_survey =="Remote Education,In-person Education"|
               covid_survey =="In-person Education,Work from home,Prefer not to say"|
               covid_survey =="In-person Education,Recently paused/terminated education to COVID-19 related reasons,Non-remote working"] <- "In Person Education / In Person Work"

covid_survey[covid_survey =="Other"|
               covid_survey == "Recently paused/terminated education to COVID-19 related reasons"|
               covid_survey == "Unemployed before COVID-19"|
               covid_survey == "Prefer not to say"|
               covid_survey == "Unemployed before COVID-19,Recently unemployed due to COVID-19"|
               covid_survey == "Recently unemployed due to COVID-19"] <- "Other / Unemployed"
covid_survey%>%
  count(Q44)

## education/employment status and infection rate

educ_employment <- tibble(covid_survey$Q42, covid_survey$Q44)
educ_employment <- educ_employment %>%
  count(`covid_survey$Q42`, `covid_survey$Q44`)
educ_employment <- educ_employment %>%
  rename(
    `Education / Employment Status` = `covid_survey$Q42`,
    `Infection Status` = `covid_survey$Q44`
  )

ggplot(educ_employment, aes(fill=`Education / Employment Status`, y=n, x=`Infection Status`)) + 
  geom_bar(position="dodge", stat="identity")+
  ylab("Count")



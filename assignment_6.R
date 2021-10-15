PERMID <- 8045494
library(tidyverse)
library(readr)
library(janitor)
setwd("/Users/elisenguyen/Desktop/econ\ 45")
airbnb <- read_csv("airbnb_small.csv")

## 1
set.seed(8045494)
practice_vector <- runif(10, min = 0, max = 25)
one_hundred <- seq(1:100)

##2a
for (i in practice_vector[1:5]){
  print(i)
}

##2b
squared_numbers <- rep(NA,10)

##2c
squared_numbers <- vector(mode = "numeric")
for(i in practice_vector){
  result <- c(i^2)
  squared_numbers <- append(squared_numbers, result)
}

##2d
even_numbers <- c()
odd_numbers <- c()

##2e
even_numbers<-even_numbers[!is.na(even_numbers)]
odd_numbers<-odd_numbers[!is.na(odd_numbers)]

for (i in one_hundred) {
  if(i %% 2 == 0) even_numbers[i]=i}
print(even_numbers)

for (i in one_hundred) {
  if(i %% 2 == 1) odd_numbers[i]=i}
print(odd_numbers)

##3a
maximum <- function(practice_vector){
  for(i in 1:1)
    return(max(practice_vector))
}

##3b
hist_gg <- function(airbnb,column,xlabel,title){
    ggplot(data = airbnb)+
    geom_histogram(aes(x=column)) + 
    xlab(xlabel) +
    ggtitle(title)
}

##4a
top10 <- airbnb%>%
  count(neighbourhood, sort = T)%>%
  filter(!is.na(neighbourhood))%>%
  top_n(10)
  
##4b
top10_reordered <- airbnb %>%
  count(neighbourhood)%>%
  filter(!is.na(neighbourhood))%>%
  top_n(10)%>%
  mutate(neighbourhood_reordered = fct_reorder(neighbourhood, n,))

##4c
ggplot(data = top10_reordered) +
  geom_col(aes(x=neighbourhood_reordered, y=n)) +
  coord_flip() +
  ggtitle("Most Frequently Listed Neighbourhoods in Seattle for Airbnb")
  

##4d
stacked_gg <- function(tibble,col,n,title){
  tibble%>%
    ggplot() +
    geom_col(aes(x = column, y =n)) +
    xlab("") +
    ylab("") +
    ggtitle("Most Frequently Listed Neighbourhoods in Seattle for Airbnb")
}

##5
my_distinct <- function(x){
  vector_x <- c()

  for(i in 1:length(x)){
    if(is.double(x[i]) | is.integer (x[i])) {
      vector_x <- append (vector_x, x[i])
    } else {
      return("Type Error! Try again.")
    }
  }
  
    if(is.null(vector_x)) {rm(vector_x)
      } else{
      return(vector_x <- unique(vector_x))
      }
  }
my_distinct(airbnb$price)
  

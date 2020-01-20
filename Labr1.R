#Lab 1

#Load appropriate libraries
library(ggplot2)
library(tidyverse)

#Introducing the csv file to R
myData <- read.csv(file = "bank.csv", header = TRUE, sep = ",")

#Filtering Data to consumers that HAVE subscribed to product
yesSubscribe <- myData %>%
  filter(y == "yes")
#Filtering data for consumers that have NOT subscribed to product
noSubscribe <- myData %>%
  filter(y == "no")

##Relationship between Age and Subscription to Product##
plotOne <- yesSubscribe %>% ggplot(aes(x = age)) +
  geom_bar()

#Filtering data for those WITH housing loans
yesHousing <- myData %>%
  filter(housing == "yes")
#Filtering data for those WITHOUT housing loans
noHousing <- myData %>%
  filter(housing == "no")

#Filtering data for those WITH bank loans
yesBank <- myData %>%
  filter(loan == "yes")
#Filtering data for those WITHOUT bank loans
noBank <- myData %>%
  filter(loan == "no")

#Relationship between buying a housing loan and ubscribing 
plotTwo <- yesHousing %>% ggplot(aes(x = y)) + geom_bar()
#Relationship between buying a bank loan and subscribing
plotThree <- yesBank %>% ggplot(aes(x = y)) + geom_bar()





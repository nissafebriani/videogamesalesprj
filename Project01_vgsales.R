#Setting Working Directory
setwd("C:/Users/Dell/Downloads/WDR")
getwd


#Preparing the Library
library(dplyr)
library(tidyverse)
library(ggplot2)
library(mice)

#Reading the Data
vgsales <- read.csv("vgsales.csv")

#Checking the Data
head(vgsales)
glimpse(vgsales)


#Checking and Deleting Null Value
md.pattern(vgsales)
vgsales <- na.omit(vgsales)
vgsales <- vgsales %>%
  filter(!str_detect(Year, 'N/A'))

#Checking the distribution of the Data
vgsales %>% ggplot(aes(Year)) + geom_bar()


#Checking the unique Value of Genre, Platform, and Publisher Column
genrename <- unique(vgsales$Genre)
genrename

platform <- unique(vgsales$Platform)
platform

publisher <- unique(vgsales$Publisher)
publisher

#Deleting data from 2020
vgsales <-vgsales %>%
  filter(Year != 2020)

#Looking for the best Publisher of all time
vgsales %>%
  group_by(Publisher) %>%
  summarise(frequency =
              sum(Global_Sales, na.rm = TRUE))  %>%
  arrange(desc(frequency)) 


#Looking for total sales from 1980 to 2017 
sum(vgsales$NA_Sales)
sum(vgsales$EU_Sales)
sum(vgsales$JP_Sales)

#Looking for best game in rescent year
merge(aggregate(Global_Sales ~ Year, data=vgsales, max), vgsales, all.x=T)


#Looking for the best Platform of all time
vgsales %>%
  group_by(Platform) %>%
  summarise(frequency = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(frequency))


#Looking for best game of the decade
vgsales_group <- vgsales%>%
  select(Name, Year, Global_Sales) %>%
  mutate(decade = case_when(Year >= 1980  & Year <= 1989 ~ '1',
                            Year >= 1990  & Year <= 1999 ~ '2',
                            Year >= 2000  & Year <= 2009 ~ '3',
                            Year >= 2010  & Year <= 2020 ~ '4')) %>%
  group_by(decade) 
  

merge(aggregate(Global_Sales ~ decade, data=vgsales_group, max), vgsales_group, all.x=T)                   



#Looking for most favorite genre of all time

vgsales_group2 <- vgsales%>%
  select(Genre, Year, Global_Sales) %>%
  filter(Year >= 2010 & Year <= 2017)%>%
  group_by(Genre) %>%
  summarise(frequency = sum(Global_Sales, na.rm = TRUE)) 

vgsales_group2


#Most favorite Action Game of all time
vgsales%>%
  select(Name, Genre, Year, Global_Sales, Platform) %>%
  filter(Genre == 'Action')%>%
  group_by(Name) %>%
  arrange(desc(Global_Sales))



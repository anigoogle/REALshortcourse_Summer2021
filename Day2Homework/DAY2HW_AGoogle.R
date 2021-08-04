title: "Day 2 Homework"
author: "Angela Google"
date: "`r Sys.Date()`"
output:
  html_document:
  theme: spacelab
toc: no

knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(janitor)
library(here)
library(naniar)

setwd("~/GitHub/REALshortcourse_Summer2021/Day2Homework")

colleges <- readr::read_csv("ca_college_data.csv")
glimpse(colleges)
str(colleges) #structure of data#
summary(colleges) #summary of data set#


####2. Which cities in California have the highest number of colleges? Los Angeles-24, and San Diego-18###
table(colleges$CITY)

colleges %>% 
  count(CITY)

####3. Based on your answer to #2, make a plot that shows the number of colleges in the top 10 cities.###
names(colleges)
colleges %>% 
  count(CITY,INSTNM, sort = F )

colleges %>% 
  count(CITY)%>%
  top_n(10, n) %>%
  ggplot(aes(x=CITY, y=n))+
  geom_col()

colleges %>% 
  count(CITY) %>% 
  arrange(desc(n))

colleges %>% 
  count(CITY)%>%
  top_n(10, n) %>%
ggplot(aes(x=reorder(CITY,n), y=n))+
  geom_col()



  
###4. The column `COSTT4_A` is the annual cost of each institution. Which city has the highest average cost?###

colleges %>% 
  group_by(CITY) %>% 
  summarise(ave_COSTT4_A=mean(COSTT4_A)) %>% 
  ggplot(aes(x=CITY, y=ave_COSTT4_A)) +
  geom_col()

###5. Based on your answer to #4, make a plot that compares the cost of the individual colleges in the most expensive city.###




###6. The column `ADM_RATE` is the admissions rate by college and `C150_4_POOLED` is the four-year completion rate. 
###Use a scatterplot to show the relationship between these two variables. What do you think this means?

###7. Is there a relationship between cost and four-year completion rate? (You don't need to do the stats, just produce a plot). What do you think this means?

###8. If you wanted to get a degree in biological or biomedical sciences, which campus confers the majority of these degrees? Produce a numerical summary and an appropriate plot.
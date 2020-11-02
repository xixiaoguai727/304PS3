#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set 
# Author: Jie Huang(1004925156) Qing Li (1005148010) Mengyuan Wang(1005239341) Xi Zheng(1005153628)
# Data: 02 November 2020
# Contact: xi.zheng@mail.utoronto.ca


#### Workspace setup ####
library(haven)
library(tidyverse)
library(dplyr)


# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625.dta")

# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% select(vote_2020, gender, race_ethnicity, household_income, employment, education, state, age)
# In order to see a significant proportion difference between votes for Trump and Biden, 
#we clean all observations chosen other than Trump and Biden from the survey data and 
#assume people vote only for Trump or Biden
reduced_data <- reduced_data[(reduced_data$vote_2020 == 1|reduced_data$vote_2020 ==2),]
 
# create binary columns to fit the model
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020==1, 1, 0))
reduced_data<-
  reduced_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020==2, 1, 0))
# change resopondent's answer from code number in to descriptions
seta <- c(4,6,8,9,10,11,12,13,14)
reduced_data <- 
  reduced_data %>%
  mutate(race = case_when(race_ethnicity == 1 ~ "white", 
                          race_ethnicity == 2 ~ "black",
                          race_ethnicity == 3 ~ "American Indian or Alaska Native",
                          race_ethnicity == 5 ~ "Chinese",
                          race_ethnicity == 7 ~ "Japanese",
                          race_ethnicity %in% seta ~"ther Asian or pacific Islander",
                          race_ethnicity == 15 ~ "Other race"))
reduced_data <- 
  reduced_data %>%
  mutate(sex = case_when(gender == 1 ~"male",
                         gender == 2 ~"female"))
set1 <- c(5,6,7)
set2<- c(9,10,11)
reduced_data <- 
  reduced_data %>%
  mutate(edu = case_when(education == 1 ~"less than grade 4",
                         education == 2 ~"middle school(grade 4-8)",
                         education == 3 ~"completed some high school(grade 9 -11)",
                         education == 4 ~"high school graduate",
                         education %in% set1 ~"education above high school(less than 3 year)",
                         education == 8 ~ "college degree (education above  high school 4 year)",
                         education %in% set2 ~"higher than college degree"))

setb <- c(2,3,4,6,7,9)
setc <-c(1,5)
reduced_data <- 
  reduced_data %>%
  mutate(class = case_when(employment %in% setc ~"works for wages",
                           employment == 8 ~"self-employed",
                           employment %in% setb ~"not available(student, retire…)"))




# Saving the survey/sample data as a csv file in my
# working directory
#write_csv(reduced_data, "survey_data.csv")



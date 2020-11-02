#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
raw_data <- read_dta("usa_00002.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)

reduced_data <- 
  raw_data %>% 
  select(age, race, educ, citizen,perwt,stateicp,classwkr,sex)#, 
         #race, 
         #hispan,
         #marst, 
         #bpl,
         #citizen,
         #educd,
         #labforce,
         #labforce)
         
reduced_data <- reduced_data[(reduced_data$citizen == 2),]
reduced_data <- reduced_data[(18<=reduced_data$age),]
#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)
set1 <- c(0,1)
set2 <- c(3,4,5)
set3 <- c(7,8,9)
reduced_data <- 
  reduced_data %>%
  mutate(edu = case_when(educ %in% set1 ~"less than grade 4",
                         educ == 2 ~"middle school(grade 4-8)",
                         educ %in% set2 ~"completed some high school(grade 9 -11)",
                         educ == 6 ~ "high school graduate",
                         educ %in% set3 ~"education above high school(less than 3 year)",
                         educ == 10 ~ "college degree (education above  high school 4 year)",
                         educ == 11 ~"higher than college degree"))
reduced_data <- 
  reduced_data %>%
  mutate(class = case_when(classwkr == 0 ~"not available(student, retire…)",
                           classwkr == 1 ~"self-employed",
                           classwkr == 2 ~"works for wages"))
set4 <-c(7,8,9)
reduced_data <- 
  reduced_data %>%
  mutate(race1 = case_when(race == 1 ~"white",
                           race == 2 ~"black",
                           race == 3 ~"American Indian or Alaska Native",
                           race == 4 ~"Chinese",
                           race == 5 ~"Japanese",
                           race == 6~"ther Asian or pacific Islander",
                           race %in% set4~"Other race"))
reduced_data <- 
  reduced_data %>%
  mutate(state = case_when(stateicp == 1 ~ "CT",
                           stateicp == 2 ~ "ME",
                           stateicp == 3 ~ "MA",
                           stateicp == 4 ~ "NH",
                           stateicp == 5 ~ "RI",
                           stateicp == 6 ~ "VT",
                           stateicp == 11 ~ "DE",
                           stateicp == 12 ~ "NJ",
                           stateicp == 13 ~ "NY",
                           stateicp == 14 ~ "PA",
                           stateicp == 21 ~ "IL",
                           stateicp == 22 ~ "IN",
                           stateicp == 23 ~ "MI",
                           stateicp == 24 ~ "OH",
                           stateicp == 25 ~ "WI",
                           stateicp == 31 ~ "IA",
                           stateicp == 32 ~ "KS",
                           stateicp == 33 ~ "MN",
                           stateicp == 34 ~ "MO",
                           stateicp == 35 ~ "NE",
                           stateicp == 36 ~ "ND",
                           stateicp == 37 ~ "SD",
                           stateicp == 40 ~ "VA",
                           stateicp == 41 ~ "AL",
                           stateicp == 42 ~ "AK",
                           stateicp == 43 ~ "FL",
                           stateicp == 44 ~ "GA",
                           stateicp == 45 ~ "LA",
                           stateicp == 46 ~ "MS",
                           stateicp == 47 ~ "NC",
                           stateicp == 48 ~ "SC",
                           stateicp == 49 ~ "TX",
                           stateicp == 51 ~ "KY",
                           stateicp == 52 ~ "MD",
                           stateicp == 53 ~ "OK",
                           stateicp == 54 ~ "TN",
                           stateicp == 56~ "WV",
                           stateicp == 61 ~ "AZ",
                           stateicp == 62 ~ "CO",
                           stateicp == 63 ~ "IA",
                           stateicp == 64 ~ "MT",
                           stateicp == 65 ~ "NV",
                           stateicp == 66 ~ "NM",
                           stateicp == 67 ~ "UT",
                           stateicp == 68 ~ "WY",
                           stateicp == 71 ~ "CA",
                           stateicp == 72 ~ "OR",
                           stateicp == 73 ~ "WA",
                           stateicp == 81~ "AK",
                           stateicp == 82 ~ "HI",
                           stateicp == 83 ~ "PR",
                           stateicp == 98 ~ "DC"))
reduced_data <- 
  reduced_data %>%
  mutate(sex = case_when(sex == 1 ~"male",
                         sex == 2 ~"female"))

# Saving the census data as a csv file in my
# working directory

write_csv(reduced_data, "census_data.csv")



         
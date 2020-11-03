#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from American Community Surveys 
# Author: Boyang Li
# Data: 30 October 2020
# Contact: leoboyang.li@mail.utoronto,ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/leoli 1/Desktop/PS3")
raw_ps_data <- read_dta("/Users/leoli 1/Desktop/PS3/usa_00001.dta")

# Add the labels
raw_ps_data <- labelled::to_factor(raw_ps_data)

# Just keep some variables that may be of interest 
# (intrested in region, state, gender, age, race, education, employment state, and family income) 
reduced_ps_data <- 
  raw_ps_data %>% 
  select(region,
         stateicp,
         sex, 
         age, 
         race, 
         educd,
         empstat, 
         ftotinc)

#### Set variables to numeric to reduce categorical data in the final model. 

# Decide to drop region and state because they are meaningless in number. 
# Moreover, it is more useful to do a voting prediction for each state due to 
# the voting system in US. But 50 states are way too many for this assignment, 
# so I decide to drop them and mention it again in the discussion session. 

# First find the number of categories each variables contains, then convert them. 

# gender
reduced_ps_data %>%
  select(sex) %>%
  pull() %>%
  table() 

reduced_ps_data<-
  reduced_ps_data %>%
  mutate(male = ifelse(sex=="male", 1, 0))

# age
# Here I arrange age into age groups. I ignore people who are below 15 at 2018 
# because they won't meet age requirement by 2020. Each group has a range of 
# 10 years. There are 8 groups in total.
reduced_ps_data %>%
  select(age) %>%
  pull() %>%
  table() 

reduced_ps_data$age <- as.integer(reduced_ps_data$age)

reduced_ps_data <- 
  reduced_ps_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age >= 15)

reduced_ps_data<-
  reduced_ps_data %>%
  mutate(age_group = ifelse(age>=15 & age<25, 1, 
                     ifelse(age>=25 & age<35, 2, 
                     ifelse(age>=35 & age<45, 3, 
                     ifelse(age>=45 & age<55, 4, 
                     ifelse(age>=55 & age<65, 5, 
                     ifelse(age>=65 & age<75, 6,
                     ifelse(age>=75 & age<85, 7,
                     ifelse(age>=85 & age=="90 (90+ in 1980 and 1990)", 8, 0)))))))))
# race
# Here I only take five variables: white, african american, ameican indian, asian, and other
reduced_ps_data %>%
  select(race) %>%
  pull() %>%
  table() 

reduced_ps_data<-
  reduced_ps_data %>%
  mutate(white = ifelse(race=="white", 1, 0)) %>%
  mutate(african_american = ifelse(race=="black/african american/negro", 1, 0)) %>%
  mutate(american_indian = ifelse(race=="american indian or alaska native", 1, 0)) %>%
  mutate(asian = ifelse(race=="chinese"|
                          race=="japanese"|
                          race=="other asian or pacific islander", 1, 0)) %>%
  mutate(other = ifelse(race=="other race, nec"|
                          race=="two major races"|
                          race=="three or more major races", 1, 0))

# education
# Here I scale the level of education. 1 indicates none to grade 3; 2 indicates 
# grade 4 to grade 8; 3 indicates some high school education; 
# 4 indicates high school graduate; 5 indicates some post-secondary education; 
# 6 indicates bachelor's degree or associate's degree; 7 indicates masters 
# degree; 8 indicates doctoral degree
reduced_ps_data %>%
  select(educd) %>%
  pull() %>%
  table() 

reduced_ps_data<-
  reduced_ps_data %>%
  mutate(degree = ifelse(educd=="n/a"|
                           educd=="no schooling completed"|
                           educd=="nursery school, preschool"|
                           educd=="kindergarten"|
                           educd=="grade 1"|
                           educd=="grade 2"|
                           educd=="grade 3", 1, 
                  ifelse(educd=="grade 4"|
                           educd=="grade 5"|
                           educd=="grade 6"|
                           educd=="grade 7"|
                           educd=="grade 8", 2, 
                  ifelse(educd=="grade 9"|
                           educd=="grade 10"|
                           educd=="grade 11", 3,
                  ifelse(educd=="12th grade, no diploma"|
                           educd=="regular high school diploma"|
                           educd=="ged or alternative credential", 4, 
                  ifelse(educd=="some college, but less than 1 year"|
                           educd=="1 or more years of college credit, no degree", 5, 
                  ifelse(educd=="associate's degree, type not specified"|
                           educd=="bachelor's degree", 6, 
                  ifelse(educd=="master's degree"|
                           educd=="professional degree beyond a bachelor's degree", 7, 
                  ifelse(educd=="doctoral degree", 8, 0)))))))))

# employment
# Here I separate employment and unemployed into three category because many 
# participants are not in labor force
reduced_ps_data %>%
  select(empstat) %>%
  pull() %>%
  table() 

reduced_ps_data<-
  reduced_ps_data %>%
  mutate(employed = ifelse(empstat=="employed", 1, 0))

reduced_ps_data<-
  reduced_ps_data %>%
  mutate(unemployed = ifelse(empstat=="unemployed", 1, 0))

reduced_ps_data<-
  reduced_ps_data %>%
  mutate(not_in_labor_force = ifelse(empstat=="not in labor force", 1, 0))

# income
# Here I scale the level of income. 1 indicates lowever than 14,999; 2 indicates
# 15,000 to 29,999; 3 indicates 30,000 to 49,999; 4 indicates 50,000 to 69,999; 
# 5 indicates 70,000 to 89,999; 6 indicates 90,000 to 149,000; 7 indicates 
# 150,000 to 249,999; 8 indicates 250,000 and above
reduced_ps_data %>%
  select(ftotinc) %>%
  pull() %>%
  table() %>%
  barplot()

reduced_ps_data$ftotinc <- as.integer(reduced_ps_data$ftotinc)

reduced_ps_data<-
  reduced_ps_data %>%
  mutate(income_level = ifelse(ftotinc<15000, 1, 
                        ifelse(ftotinc>=15000 & ftotinc<30000, 2, 
                        ifelse(ftotinc>=30000 & ftotinc<50000, 3, 
                        ifelse(ftotinc>=50000 & ftotinc<70000, 4, 
                        ifelse(ftotinc>=70000 & ftotinc<90000, 5, 
                        ifelse(ftotinc>=90000 & ftotinc<1500000, 6,
                        ifelse(ftotinc>=1500000 & ftotinc<2500000, 7,
                        ifelse(ftotinc>=2500000, 8, 0)))))))))



# Saving the census data as a csv file in my
# working directory
write_csv(reduced_ps_data, "/Users/leoli 1/Desktop/PS3/census_data.csv")



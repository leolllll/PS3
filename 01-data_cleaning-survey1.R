#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from  Democracy Fund + UCLA Nationscape
# Author: Boyang Li
# Data: 30 October 2020
# Contact: leoboyang.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/leoli 1/Desktop/PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_survey_data <- read_dta("/Users/leoli 1/Desktop/PS3/ns20200625/ns20200625.dta")
# Add the labels
raw_survey_data <- labelled::to_factor(raw_survey_data)
# Just keep some variables
reduced_survey_data <- 
  raw_survey_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         census_region, 
         state, 
         gender, 
         age, 
         race_ethnicity, 
         education, 
         employment, 
         household_income)

#### Set variables to numeric to reduce categorical data in the final model. 

# Here I only choose vote_2020 and variables that are in the post stratification data. 

# First find the number of categories each variables contains, then convert them. 

# vote for 2020, I take votes for 2 candidates seperatly. 
reduced_survey_data<-
  reduced_survey_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))

# gender 
reduced_survey_data %>%
  select(gender) %>%
  pull() %>%
  table() 

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(male = ifelse(gender=="Male", 1, 0))

# age 
# Here I arrange age into age groups. The first groups contains participants of 
# age below 25, other groups have a range of 10 years. There are 8 groups in total. 
reduced_survey_data %>%
  select(age) %>%
  pull() %>%
  table() 

reduced_survey_data$age <- as.integer(reduced_survey_data$age)

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(age_group = ifelse(age<25, 1, 
                     ifelse(age>=25 & age<35, 2, 
                     ifelse(age>=35 & age<45, 3, 
                     ifelse(age>=45 & age<55, 4, 
                     ifelse(age>=55 & age<65, 5, 
                     ifelse(age>=65 & age<75, 6,
                     ifelse(age>=75 & age<85, 7,
                     ifelse(age>=85, 8, 0)))))))))

# race
# Here I only take five variables: white, african american, ameican indian, asian, and other.
reduced_survey_data %>%
  select(race_ethnicity) %>%
  pull() %>%
  table() 

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(white = ifelse(race_ethnicity=="White", 1, 0)) %>%
  mutate(african_american = ifelse(race_ethnicity=="Black, or African American", 1, 0)) %>%
  mutate(american_indian = ifelse(race_ethnicity=="American Indian or Alaska Native", 1, 0)) %>%
  mutate(asian = ifelse(race_ethnicity=="Asian (Asian Indian)"|
                          race_ethnicity=="Asian (Chinese)"|
                          race_ethnicity=="Asian (Filipino)"|
                          race_ethnicity=="Asian (Japanese)"|
                          race_ethnicity=="Asian (Korean)"|
                          race_ethnicity=="Asian (Vietnamese)"|
                          race_ethnicity=="Asian (Other)", 1, 0)) %>%
  mutate(other = ifelse(race_ethnicity=="Pacific Islander (Native Hawaiian)"|
                          race_ethnicity=="Pacific Islander (Guamanian)"|
                          race_ethnicity=="Pacific Islander (Samoan)"|
                          race_ethnicity=="Pacific Islander (Other)"|
                          race_ethnicity=="Some other race", 1, 0))

# education
# Here I scale the level of education. 1 indicates none to grade 3; 2 indicates 
# grade 4 to grade 8; 3 indicates some high school education; 
# 4 indicates high school graduate; 5 indicates some post-secondary education; 
# 6 indicates bachelor's degree or associate's degree; 7 indicates masters 
# degree; 8 indicates doctoral degree
reduced_survey_data %>%
  select(education) %>%
  pull() %>%
  table() 

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(degree = ifelse(education=="3rd Grade or less", 1, 
                  ifelse(education=="Middle School - Grades 4 - 8", 2, 
                  ifelse(education=="Completed some high school", 3,
                  ifelse(education=="High school graduate", 4, 
                  ifelse(education=="Other post high school vocational training"|
                           education == "Completed some college, but no degree", 5, 
                  ifelse(education=="Associate Degree"|
                           education=="College Degree (such as B.A., B.S.)", 6, 
                  ifelse(education=="Completed some graduate, but no degree"|
                           education=="Masters degree", 7, 
                  ifelse(education=="Doctorate degree", 8, 0)))))))))

# employment 
# Here I separate employment and unemployed into three category because many 
# participants are not in labor force
reduced_survey_data %>%
  select(employment) %>%
  pull() %>%
  table() 

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(employed = ifelse(employment=="Full-time employed"|
                             employment=="Part-time employed"|
                             employment=="Self-employed", 1, 0))

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(unemployed = ifelse(employment=="Unemployed or temporarily on layoff", 1, 0))

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(not_in_labor_force = ifelse(employment=="Homemaker"|
                                       employment=="Retired"|
                                       employment=="Permanently disabled"|
                                       employment=="Student", 1, 0))


# income
# Here I scale the level of income. 1 indicates lowever than 14,999; 2 indicates
# 15,000 to 29,999; 3 indicates 30,000 to 49,999; 4 indicates 50,000 to 69,999; 
# 5 indicates 70,000 to 89,999; 6 indicates 90,000 to 149,000; 7 indicates 
# 150,000 to 249,999; 8 indicates 250,000 and above
reduced_survey_data %>%
  select(household_income) %>%
  pull() %>%
  table() 

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(income_level = ifelse(household_income=="Less than $14,999", 1, 
                        ifelse(household_income=="$15,000 to $19,999"|
                                 household_income=="$20,000 to $24,999"|
                                 household_income=="$25,000 to $29,999",  2, 
                        ifelse(household_income=="$30,000 to $34,999"|
                                 household_income=="$35,000 to $39,999"|
                                 household_income=="$40,000 to $44,999"|
                                 household_income=="$45,000 to $49,999",  3, 
                        ifelse(household_income=="$50,000 to $54,999"|
                                 household_income=="$55,000 to $59,999"|
                                 household_income=="$60,000 to $64,999"|
                                 household_income=="$65,000 to $69,999",  4,
                        ifelse(household_income=="$$70,000 to $74,999"|
                                 household_income=="$75,000 to $79,999"|
                                 household_income=="$80,000 to $84,999"|
                                 household_income=="$85,000 to $89,999",  5,
                        ifelse(household_income=="$90,000 to $94,999"|
                                 household_income=="$95,000 to $99,999"|
                                 household_income=="$100,000 to $124,999"|
                                 household_income=="$125,000 to $149,999",  6,
                        ifelse(household_income=="$150,000 to $174,999"|
                                 household_income=="$175,000 to $199,999"|
                                 household_income=="$200,000 to $249,999",  7,
                        ifelse(household_income=="$250,000 and above", 8, 0)))))))))



# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_survey_data, "/Users/leoli 1/Desktop/PS3/survey_data.csv")



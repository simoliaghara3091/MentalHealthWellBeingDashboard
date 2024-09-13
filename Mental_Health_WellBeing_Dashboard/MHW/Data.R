library(shiny)
library(readxl)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinythemes)

X2022 <- read_excel("2022.xlsx")
X2021 <- read_excel("2021.xlsx")
X2020 <- read_excel("2020.xlsx")
X2019 <- read_excel("2019.xlsx")

Data <- bind_rows(X2022, X2021, X2020, X2019)

Data <- Data %>%
  mutate(Gender = case_when(
    Gender == 1 ~ "Male", 
    Gender == 2 ~ "Female",
    Gender == 7 ~ "Refused",
    Gender == 8 ~ "Not Ascertained",
    Gender == 9 ~ "Don't Know",
    TRUE ~ as.character(Gender) # Handle other cases
  )) %>%
  filter(!(Gender %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>%
  mutate(Age = case_when(
    Age < 18 ~ "Under 18", 
    Age >= 18 & Age <= 29 ~ "18-29", 
    Age >= 30 & Age <= 39 ~ "30-39", 
    Age >= 40 & Age <= 49 ~ "40-49", 
    Age >= 50 & Age <= 64 ~ "50-64", 
    Age >= 65 ~ "65+", # Added the closing double quote here
    TRUE ~ as.character(Age)
  )) %>%
  filter(!(Age %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>% 
  mutate(Race = case_when(
    Race == 1 ~ "White only", 
    Race == 2 ~ "Black/African American only", 
    Race == 3 ~ "Asian only", 
    Race == 4 ~ "AIAN only", 
    Race == 5 ~ "AIAN and any other group",
    Race == 6 ~ "Other single and multiple races",
    Race == 7 ~ "Refused" ,  
    Race == 8 ~ "Not Ascertained" ,
    Race == 9 ~ "Don't know" ,
    TRUE ~ as.character(Race)
  )) %>%
  filter(!(Race %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>%
  mutate(Education = case_when(
    Education == 01 ~ "Grade 1-11", 
    Education == 02 ~ "12th grade, no diploma", 
    Education == 03 ~ "GED or equivalent", 
    Education == 04 ~ "High School Graduate", 
    Education == 05 ~ "Some college, no degree" ,
    Education == 06 ~ "Associate degree(technical)",
    Education == 07 ~ "Associate degree(academic)",
    Education == 08 ~ "Bachelor's degree" ,
    Education == 09 ~ "Master's degree",
    Education == 10 ~ "Professional School degree",
    Education == 11 ~ "Doctoral degree",
    Education == 97 ~ "Refused", 
    Education == 98 ~ "Not Ascertained", 
    Education == 99 ~ "Don't Know",
    TRUE ~ as.character(Education)
  )) %>%
  filter(!(Education %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>%
  mutate(Maritial_Status = case_when(
    Maritial_Status == 1 ~ "Married,spouse present", 
    Maritial_Status == 2 ~ "Married,spouse not present", 
    Maritial_Status == 3 ~ "Married, spouse presence unknown", 
    Maritial_Status == 4 ~ "Widowed", 
    Maritial_Status == 5 ~ "Divorced", 
    Maritial_Status == 6 ~ "Separated", 
    Maritial_Status == 7 ~ "Never married" ,
    Maritial_Status == 8 ~ "Living with a partner", 
    Maritial_Status == 9 ~ "Unknown marital status",
    TRUE ~ as.character(Maritial_Status)
  )) %>%
  filter(!(Maritial_Status %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>%
  mutate(Emp_Status = case_when(
    Emp_Status >= 35 & Emp_Status <= 95 ~ "Full Time",
    Emp_Status < 35 ~ "Part Time",
    Emp_Status == 97 ~ "Refused", 
    Emp_Status == 98 ~ "Not Ascertained", 
    Emp_Status == 99 ~ "Don't Know", 
    is.na(Emp_Status) ~ NA_character_,
    TRUE ~ as.character(Emp_Status)
  )) %>%
  filter(!(Emp_Status %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>%
  mutate(Anx_Freq = case_when(
    Anx_Freq == 1 ~ "Daily", 
    Anx_Freq == 2 ~ "Weekly", 
    Anx_Freq == 3 ~ "Monthly", 
    Anx_Freq == 4 ~ "Sometimes", 
    Anx_Freq == 5 ~ "Never", 
    Anx_Freq == 7 ~ "Refused", 
    Anx_Freq == 8 ~ "Not Ascertained", 
    Anx_Freq == 9 ~ "Don't Know",
    TRUE ~ as.character(Anx_Freq)
  )) %>%
  filter(!(Anx_Freq %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>%
  mutate(Anx_Level = case_when(
    Anx_Level == 1 ~ "A little", 
    Anx_Level == 2 ~ "A lot" ,  
    Anx_Level == 3 ~ "In Between",
    Anx_Level == 7 ~ "Refused", 
    Anx_Level == 8 ~ "Not Ascertained", 
    Anx_Level == 9 ~ "Don't Know",
    TRUE ~ as.character(Anx_Level)
  )) %>%
  filter(!(Anx_Level %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>%
  mutate(Anx_Meds = case_when(
    Anx_Meds == 1 ~ "Yes",
    Anx_Meds == 2 ~ "No" ,
    Anx_Meds == 7 ~ "Refused" ,
    Anx_Meds == 8 ~ "Not Ascertained" ,
    Anx_Meds == 9 ~ "Don't Know",
    TRUE ~ as.character(Anx_Meds)
  )) %>%
  filter(!(Anx_Meds %in% c("Refused" , "Not Ascertained" , "Don't Know")))

Data <- Data %>%
  mutate(Anx_Therapy = case_when(
    Anx_Therapy == 1 ~ "Yes",
    Anx_Therapy == 2 ~ "No" ,
    Anx_Therapy == 7 ~ "Refused" ,
    Anx_Therapy == 8 ~ "Not Ascertained" ,
    Anx_Therapy == 9 ~ "Don't Know",
    TRUE ~ as.character(Anx_Therapy)
  )) %>%
  filter(!(Anx_Therapy %in% c("Refused" , "Not Ascertained" , "Don't Know")))



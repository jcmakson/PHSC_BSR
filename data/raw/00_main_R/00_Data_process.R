# setting working directory 
getwd()

# loading and tidying data

## Loading libraries 

library(tidyverse)
library(lubridate)
library(unibeCols)
library(esquisse)
library(medicaldata)
library(readr)

## upload data from medicaldata package 

setwd("data/raw/")
data <- covid_testing
write.csv(data, file = "my_data.csv")

my_data <- read_csv("my_data.csv")
my_data_processed <- my_data %>% 
  mutate(gender = factor(gender, levels= c("female", "male"), labels=c("female", "male")),
         result=factor(result, levels=c("positive", "negative", "invalid")),
         clinic_name=factor(clinic_name, levels=c( )),
         test_id = factor(test_id, levels=c("covid", "xcvd1")),
                          demo_group=factor(demo_group, levels=c("patient", "misc_adult", "client", "other adult", "unidentified")),
         drive_thru_ind=factor(drive_thru_ind, levels=c(0,1), labels=c("Not collected at drive_thru-site", "collected at drive-thru site")),
         payor_group=factor(payor_group, levels=c("commercial", "governmenet", "unassigned", "medical assistance", "self pay", "charity care", "other")),
         orderset=factor(orderset, levels=c(1, 0), labels=c("Collected via orderset", "Not collected via orderset")))

write.csv(my_data_processed, file = "~/Documents/PHSC_BSR/data/processed/my_data_processed.csv")

str(my_data_processed$gender)


#******************************************************************************
## Description .......: Clean the table "course" from the UTMB DB
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-21
## License ...........: GPL
##
## Inputs ............: 'RAW_course.csv" - an csv export of UTMB DB's "course" table
## Outputs ...........: 'WRK_course.csv' 
##                    
#******************************************************************************

library(tidyverse)
library(data.table)
library(lubridate)
library(useful)
library(funModeling)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/2. Prepared Data/course") #set the working directory 
dataset <- fread('RAW_course.csv', na.strings = "", encoding = "UTF-8") #load the csv file in a storage dataframe named "dataset"
# the file contains 14648 lines

course <- dataset[,c("id", "id_evt", "type_course", "type_partic", "nb_eqp")] #Keep only the interested variable 

str(course)

# > str(course)
# 'data.frame':	14648 obs. of  5 variables:
#   $ id         : int  600 601 602 3702 4 1 4910 5138 5139 5140 ...
# $ id_evt     : int  35 1739 1495 1888 32 597 2480 2586 2586 2587 ...
# $ type_course: int  1 1 1 1 1 4 1 1 1 1 ...
# $ type_partic: chr  "solo" "solo" "solo" "solo" ...
# $ nb_eqp     : int  1 0 1 0 1 1 0 1 1 1 ...

status(course)
# > status(course)
# variable q_zeros      p_zeros q_na        p_na q_inf p_inf      type unique
# id                   id       0 0.000000e+00    0 0.000000000     0     0   integer  14648
# id_evt           id_evt       1 6.826871e-05    0 0.000000000     0     0   integer   5887
# type_course type_course      26 1.774986e-03    0 0.000000000     0     0   integer      7
# type_partic type_partic       0 0.000000e+00   41 0.002799017     0     0 character      3
# nb_eqp           nb_eqp    9796 6.687602e-01    0 0.000000000     0     0   integer     18

######################
## Variables cleaning 
######################

course %>% #check the "type_course" variable possibility 
  group_by(type_course) %>%
  summarise(n())
colnames(course)[3] <- "id_type_course"

course %>% #check the "type_partic" variable possibility 
  group_by(type_partic) %>%
  summarise(n())
# course %>%
#   filter(is.na(type_partic) == TRUE) 

course %>% #check the "nb_eqp" variable possibility
  group_by(type_partic,nb_eqp) %>%
  summarise(n()) %>%
  print(n = Inf)

######################
## Dataset new table
######################

write.csv(course, file = 'WRK_course.csv', row.names = FALSE) #Create a csv file with the clean data

#******************************************************************************
## Description .......: Clean the table "evenement" from the UTMB DB
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-20
## License ...........: GPL
##
## Inputs ............: 'RAW_evenement.csv" - an csv export of UTMB DB's "evenement" table
## Outputs ...........: 'WRK_evenement.csv' 
##                    
#******************************************************************************

library(tidyverse)
library(lubridate)
library(funModeling)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/2. Prepared Data/evenement/")
evt <- read.csv('RAW_evenement.csv', na.strings = "", encoding = "UTF-8") 


evt <- evt[,c("id", "alias", "cp", "ville", "pays", "id_subdiv")] #keep useful variables
str(evt)

# > str(evt)
# 'data.frame':	6406 obs. of  6 variables:
# $ id       : int  1 2 3 4 5 6 7 8 9 10 ...
# $ alias    : chr  NA "0211-100" NA "100ELODE" ...
# $ cp       : chr  "27324" NA "CA12 4TT" "36030" ...
# $ ville    : chr  "Folgoso do Courel" NA "Keswick" "Villaverla" ...
# $ pays     : chr  "ES" "DE" "GB" "IT" ...
# $ id_subdiv: int  1005 0 0 1463 0 0 1243 67 0 671 ...

status(evt)

######################
## Variables cleaning 
######################

evt %>%  #!! NA value stand for Namibia not for missing value
  group_by(pays) %>%
  summarise(n()) %>%
  print(n = Inf)

grepl("^[[:upper:]]+$", levels(as.factor(evt$pays))) #check if "pays" are all uppercase

######################
## Creating clean table
######################
write.csv(evt, 'WRK_evenement.csv', row.names = FALSE)

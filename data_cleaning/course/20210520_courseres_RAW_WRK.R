#******************************************************************************
## Description .......: Clean the table "courseres" from the UTMB DB
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-19
## License ...........: GPL
##
## Inputs ............: 'RAW_courseres.csv" - an csv export of UTMB DB's "courseres" table
## Outputs ...........: 'WRK_courseres.csv' 
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

setwd("~/Project _dataScience/UTMB_dataMining/2. Prepared Data/course")
dataset <- read.csv('RAW_courseres.csv', na.strings = c("NULL", "0"))
# -5305412 lines

courseRes <- dataset
str(courseRes)

# 'data.frame':	5305412 obs. of  6 variables:
#   $ id_course_annee: int  3449 3449 3449 3449 3449 3449 3449 3449 3449 3449 ...
# $ idx            : int  1 2 3 4 5 6 7 8 9 10 ...
# $ id_coureur     : int  25829 25830 25831 23509 8127 25832 23777 25833 23783 23936 ...
# $ temps          : chr  "08:50:42" "09:15:48" "09:53:36" "10:02:33" ...
# $ place          : chr  "1" "2" "3" "4" ...
# $ cote           : num  805 768 719 709 700 ...

status(courseRes)
# > status(courseRes)
#                         variable q_zeros    p_zeros   q_na       p_na q_inf p_inf      type  unique
# id_course_annee id_course_annee       0 0.00000000      0 0.00000000     0     0   integer   28380
# idx                         idx       0 0.00000000      0 0.00000000     0     0   integer    9403
# id_coureur           id_coureur       0 0.00000000      0 0.00000000     0     0   integer 1881016
# temps                     temps       0 0.00000000 178083 0.03356629     0     0 character  185613
# place                     place       0 0.00000000 178415 0.03362887     0     0   integer    6066
# cote                       cote  226100 0.04261686      0 0.00000000     0     0   numeric   70467

######################
## variables cleaning 
######################


courseRes %>%
  ggplot(aes(x = cote)) +
  geom_histogram()

courseRes %>%
  filter(is.na(place) == TRUE & is.na(temps) == FALSE)
courseRes %>%
  filter(is.na(place) == FALSE & is.na(temps) == TRUE)
courseRes %>%
  filter(cote == 0)

courseRes$temps[which(courseRes$temps == '00:00:00')] <- NA
courseRes$cote[which(courseRes$cote == 0)] <- NA


write.csv(courseRes, file = 'WRK_courseres.csv', row.names = FALSE)

#******************************************************************************
## Description .......: Clean the table "courseanneeetape" from the UTMB DB
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-19
## License ...........: GPL
##
## Inputs ............: 'RAW_courseannee.csv" - an csv export of UTMB DB's "courseanneeetape" table
## Outputs ...........: 'WRK_courseanneeetape.csv' 
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
dataset <- read.csv('RAW_courseanneeetape.csv', na.strings = c("", "NULL", "-1", "0"), encoding = "UTF-8") #load the csv file in a storage dataframe named "dataset"
# the file contains 38815 lines

courseAnneeEtape <- dataset[,c("id_course_annee", "idx", "dist", "deniv", "deniv_neg", "deniv_max_montee", 
                                                            "nb_ravito", "alt_min", "alt_max", "alt_moy", "dist_retenu",
                                                            "deniv_retenu", "deniv_neg_retenu", "ville_dep", "pays_dep", "ville_arr", "pays_arr")]
str(courseAnneeEtape)
# > str(courseAnneeEtape)
# 'data.frame':	41276 obs. of  17 variables:
#   $ id_course_annee : int  2 3 10385 5 7 9 10 11 12 14 ...
# $ idx             : int  1 1 1 1 1 1 1 1 1 1 ...
# $ dist            : num  81 250 130 105 80 160 141 66 80 81 ...
# $ deniv           : int  NA NA 5500 2400 2000 7500 5400 2900 3300 2200 ...
# $ deniv_neg       : int  NA NA 5500 NA NA 7500 NA NA NA NA ...
# $ deniv_max_montee: int  NA NA 902 NA NA NA NA NA NA NA ...
# $ nb_ravito       : int  NA NA 14 NA NA NA NA NA NA NA ...
# $ alt_min         : int  NA NA 183 NA NA NA NA NA NA NA ...
# $ alt_max         : int  NA NA 1123 NA NA NA NA NA NA NA ...
# $ alt_moy         : int  NA NA 650 NA NA NA NA NA NA NA ...
# $ dist_retenu     : num  81 70 133 105 80 ...
# $ deniv_retenu    : int  NA NA 5560 2400 2000 7500 5400 2900 3300 2200 ...
# $ deniv_neg_retenu: int  NA NA 5600 NA NA 7500 NA NA NA NA ...
# $ ville_dep       : chr  NA NA "IGUALEJA" NA ...
# $ pays_dep        : chr  "ZA" "ZA" "ES" "ZA" ...
# $ ville_arr       : chr  NA NA "IGUALEJA" NA ...
# $ pays_arr        : chr  "ZA" "ZA" "ES" "ZA" ...

status(courseAnneeEtape)
# > status(courseAnneeEtape)
# variable q_zeros     p_zeros  q_na         p_na q_inf p_inf      type unique
# id_course_annee   id_course_annee       0 0.000000000     0 0.000000e+00     0     0   integer  38860
# idx                           idx       0 0.000000000     2 4.845431e-05     0     0   integer     56
# dist                         dist      87 0.002107762     0 0.000000e+00     0     0   numeric   2913
# deniv                       deniv       0 0.000000000   369 8.939820e-03     0     0   integer   4926
# deniv_neg               deniv_neg       0 0.000000000  2373 5.749104e-02     0     0   integer   4871
# deniv_max_montee deniv_max_montee       0 0.000000000 15001 3.634315e-01     0     0   integer    820
# nb_ravito               nb_ravito       0 0.000000000 12398 3.003683e-01     0     0   integer     53
# alt_min                   alt_min       0 0.000000000 16215 3.928433e-01     0     0   integer   2407
# alt_max                   alt_max       0 0.000000000 14686 3.558000e-01     0     0   integer   3547
# alt_moy                   alt_moy       0 0.000000000 15044 3.644733e-01     0     0   integer   3146
# dist_retenu           dist_retenu    3456 0.083729044  1448 3.508092e-02     0     0   numeric   2085
# deniv_retenu         deniv_retenu       0 0.000000000  5136 1.244307e-01     0     0   integer   2436
# deniv_neg_retenu deniv_neg_retenu       0 0.000000000  6932 1.679426e-01     0     0   integer   2330
# ville_dep               ville_dep       0 0.000000000 12176 2.949898e-01     0     0 character   7326
# pays_dep                 pays_dep       0 0.000000000   134 3.246439e-03     0     0 character    123
# ville_arr               ville_arr       0 0.000000000 17544 4.250412e-01     0     0 character   5844
# pays_arr                 pays_arr       0 0.000000000   218 5.281520e-03     0     0 character    121



courseAnneeEtape$dist[which(courseAnneeEtape$dist == 0)] <- NA
courseAnneeEtape$dist_retenu[which(courseAnneeEtape$dist_retenu == 0)] <- NA

write.csv(courseAnneeEtape, file = 'WRK_courseAnneeEtape.csv', row.names = FALSE)

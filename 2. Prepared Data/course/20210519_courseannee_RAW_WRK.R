#******************************************************************************
## Description .......: Clean the table "courseannee" from the UTMB DB
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-19
## License ...........: GPL
##
## Inputs ............: 'RAW_courseannee.csv" - an csv export of UTMB DB's "courseannee" table
## Outputs ...........: 'WRK_courseannee.csv' 
##                    
#******************************************************************************

library(tidyverse)
library(data.table)
library(lubridate)
library(useful)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/2. Prepared Data/course") #set the working directory 
dataset <- read.csv('RAW_courseannee.csv', na.strings = c("", "NULL", "-1"), encoding = "UTF-8") #load the csv file in a storage dataframe named "dataset"
# the file contains 38815 lines

courseAnnee <- dataset[,c("id", "id_course", "annee", "nom", "challenge", "championnat", 
                                             "dist_tot", "deniv_tot", "deniv_neg_tot", "longitude", "latitude",
                                             "pct_route", "pct_piste", "pct_sentier", "nb_etp", "nb_pts", "nb_pts_old", "nb_pts_tps", 
                                             "dist_cote", "deniv_cote", "deniv_neg_cote", "pct_correction_deniv", "coef_fiab")]
str(courseAnnee)
# > str(courseAnnee)
# 'data.frame':	38815 obs. of  23 variables:
#   $ id                  : int  2 3 5 7 9 10 11 12 14 15 ...
# $ id_course           : int  4 1 535 554 8 474 365 366 421 217 ...
# $ annee               : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
# $ nom                 : chr  "50 Mile" "KALAHARI AUGRABIES EXTREME MARATHON - KAEM" "100 Km" "BROCKEN CHALLENGE" ...
# $ challenge           : chr  NA NA NA NA ...
# $ championnat         : chr  NA NA NA NA ...
# $ dist_tot            : num  81 70 105 80 160 141 66 80 81 72.7 ...
# $ deniv_tot           : int  0 -1 2400 2000 7500 5400 2900 3300 2200 1490 ...
# $ deniv_neg_tot       : int  0 -1 0 -1 7500 0 0 0 0 900 ...
# $ longitude           : chr  "25.6208" "24.9916" "26.2299" "10.4234" ...
# $ latitude            : chr  "-33.9617" "-28.8166" "-29.12" "51.0834" ...
# $ pct_route           : chr  "0" "0" "0" "0" ...
# $ pct_piste           : chr  "0" "0" "0" "0" ...
# $ pct_sentier         : chr  "0" "0" "0" "0" ...
# $ nb_etp              : int  1 6 1 1 1 1 1 1 1 1 ...
# $ nb_pts              : chr  "3" "5" "4" "4" ...
# $ nb_pts_old          : chr  "1" "3" "3" "2" ...
# $ nb_pts_tps          : chr  "0" "0" "0" "0" ...
# $ dist_cote           : chr  "81.00" "70.00" "105.00" "80.00" ...
# $ deniv_cote          : chr  "-1" "-1" "2400" "2000" ...
# $ deniv_neg_cote      : chr  "-1" "-1" "-1" "-1" ...
# $ pct_correction_deniv: chr  "0" "0" "0" "0" ...
# $ coef_fiab           : int  100 100 100 100 100 100 100 100 100 100 ...

status(courseAnnee)
#                                   variable q_zeros      p_zeros  q_na         p_na q_inf p_inf      type unique
# id                                     id       0 0.000000e+00     0 0.0000000000     0     0   integer  38815
# id_course                       id_course       0 0.000000e+00     0 0.0000000000     0     0   integer  14479
# annee                               annee       0 0.000000e+00     0 0.0000000000     0     0   integer     32
# nom                                   nom       1 2.576324e-05    18 0.0004637382     0     0 character  13791
# challenge                       challenge       1 2.576324e-05 34714 0.8943449697     0     0 character   1409
# championnat                   championnat       1 2.576324e-05 35591 0.9169393276     0     0 character   1184
# dist_tot                         dist_tot      93 2.395981e-03     0 0.0000000000     0     0   numeric   2464
# deniv_tot                       deniv_tot     152 3.916012e-03     0 0.0000000000     0     0   integer   3068
# deniv_neg_tot               deniv_neg_tot     971 2.501610e-02     0 0.0000000000     0     0   integer   2994
# longitude                       longitude     905 2.331573e-02  1103 0.0284168492     0     0   numeric   5965
# latitude                         latitude     905 2.331573e-02  1103 0.0284168492     0     0   numeric   5965
# pct_route                       pct_route   15310 3.944351e-01    26 0.0006698441     0     0   integer     95
# pct_piste                       pct_piste   14837 3.822491e-01    26 0.0006698441     0     0   integer    101
# pct_sentier                   pct_sentier   12300 3.168878e-01    26 0.0006698441     0     0   integer    101
# nb_etp                             nb_etp       0 0.000000e+00     0 0.0000000000     0     0   integer     19
# nb_pts                             nb_pts    8001 2.061317e-01  1234 0.0317918331     0     0   integer      8
# nb_pts_old                     nb_pts_old   27184 7.003478e-01  1234 0.0317918331     0     0   integer      5
# nb_pts_tps                     nb_pts_tps   15243 3.927090e-01  1234 0.0317918331     0     0   integer     90
# dist_cote                       dist_cote     550 1.416978e-02  1158 0.0298338271     0     0   numeric   2370
# deniv_cote                     deniv_cote     558 1.437589e-02  1158 0.0298338271     0     0   integer   2686
# deniv_neg_cote             deniv_neg_cote     847 2.182146e-02  1158 0.0298338271     0     0   integer   2604
# pct_correction_deniv pct_correction_deniv    5567 1.434239e-01  7765 0.2000515265     0     0   numeric   9767
# coef_fiab                       coef_fiab       1 2.576324e-05     0 0.0000000000     0     0   integer      4

courseAnnee <- courseAnnee[,c("id", "id_course", "annee", "nom", "dist_tot", "deniv_tot", "deniv_neg_tot", "longitude", "latitude",
                          "pct_route", "pct_piste", "pct_sentier", "nb_etp", "nb_pts", "nb_pts_old", "nb_pts_tps", 
                          "dist_cote", "deniv_cote", "deniv_neg_cote", "pct_correction_deniv", "coef_fiab")]

######################
## numerical variables cleaning 
######################

courseAnnee %>%
  group_by(annee) %>%
  summarise(n()) %>%
  print(n = Inf)

courseAnnee %>% 
  filter(as.numeric(annee) - as.numeric(substring(dataset$dt, 1, 4)) != 0) #check if annee variable correspond to dt variable
#--> dt is not always fiable keep varibale annee

courseAnnee %>%
  ggplot(aes(x = (dist_tot))) +
  geom_boxplot()
courseAnnee %>%
  filter(dist_tot == 0)

boxplot(courseAnnee$dist_tot)$out

courseAnnee %>%
  ggplot(aes(x = (deniv_tot))) +
  geom_boxplot()

boxplot(courseAnnee$deniv_tot)$out
boxplot(courseAnnee$deniv_neg_tot)$out
courseAnnee %>%
  filter(deniv_tot > 80000)

courseAnnee <- courseAnnee %>% #if pct = 0 for all then it's a missing values -> NA 
  mutate(pct_tot = apply(courseAnnee %>% select("pct_route", "pct_piste", "pct_sentier"), 1, sum)) %>%
  mutate(pct_piste = ifelse(pct_tot == 0, NA, pct_piste), 
         pct_route = ifelse(pct_tot == 0, NA, pct_route), 
         pct_sentier = ifelse(pct_tot == 0, NA, pct_sentier))

courseAnnee$dist_tot[which(courseAnnee$dist_tot == 0)] <- NA
courseAnnee$deniv_tot[which(courseAnnee$deniv_tot == 0)] <- NA  
courseAnnee$deniv_neg_tot[which(courseAnnee$deniv_neg_tot == 0)] <- NA 

courseAnnee$dist_cote[which(courseAnnee$dist_cote == 0)] <- NA
courseAnnee$deniv_cote[which(courseAnnee$deniv_cote == 0)] <- NA  
courseAnnee$deniv_neg_cote[which(courseAnnee$deniv_neg_cote == 0)] <- NA 

courseAnnee$latitude[which(courseAnnee$latitude == 0)] <- NA  
courseAnnee$longitude[which(courseAnnee$longitude == 0)] <- NA


courseAnnee %>% #I volontary keep values with a total pct >< 100
  filter(pct_tot != 100)


write.csv(courseAnnee, file = 'WRK_courseannee.csv', row.names = FALSE)

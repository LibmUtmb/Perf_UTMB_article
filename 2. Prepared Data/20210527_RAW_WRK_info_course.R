#******************************************************************************
## Description .......: Clean info_course dataset for the study difference H/F
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-27
## License ...........: GPL
##
## Inputs ............: 'RAW_info_course.csv'
##                      
## Outputs ...........: 'WRK_info_course.csv' 
##                    
#******************************************************************************

library(tidyverse)
library(funModeling)
library(data.table)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/2. Prepared Data/")
dataset <- fread('RAW_info_course.csv')

#dataset contains 41276 lines

info_course <- dataset # create a "info_course" working dataframe
str(info_course)
# > str(info_course)
# Classes 'data.table' and 'data.frame':	41276 obs. of  46 variables:
#   $ id_course_etape     : int  1 1 1 1 1 1 1 1 1 1 ...
# $ id_course_annee     : int  2 3 10385 5 7 9 10 11 12 14 ...
# $ id_course           : int  4 1 2289 535 554 8 474 365 366 421 ...
# $ id_evt              : int  32 597 464 1123 157 224 224 224 224 563 ...
# $ annee               : int  2007 2007 2015 2007 2007 2007 2007 2007 2007 2007 ...
# $ nom_course          : chr  "50 Mile" "KALAHARI AUGRABIES EXTREME MARATHON - KAEM" "GVVG" "100 Km" ...
# $ type_course         : chr  "Trail" "Course en étapes" "Trail" "Trail" ...
# $ pct_route           : int  NA NA 8 NA NA NA NA NA NA NA ...
# $ pct_piste           : int  NA NA 50 NA NA NA NA NA NA NA ...
# $ pct_sentier         : int  NA NA 42 NA NA NA NA NA NA NA ...
# $ nb_etp              : int  1 6 1 1 1 1 1 1 1 1 ...
# $ nb_pts              : int  3 5 5 4 4 6 6 3 4 0 ...
# $ nb_pts_old          : int  1 3 4 3 2 4 4 1 2 0 ...
# $ nb_pts_tps          : int  0 0 360 0 0 0 0 0 0 0 ...
# $ dist                : num  81 250 130 105 80 160 141 66 80 81 ...
# $ deniv               : int  NA NA 5500 2400 2000 7500 5400 2900 3300 2200 ...
# $ deniv_neg           : int  NA NA 5500 NA NA 7500 NA NA NA NA ...
# $ deniv_max_montee    : int  NA NA 902 NA NA NA NA NA NA NA ...
# $ dist_tot            : num  81 70 133 105 80 ...
# $ deniv_tot           : int  NA NA 5560 2400 2000 7500 5400 2900 3300 2200 ...
# $ deniv_neg_tot       : int  NA NA 5600 NA NA 7500 NA NA NA NA ...
# $ dist_retenu         : num  81 70 133 105 80 ...
# $ deniv_retenu        : int  NA NA 5560 2400 2000 7500 5400 2900 3300 2200 ...
# $ deniv_neg_retenu    : int  NA NA 5600 NA NA 7500 NA NA NA NA ...
# $ dist_cote           : num  81 70 133 105 80 ...
# $ deniv_cote          : int  NA NA 5560 2400 2000 7500 5400 2900 3300 2200 ...
# $ deniv_neg_cote      : int  NA NA 5590 NA NA 7500 NA NA NA NA ...
# $ pct_correction_deniv: num  0 0 -13.7 0 0 ...
# $ coef_fiab           : int  100 100 100 100 100 100 100 100 100 100 ...
# $ nb_ravito           : int  NA NA 14 NA NA NA NA NA NA NA ...
# $ alt_min             : int  NA NA 183 NA NA NA NA NA NA NA ...
# $ alt_max             : int  NA NA 1123 NA NA NA NA NA NA NA ...
# $ alt_moy             : int  NA NA 650 NA NA NA NA NA NA NA ...
# $ type_partic         : chr  "solo" "solo" "solo" "solo" ...
# $ nb_eqp              : int  1 1 0 0 1 1 1 1 1 1 ...
# $ contient            : chr  "Africa" "Africa" "Europe" "Africa" ...
# $ pays                : chr  "South Africa" "South Africa" "Spain" "South Africa" ...
# $ code_pays           : chr  "ZA" "ZA" "ES" "ZA" ...
# $ cio                 : chr  "RSA" "RSA" "ESP" "RSA" ...
# $ alpha3              : chr  "ZAF" "ZAF" "ESP" "ZAF" ...
# $ ville               : chr  "Addo" "" "BENARRABA" "Bloemfontein" ...
# $ cp                  : chr  "6001" "" "29490" "Po Box 124" ...
# $ code_subdiv         : chr  "EC" NA "AN" "FS" ...
# $ libelle_subdiv      : chr  "Eastern Cape" NA "Andalucia" "Free State" ...
# $ longitude           : num  25.62 24.99 -5.12 26.23 10.42 ...
# $ latitude            : num  -34 -28.8 36.6 -29.1 51.1 ...
# - attr(*, ".internal.selfref")=<externalptr> 


status(info_course)
# > status(info_course)
# variable q_zeros      p_zeros  q_na         p_na q_inf p_inf      type unique
# id_course_etape           id_course_etape       0 0.000000e+00     2 4.845431e-05     0     0   integer     56
# id_course_annee           id_course_annee       0 0.000000e+00     0 0.000000e+00     0     0   integer  38860
# id_course                       id_course       0 0.000000e+00    53 1.284039e-03     0     0   integer  14478
# id_evt                             id_evt       0 0.000000e+00    56 1.356721e-03     0     0   integer   5858
# annee                               annee       0 0.000000e+00    53 1.284039e-03     0     0   integer     32
# nom_course                     nom_course       1 2.422715e-05    76 1.841264e-03     0     0 character  13782
# type_course                   type_course       0 0.000000e+00    67 1.623219e-03     0     0 character      6
# pct_route                       pct_route    4578 1.109119e-01 11395 2.760684e-01     0     0   integer     95
# pct_piste                       pct_piste    3824 9.264464e-02 11395 2.760684e-01     0     0   integer    101
# pct_sentier                   pct_sentier    1147 2.778855e-02 11395 2.760684e-01     0     0   integer    101
# nb_etp                             nb_etp       0 0.000000e+00    53 1.284039e-03     0     0   integer     19
# nb_pts                             nb_pts    8256 2.000194e-01  1477 3.578351e-02     0     0   integer      8
# nb_pts_old                     nb_pts_old   28849 6.989292e-01  1477 3.578351e-02     0     0   integer      5
# nb_pts_tps                     nb_pts_tps   15971 3.869319e-01  1477 3.578351e-02     0     0   integer     90
# dist                                 dist       0 0.000000e+00    87 2.107762e-03     0     0   numeric   2912
# deniv                               deniv       0 0.000000e+00   369 8.939820e-03     0     0   integer   4926
# deniv_neg                       deniv_neg       0 0.000000e+00  2373 5.749104e-02     0     0   integer   4871
# deniv_max_montee         deniv_max_montee       0 0.000000e+00 15001 3.634315e-01     0     0   integer    820
# dist_tot                         dist_tot       0 0.000000e+00   148 3.585619e-03     0     0   numeric   2463
# deniv_tot                       deniv_tot       0 0.000000e+00   376 9.109410e-03     0     0   integer   3066
# deniv_neg_tot               deniv_neg_tot       0 0.000000e+00  2224 5.388119e-02     0     0   integer   2992
# dist_retenu                   dist_retenu       0 0.000000e+00  4904 1.188100e-01     0     0   numeric   2084
# deniv_retenu                 deniv_retenu       0 0.000000e+00  5136 1.244307e-01     0     0   integer   2436
# deniv_neg_retenu         deniv_neg_retenu       0 0.000000e+00  6932 1.679426e-01     0     0   integer   2330
# dist_cote                       dist_cote       0 0.000000e+00  1976 4.787286e-02     0     0   numeric   2369
# deniv_cote                     deniv_cote       0 0.000000e+00  2196 5.320283e-02     0     0   integer   2684
# deniv_neg_cote             deniv_neg_cote       0 0.000000e+00  3999 9.688439e-02     0     0   integer   2602
# pct_correction_deniv pct_correction_deniv    5777 1.399603e-01  8757 2.121572e-01     0     0   numeric   9766
# coef_fiab                       coef_fiab       1 2.422715e-05    53 1.284039e-03     0     0   integer      4
# nb_ravito                       nb_ravito       0 0.000000e+00 12398 3.003683e-01     0     0   integer     53
# alt_min                           alt_min       0 0.000000e+00 16215 3.928433e-01     0     0   integer   2407
# alt_max                           alt_max       0 0.000000e+00 14686 3.558000e-01     0     0   integer   3547
# alt_moy                           alt_moy       0 0.000000e+00 15044 3.644733e-01     0     0   integer   3146
# type_partic                   type_partic       0 0.000000e+00    93 2.253125e-03     0     0 character      3
# nb_eqp                             nb_eqp   28398 6.880027e-01    56 1.356721e-03     0     0   integer     18
# contient                         contient       0 0.000000e+00   119 2.883031e-03     0     0 character      6
# pays                                 pays       0 0.000000e+00   119 2.883031e-03     0     0 character    119
# code_pays                       code_pays       0 0.000000e+00   118 2.858804e-03     0     0 character    120
# cio                                   cio       0 0.000000e+00   119 2.883031e-03     0     0 character    119
# alpha3                             alpha3       0 0.000000e+00   119 2.883031e-03     0     0 character    119
# ville                               ville       0 0.000000e+00    56 1.356721e-03     0     0 character   3708
# cp                                     cp      45 1.090222e-03    90 2.180444e-03     0     0 character   3384
# code_subdiv                   code_subdiv       0 0.000000e+00  5989 1.450964e-01     0     0 character    459
# libelle_subdiv             libelle_subdiv       0 0.000000e+00  5989 1.450964e-01     0     0 character    863
# longitude                       longitude       0 0.000000e+00  2245 5.438996e-02     0     0   numeric   5964
# latitude                         latitude       0 0.000000e+00  2245 5.438996e-02     0     0   numeric   5964

######################
## Keep only one step race
######################
length(unique(info_course$id_course_annee))
  

info_course <- info_course %>% 
  filter(nb_etp == 1)
#- 37 424 lines

info_course %>% #double check race occurrences
  group_by(id_course_annee) %>%
  summarise(nb_ = n()) %>%
  group_by(nb_) %>%
  summarise(count = length(nb_))

info_course %>% #delete step races
  group_by(nb_etp) %>%
  summarise(nb_ = n()) 


######################
## Keep only trail race
######################
info_course %>%
  group_by(type_course) %>%
  summarise(n())

keepType_course <- c("Trail")

info_course <- info_course %>% 
  filter(type_course == keepType_course)
#- 35 798 lines remaining 


######################
## Keep only solo race
######################

info_course %>%
  group_by(type_partic) %>%
  summarise(n())

#We consider "" as a not solo race

keepType_partic <- c("solo")

info_course <- info_course %>% 
  filter(type_partic %in% keepType_partic)

#33862 lines

######################
## Clean distance variable 
######################

##dist -> distance gave by the race organisation
##dist_tot -> 
##dist_retenu -> distance caculated by UTMB gps during the race (most reliable)
##dist_cote -> distance modify by UTMB to calculate cote
## dist_retenu >> dist_tot >> dist >> dist_cote

status(info_course %>%
          select(dist, dist_tot, dist_retenu, dist_cote))

summary(info_course %>%
         select(dist, dist_tot, dist_retenu, dist_cote))

info_course %>%
  select(dist, dist_tot, dist_retenu, dist_cote) %>%
  filter(is.na(dist_retenu) == TRUE & is.na(dist_tot) == TRUE)
  
dist_final <- ifelse(is.na(info_course$dist_retenu) == FALSE, info_course$dist_retenu, info_course$dist_tot )
dist_final <- ifelse(is.na(dist_final) == FALSE, dist_final, info_course$dist_tot )
dist_final <- ifelse(is.na(dist_final) == FALSE, dist_final, info_course$dist )

status(dist_final)
summary(dist_final)

info_course$dist_final <- dist_final

info_course %>%
  filter(dist_final <= 3)


summary(info_course %>%
          select(dist, dist_tot, dist_retenu, dist_cote, dist_final))
hist(info_course$dist_final)


######################
## Clean deniv variable 
######################
summary(info_course %>%
         select(deniv, deniv_tot, deniv_retenu, deniv_cote))
status(info_course %>%
          select(deniv, deniv_tot, deniv_retenu, deniv_cote))

info_course %>%
  select(deniv, deniv_tot, deniv_retenu, deniv_cote) %>%
  filter(is.na(deniv_retenu) == TRUE & is.na(deniv_tot) == TRUE) 



deniv_final <- ifelse(is.na(info_course$deniv_retenu) == FALSE, info_course$deniv_retenu, info_course$deniv_tot )
deniv_final <- ifelse(is.na(deniv_final) == FALSE, deniv_final, info_course$deniv_tot )
deniv_final <- ifelse(is.na(deniv_final) == FALSE, deniv_final, info_course$deniv )


status(deniv_final)
summary(deniv_final)

info_course[which(is.na(deniv_final) == TRUE),] %>%
  select(deniv, deniv_tot, deniv_retenu, deniv_cote) %>%
  print(n = Inf)


info_course$deniv_final <- deniv_final

summary(info_course %>%
          select(deniv, deniv_tot, deniv_retenu, deniv_cote, deniv_final))

hist(info_course$deniv_final)

######################
## Clean deniv_neg variable 
######################
summary(info_course %>%
          select(deniv_neg, deniv_neg_tot, deniv_neg_retenu, deniv_neg_cote))

info_course %>%
  select(deniv_neg, deniv_neg_tot, deniv_neg_retenu, deniv_neg_cote) %>%
  filter(is.na(deniv_neg_retenu) == TRUE & is.na(deniv_neg_tot) == TRUE) 

deniv_neg_final <- ifelse(is.na(info_course$deniv_neg_retenu) == FALSE, info_course$deniv_neg_retenu, info_course$deniv_neg_tot )
deniv_neg_final <- ifelse(is.na(deniv_neg_final) == FALSE, deniv_neg_final, info_course$deniv_neg_tot )
deniv_neg_final <- ifelse(is.na(deniv_neg_final) == FALSE, deniv_neg_final, info_course$deniv_neg )


summary(deniv_neg_final)
info_course[which(is.na(deniv_neg_final) == TRUE),]

info_course$deniv_neg_final <- deniv_neg_final

summary(info_course %>%
          select(deniv_neg, deniv_neg_tot, deniv_neg_retenu, deniv_neg_cote, deniv_neg_final))
hist(info_course$deniv_neg_final)

######################
## Clean deniv_max_montee variable 
######################

info_course$deniv_max_montee <- as.numeric(info_course$deniv_max_montee)
summary(info_course$deniv_max_montee)

######################
## nb_pts, nb_pts_old, nb_pts_tps variable 
######################

info_course$nb_pts <- as.numeric(info_course$nb_pts)
summary(info_course$nb_pts)

info_course$nb_pts_old <- as.numeric(info_course$nb_pts_old)
summary(info_course$nb_pts_old)

info_course$nb_pts_tps <- as.numeric(info_course$nb_pts_tps)
summary(info_course$nb_pts_tps)


######################
## pct_correction_deniv 
######################

info_course$pct_correction_deniv <- as.numeric(info_course$pct_correction_deniv)
summary(info_course$pct_correction_deniv)

######################
## alt_min, alt_max, alt_moy
######################

info_course$alt_min <- as.numeric(info_course$alt_min)
summary(info_course$alt_min)
boxplot(info_course$alt_min)$out
hist(info_course$alt_min)

info_course$alt_max <- as.numeric(info_course$alt_max)
summary(info_course$alt_max)
boxplot(info_course$alt_max)$out

info_course$alt_moy <- as.numeric(info_course$alt_moy)
summary(info_course$alt_moy)
boxplot(info_course$alt_moy)$out

info_course %>% #modification of extreme outlier
  filter(alt_min == 32767)
info_course$alt_max[which(info_course$id_course_annee == 46103)] <- 1680
info_course$alt_min[which(info_course$id_course_annee == 46103)] <- 1254
info_course$alt_moy[which(info_course$id_course_annee == 46103)] <- (1680 + 1254)/2


######################
## variable creation
######################

info_course$km_effort <- info_course$dist_final + info_course$deniv_final/100

km_effort <- c(0,25,45,75,115,155,210,Inf)
info_course$catUTMB <- cut(info_course$km_effort, km_effort, right = FALSE)



info_course <- info_course[,c("id_course_annee", "id_course", "id_evt", "annee", "nom_course", "dist_final", "deniv_final","deniv_neg_final","km_effort",
                              "catUTMB", "dist_cote", "deniv_cote","deniv_neg_cote", "pct_route", "pct_piste", "pct_sentier", "nb_pts", "nb_pts_old", "nb_pts_tps",
                              "pct_correction_deniv", "coef_fiab", "nb_ravito", "alt_min", "alt_max", "alt_moy", "contient", "pays", "cio", "alpha3", "code_subdiv",
                              "longitude", "latitude")]
write.csv(info_course, 'WRK_info_course.csv', row.names = FALSE)

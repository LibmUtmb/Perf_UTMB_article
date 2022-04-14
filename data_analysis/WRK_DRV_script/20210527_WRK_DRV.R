#******************************************************************************
## Description .......: Formatting dataset for descriptive analysis and study
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-31 updated(2022/01/13)
## License ...........: GPL
##
## Inputs ............: 'WRK_info_course.csv'
##                      'WRK_result.csv'            
## Outputs ...........: 'DRV_result.csv'
##                      'DRV_info_course.csv'
##                        
#******************************************************************************

library(tidyverse)
library(lubridate)
library(funModeling)
library(data.table)

######################
## Dataset load 
######################

setwd("~/Project_dataScience/UTMB_dataMining/3. Analysis/1. WRK_DRV_script/")

dataset <- fread('WRK_info_course.csv')
info_course <- dataset %>%
  dplyr::select(id_course_annee,
                nom_course,
                annee,
                dist_final,
                deniv_final,
                deniv_neg_final,
                km_effort,
                catUTMB,
                contient,
                cio)

result <- fread('WRK_result.csv')


######################
## Merging 
######################

df <- inner_join(info_course, result , by = 'id_course_annee') #join both 
summary(df)
status(df)

######################
## Encoding variable
######################

# Ordering catUTMB 
km_effort <- c(0,25,45,75,115,155,210,Inf)
df$catUTMB <- cut(df$km_effort, km_effort, right = FALSE)

# Encoding categorical features as factor
df$nationalite <- as.factor(df$nationalite)
df$sexe <- as.factor(df$sexe)

df$age <- df$annee - df$naissance #creation of age variable before encoding annee and naissance

df$naissance <- as.factor(df$naissance)
df$annee <- as.factor(df$annee)
df$contient <- as.factor(df$contient)
df$cio <- as.factor(df$cio)

# Calculation of temps_s which is the time in seconds
df$temps_s <- lubridate::hour(hms(df$temps))* 3600 + (lubridate::minute(hms(df$temps)) *60) + (lubridate::second(hms(df$temps))) #calculate running time in hours
summary(df$temps_s)

# Average runner speed on a race
df$vitesse_moy <- df$dist_final / (df$temps_s/3600) #calculate the average speed for each results
summary(df$vitesse_moy)

######################
## Cleaning datasets 
######################

boxplot(df$vitesse_moy)$out #Shows outlier

df %>%
  filter(vitesse_moy > quantile(vitesse_moy, 0.75, na.rm = TRUE) + 3 * IQR(vitesse_moy, na.rm = TRUE)) %>%
  arrange(desc(vitesse_moy)) %>%
  print(n = Inf)#check outliers upper

# We have decide too drop all speed above 22 km/h
df <- df %>%
  filter(vitesse_moy < 22)

df %>%
  filter(vitesse_moy < quantile(vitesse_moy, 0.25, na.rm = TRUE) - 3 * IQR(vitesse_moy, na.rm = TRUE)) %>%
  arrange(desc(vitesse_moy)) #check outliers lower


df <- df %>%
  filter(vitesse_moy > 0.1)

#-4571498 lines remaining

######################
## Calculating interest variable 
######################

compl_race <- df %>%
  group_by(id_course_annee) %>%
  arrange(desc(vitesse_moy)) %>%
  summarise(
    rate_DNF_M = sum(is.na(temps_s[sexe == "H"]))/length(temps_s[sexe == "H"]), #DNF men rate
    
    rate_DNF_W = sum(is.na(temps_s[sexe == "F"]))/length(temps_s[sexe == "F"]), #DNF women rate
    
    rate_F = length(sexe[sexe == "F"])/length(sexe), #women rate per race
    
    participation = length(id_coureur), #participation
    
    temps_best = min(temps_s), #Best time in the race
    
    diff_best = (max(vitesse_moy[sexe == "H"], na.rm = TRUE)
                 - max(vitesse_moy[sexe == "F"], na.rm = TRUE))
    / max(vitesse_moy[sexe == "F"], na.rm = TRUE), #difference between best W / best M
    
    diff_3best =( (mean(c(nth(vitesse_moy[sexe == "H"], 1L), #difference between 3 best W and 3 best M
                          nth(vitesse_moy[sexe == "H"], 2L),
                          nth(vitesse_moy[sexe == "H"], 3L)
    ), na.rm = TRUE
    ) - mean(c(nth(vitesse_moy[sexe == "F"], 1L),
               nth(vitesse_moy[sexe == "F"], 2L),
               nth(vitesse_moy[sexe == "F"], 3L)
    ), na.rm = TRUE
    )
    )
    / mean(c(nth(vitesse_moy[sexe == "F"], 1L),
             nth(vitesse_moy[sexe == "F"], 2L),
             nth(vitesse_moy[sexe == "F"], 3L)
    ), na.rm = TRUE
    )),
    
    diff_5best =( (mean(c(nth(vitesse_moy[sexe == "H"], 1L), #difference between 3 best W and 3 best M
                          nth(vitesse_moy[sexe == "H"], 2L),
                          nth(vitesse_moy[sexe == "H"], 3L),
                          nth(vitesse_moy[sexe == "H"], 4L),
                          nth(vitesse_moy[sexe == "H"], 5L)
    ), na.rm = TRUE
    ) - mean(c(nth(vitesse_moy[sexe == "F"], 1L),
               nth(vitesse_moy[sexe == "F"], 2L),
               nth(vitesse_moy[sexe == "F"], 3L),
               nth(vitesse_moy[sexe == "H"], 4L),
               nth(vitesse_moy[sexe == "H"], 5L)
    ), na.rm = TRUE
    )
    )
    / mean(c(nth(vitesse_moy[sexe == "F"], 1L),
             nth(vitesse_moy[sexe == "F"], 2L),
             nth(vitesse_moy[sexe == "F"], 3L),
             nth(vitesse_moy[sexe == "H"], 4L),
             nth(vitesse_moy[sexe == "H"], 5L)
    ), na.rm = TRUE
    )),
    
    # diff_5perc =  ifelse(length(sexe[sexe == "F"])>= 20, (mean(vitesse_moy[sexe == "H" & vitesse_moy > quantile(vitesse_moy[sexe == "H"], 0.95)]) #difference between 5% best W and 5% best M
    #                                                       - mean(vitesse_moy[sexe == "F" & vitesse_moy > quantile(vitesse_moy[sexe == "F"], 0.95)]))
    #                      / mean(vitesse_moy[sexe == "F" & vitesse_moy > quantile(vitesse_moy[sexe == "F"], 0.95)]), NA), 
    diff_5perc =  (mean(vitesse_moy[sexe == "H" & vitesse_moy > quantile(vitesse_moy[sexe == "H"], 0.95, na.rm = TRUE)], na.rm = TRUE) #difference between 5% best W and 5% best M
                   - mean(vitesse_moy[sexe == "F" & vitesse_moy > quantile(vitesse_moy[sexe == "F"], 0.95, na.rm = TRUE)], na.rm = TRUE))
    / mean(vitesse_moy[sexe == "F" & vitesse_moy > quantile(vitesse_moy[sexe == "F"], 0.95, na.rm = TRUE)], na.rm = TRUE), 
    
    diff_mean = (mean(vitesse_moy[sexe == "H"], na.rm = TRUE) #difference mean time F and M
                 - mean(vitesse_moy[sexe == "F"], na.rm = TRUE)) 
    / mean(vitesse_moy[sexe == "F"], na.rm = TRUE)
    
  )

#Warnings because of missing values 

summary(compl_race)

######################
## Dataset book GUI
######################

df_UTMB <- info_course %>% filter(nom_course == "UTMB®" | nom_course == "CCC®" | nom_course == "OCC" | nom_course == "TDS®")

fwrite(df_UTMB, 'info_course_UTMB.csv', row.names = FALSE)

######################
## Participation filter 
######################

ggplot(compl_race, aes(y = rate_F, x = participation)) + #check aberration pattern
  geom_point(position = 'jitter')

#keep race with at least 15 participants and mixed participation
keepRace <- compl_race$id_course_annee[which(compl_race$participation >=15 
                                             & compl_race$rate_F != 1 
                                             & compl_race$rate_F != 0 
                                             & compl_race$rate_F != 0.5)] 
compl_race <- compl_race %>%
  filter(id_course_annee %in% keepRace)

df %>%
  filter(id_course_annee %in% compl_race$id_course_annee[which(compl_race$rate_F == 1 
                                                               | compl_race$rate_F == 0 
                                                               | compl_race$rate_F == 0.5)]) %>%
  count()

df <- df %>%
  filter(id_course_annee %in% keepRace)


##Manual check rate_W > 0.75 
length(which(compl_race$rate_F >= 0.75 & compl_race$rate_F < 1))
#18
manual_check <- compl_race$id_course_annee[which(compl_race$rate_F >= 0.75 & compl_race$rate_F < 1)]
manual_check
# [1] 11819 14531 14533 15791 19407 28537 36481 39951 43927 45413 46583 46931 50279 50811 59787 60901 63601 65521

# df %>%
#   filter(id_course_annee == 65521) %>%
#   print(n=Inf)

compl_race <- compl_race %>%
  filter(id_course_annee != 11819 & id_course_annee != 15791 & id_course_annee != 36481 & id_course_annee != 46931)

df <- df %>%
  filter(id_course_annee != 11819 & id_course_annee != 15791 & id_course_annee != 36481 & id_course_annee != 46931)


length(which(compl_race$rate_F > 0 & compl_race$rate_F <= 0.01))
#2
manual_check <- compl_race$id_course_annee[which(compl_race$rate_F > 0 & compl_race$rate_F <= 0.01)]
manual_check
# [1]  8097 55515

df %>%
  filter(id_course_annee == 55515) %>%
  print(n=200)

summary(df)

# inner join compl race and info course 
info_course <- inner_join(compl_race, info_course, by = "id_course_annee")

######################
## Info course filter for study Speed H vs F
######################

# We will study ony race from 2010 to 2019
range <- seq(2010, 2019)

info_course %>%
  group_by(annee) %>%
  summarise(n())

info_course %>%
  filter(annee %in% range) %>%
  summarise(n())

info_course <- info_course %>% 
  filter(annee %in% range)

info_course <- info_course %>% filter(is.na(km_effort) == FALSE)

# Change NaN into NA
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
info_course[is.nan(info_course)] <- NA

summary(info_course)

# Drop race dropped in races filtering
df <- df %>%
  filter(id_course_annee %in% unique(info_course$id_course_annee))

#21089

######################
## df filter for study Speed H vs F
######################

# Drop missing value of features of interest
df <- df[complete.cases(df$temps),]
df <- df[complete.cases(df$km_effort),]

df %>% #race with an average age less than 20 
  group_by(id_course_annee, annee) %>%
  summarise(moy_age = mean(age)) %>%
  filter(moy_age < 20)

# We keep for the study runner from 18 to 60 years old
df <- df %>% 
  filter(age >= 18 & age <= 60) 
summary(df$age)



status(df)
# > status(df)
# variable q_zeros      p_zeros  q_na         p_na q_inf p_inf      type  unique
# id_course_annee id_course_annee       0 0.000000e+00     0 0.000000e+00     0     0   integer   21088
# nom_course           nom_course      29 7.534282e-06     0 0.000000e+00     0     0 character    6884
# annee                     annee       0 0.000000e+00     0 0.000000e+00     0     0    factor      10
# dist_final           dist_final       0 0.000000e+00     0 0.000000e+00     0     0   numeric    1767
# deniv_final         deniv_final       0 0.000000e+00     0 0.000000e+00     0     0   integer    1974
# deniv_neg_final deniv_neg_final       0 0.000000e+00  9676 2.513852e-03     0     0   integer    1985
# km_effort             km_effort       0 0.000000e+00     0 0.000000e+00     0     0   numeric    3511
# catUTMB                 catUTMB       0 0.000000e+00     0 0.000000e+00     0     0 character       7
# contient               contient       0 0.000000e+00    29 7.534282e-06     0     0    factor       6
# cio                         cio       0 0.000000e+00    29 7.534282e-06     0     0    factor      97
# id_coureur           id_coureur       0 0.000000e+00     0 0.000000e+00     0     0   integer 1381914
# temps                     temps       0 0.000000e+00     0 0.000000e+00     0     0 character  166617
# place                     place       0 0.000000e+00    21 5.455859e-06     0     0   integer    5765
# cote                       cote       0 0.000000e+00 25281 6.568075e-03     0     0   numeric   67838
# nationalite         nationalite       0 0.000000e+00     0 0.000000e+00     0     0    factor     195
# sexe                       sexe       0 0.000000e+00     0 0.000000e+00     0     0    factor       2
# naissance             naissance       0 0.000000e+00     0 0.000000e+00     0     0    factor      52
# age                         age       0 0.000000e+00     0 0.000000e+00     0     0   integer      43
# temps_s                 temps_s       0 0.000000e+00     0 0.000000e+00     0     0   numeric  166617
# vitesse_moy         vitesse_moy       0 0.000000e+00     0 0.000000e+00     0     0   numeric 2484455



df %>%
  group_by(catUTMB) %>%
  summarise(nb_course = length(unique(id_course_annee)), 
            nb_H = length(unique(id_coureur[sexe == 'H'])),
            nb_F = length(unique(id_coureur[sexe == 'F'])),
            nb_coureur = length(unique(id_coureur)))

df %>%
  summarise(nb_course = length(unique(id_course_annee)), 
            nb_H = length(unique(id_coureur[sexe == 'H'])),
            nb_F = length(unique(id_coureur[sexe == 'F'])),
            nb_coureur = length(unique(id_coureur)))

fwrite(df, 'DRV_result.csv', row.names = FALSE)
fwrite(info_course, 'DRV_info_course.csv', row.names = FALSE)

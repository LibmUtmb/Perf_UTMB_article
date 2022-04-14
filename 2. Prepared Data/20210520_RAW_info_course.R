#******************************************************************************
## Description .......: Join all UTMB DB and keep needed variable
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-27
## License ...........: GPL
##
## Inputs ............: 'WRK_course.csv'
##                      'WRK_courseannee.csv'
##                      'WRK_courseAnneeEtape.csv'
##                      'WRK_evenement.csv'
##                      'WRK_subdivision.csv'
##                      'WRK_paysnom.csv'
##                      'WRK_pays.csv'
##                      'WRK_continent.csv'
##                      'WRK_typecourse.csv'
## Outputs ...........: 'RAW_info_course.csv' 
##                    
#******************************************************************************

library(tidyverse)
library(data.table)
library(funModeling)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/2. Prepared Data/")

course <- fread('course/WRK_course.csv', stringsAsFactors = TRUE)
colnames(course)[1] <- "id_course"

courseannee <- fread('course/WRK_courseannee.csv')
colnames(courseannee)[1] <- "id_course_annee"

info_course <- left_join(courseannee, course, by = 'id_course') #join table
status(info_course)
courseAnneeEtape <- fread('course/WRK_courseAnneeEtape.csv')

info_course <- left_join(courseAnneeEtape, info_course, by = 'id_course_annee')

evt <- fread('evenement/WRK_evenement.csv')
colnames(evt)[1] <- "id_evt"

info_course <- left_join(info_course, evt, by = 'id_evt')

subdiv <- fread('localisation/WRK_subdivision.csv', encoding = "UTF-8")
colnames(subdiv)[1] <- "id_subdiv"

info_course <- left_join(info_course, subdiv, by = 'id_subdiv')

pays <- fread('localisation/WRK_paysnom.csv', encoding = "UTF-8")
pays <- pays %>% 
  filter(code_lg == '_en')

colnames(info_course)[46] <- "code_pays"
colnames(info_course)[49] <- "code_subdiv"
colnames(info_course)[50] <- "libelle_subdiv"

colnames(pays)[1] <- "code_pays"

info_course <- left_join(info_course, pays, by = 'code_pays')

paysplus <- fread('localisation/WRK_pays.csv', encoding = "UTF-8")

colnames(paysplus)[1] <- "code_pays"

info_course <- left_join(info_course, paysplus, by = 'code_pays')

continent <- fread('localisation/WRK_continent.csv', encoding = "UTF-8")
continent <- continent %>% 
  filter(lg == '_en')
colnames(continent)[1] <- "id_continent"

info_course <- left_join(info_course, continent, by = 'id_continent')

typecourse <- fread('course/WRK_typecourse.csv', encoding = "UTF-8")
colnames(typecourse)[1] <- "id_type_course"

info_course <- left_join(info_course, typecourse, by = 'id_type_course')

info_course <- info_course[,c("idx", "id_course_annee","id_course","id_evt", "annee", "nom.x", "libelle",
                              "pct_route", "pct_piste", "pct_sentier", "nb_etp", "nb_pts", "nb_pts_old", "nb_pts_tps","dist", "deniv", "deniv_neg", "deniv_max_montee",
                              "dist_tot", "deniv_tot", "deniv_neg_tot", "dist_retenu", "deniv_retenu", "deniv_neg_retenu",  
                              "dist_cote", "deniv_cote","deniv_neg_cote", "pct_correction_deniv", "coef_fiab","nb_ravito", "alt_min", "alt_max", "alt_moy", "type_partic", 
                              "nb_eqp", "nom", "nom_epure", "code_pays", "cio", "alpha3", "ville", "cp", "code_subdiv", "libelle_subdiv", "longitude", "latitude")]

colnames(info_course) <- c("id_course_etape", "id_course_annee", "id_course","id_evt", "annee", "nom_course", "type_course", "pct_route", "pct_piste", "pct_sentier","nb_etp",
                           "nb_pts", "nb_pts_old", "nb_pts_tps", "dist", "deniv","deniv_neg", "deniv_max_montee", "dist_tot","deniv_tot", "deniv_neg_tot", "dist_retenu", "deniv_retenu", "deniv_neg_retenu",
                           "dist_cote", "deniv_cote", "deniv_neg_cote", "pct_correction_deniv", "coef_fiab", "nb_ravito", "alt_min", "alt_max", "alt_moy",
                           "type_partic", "nb_eqp", "contient", "pays", "code_pays", "cio", "alpha3", 'ville', "cp", "code_subdiv", "libelle_subdiv",
                           "longitude", "latitude")

status(info_course)

write.csv(info_course, 'RAW_info_course.csv', row.names = FALSE)


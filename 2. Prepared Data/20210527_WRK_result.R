#******************************************************************************
## Description .......: Create result dataset by merging course and coureru dataset
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-27
## License ...........: GPL
##
## Inputs ............: 'WRK_courseres.csv'
##                      'WRK_coureur.csv'            
## Outputs ...........: 'WRK_runner.csv' 
##                    
#******************************************************************************

library(tidyverse)
library(funModeling)
library(data.table)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/2. Prepared Data/")
dataset <- fread('course/WRK_courseres.csv')
courseRes <- dataset

# dataset %>%
#   group_by(id_course_annee, id_coureur) %>%
#   filter(n() > 1) %>%
#   arrange(id_course_annee, id_coureur)

runner <- read.csv('coureur/WRK_coureur.csv')
colnames(runner)[1] <- "id_coureur"

result <- inner_join(courseRes, runner, by = "id_coureur")

result <- result %>%
  select("id_course_annee", "id_coureur", "temps", "place", "cote", "nationalite", "sexe", "naissance")

result <- result[!(duplicated(result ) | duplicated(result , fromLast = TRUE))]

status(result)

write.csv(result, 'WRK_result.csv', row.names = FALSE)

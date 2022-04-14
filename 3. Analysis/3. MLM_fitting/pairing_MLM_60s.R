#******************************************************************************
## Description .......: Pairs all M/W pair per race and year and calculate speed differences
##
## Author ............: Franck Le Mat
## Date ..............: 2021-07-01
## License ...........: GPL
##
## Inputs ............: 'DRV_result.csv'
##                      
## Outputs ...........: 'x_pair.csv' 
##                    
#******************************************************************************

library(tidyverse)
library(funModeling)
library(rstatix)
library(stringr)
library(data.table)
library(pander)
library(stringr)
library(scales)
library(ggpubr)
library(psych)

memory.limit(size = 999999) #Windows only increase the memory uses

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/3. Analysis/")
df <- fread('DRV_result.csv')

######################
## Create variables
######################

# Ordering catUTMB variable
km_effort <- c(0,25,45,75,115,155,210,Inf)
df$catUTMB <- cut(x = df$km_effort,
                  breaks = km_effort,
                  right = FALSE,
                  include.lowest = TRUE)

# Encoding catUMB to factor
df$catUTMB <- factor(df$catUTMB)

# Reseting cache
gc(reset = TRUE)

######################
## Create all pairs
######################

# Creation of all M/F pairs, to do so I split df into each catUTMB then split 
#   those list into each year then split those list into each race and then 
#   match all male and female runner per race

ans <- lapply(split(x = setDT(df),
                    by = "catUTMB",
                    sorted = TRUE),
              function(x) lapply(split(x = setDT(x),
                                       by = "annee"),
                                 function(x) lapply(split(x = setDT(x),
                                                          by = "id_course_annee"),
                                                    function(x) CJ(M = x[sexe == "H",
                                                                         id_coureur],
                                                                   W = x[sexe == "F",
                                                                         id_coureur])
                                 )
              )
)


######################
## Create pairs on "short cat"
######################

# reset cache
gc(reset = TRUE)

# For the study the "short" category to match samle level M and F are the 
#   24:45 km effort category
short <- levels(df$catUTMB)[2]

pair_short <- data.table()

# Create all possible existing M/F pairs per race each year
pair_short <- rbindlist(l = lapply(ans[[short]],
                                   function(x) rbindlist(l = x,
                                                         idcol = "id_course_annee")),
                        idcol = "annee") %>%
  transform(id_course_annee = as.integer(id_course_annee),
            annee = as.integer(annee))

# Drop eventual duplicate pairs 
pair_short <- pair_short[!(duplicated(pair_short) 
                           | duplicated(pair_short,
                                        fromLast = TRUE))]

# nb_resultat_init <- nrow(pair_short)
# [1] 92324870

# nb_paires_distinct <- pair_short %>%
#   dplyr::select("annee",
#                 "W",
#                 "M") %>%
#   n_distinct()
# [1] 91848468

##################
## Pair Model 1 -> 60s filter
##################

# Keep only features of interest
df_short <- df %>%
  filter(catUTMB == short) %>%
  dplyr::select("id_coureur",
                "id_course_annee",
                "temps_s",
                "vitesse_moy")

# Only keep runner which are in pairs and create of features speed diff
pair_short <- pair_short %>%
  inner_join(df_short,
             by = c("M" = "id_coureur", "id_course_annee")) %>%
  inner_join(df_short,
             by = c("W" = "id_coureur", "id_course_annee"),
             suffix = c("_H", "_F")) %>%
  mutate(diff  = (vitesse_moy_H - vitesse_moy_F) / vitesse_moy_F)
  
# nb_elimine_60s <- pair_short %>%
#   filter(abs(temps_s_H - temps_s_F) < 60) %>%
#   n_distinct()
# [1] 1384027

# Filter pairs with a time difference inferior to 60s to eliminate runner which
#   runner voluntary together
keep_pair <- pair_short %>%
  filter(abs(temps_s_H - temps_s_F) >= 60) %>%
  group_by(W, annee) %>%
  filter(abs(diff) == min(abs(diff))) %>%
  ungroup()

# nb_pair_filter <- keep_pair %>%
#   filter(abs(diff) > .10) %>%
#   count
# 1925


# Keep pairs with the smallest speed difference based on finding the 
#   closest male runner for each female runner
keep_pair <- keep_pair %>%
  filter(abs(diff) <= .10) %>%
  dplyr::select("M", "W", "annee") %>%
  distinct()
# 235063 final pairs 
  
# Match all selected pairs in the respective races
pair_short <- pair_short %>%
  inner_join(keep_pair,
             by = c("M",
                    "W",
                    "annee")) %>%
  select("annee",
         "id_course_annee",
         "M",
         "W")

# 236724 year pairs

# stats <- rbind(stats, cbind(catUTMB = short,
#                             nb_resultat_init,
#                             nb_paires_distinct,
#                             nb_elimine_60s,
#                             nb_resultat_final = nrow(pair_short)
#                             ))

##################
## create "long" cat pairs
##################

result_tot <- data.table()
pair_long <- data.table()

gc(reset = TRUE)

# Create all possible pairs in all others UTMB category
for(cat in c(1,3,4,5,6,7)){ 
  long <- levels(df$catUTMB)[cat]
  
  pair_long <- rbind(pair_long,
                     rbindlist(lapply(ans[[long]],
                                      function(x) rbindlist(l = x,
                                                            idcol = "id_course_annee")),
                               idcol = "annee") %>%
                       transform(id_course_annee = as.integer(id_course_annee),
                                 annee = as.integer(annee)))
}

# Drop eventual duplicated pairs
pair_long <- pair_long[!(duplicated(pair_long)
                         | duplicated(pair_long ,
                                      fromLast = TRUE))]

# [1] 212404129 pairs

######################
## Match short/long pairs
######################

# Finding pairs who have run at least one "short" race and one "long" race
match_pair <- pair_long %>%
  inner_join(keep_pair,
             by = c("M",
                    "W",
                    "annee")) %>%
  dplyr::select("M",
                "W",
                "annee") %>%
  distinct()
# 2611 pairs after final selection

# Reporting selected pairs in "short" race
keep_short <- pair_short %>%
  inner_join(match_pair,
             by = c("M",
                    "W",
                    "annee")) %>%
  dplyr::select("annee",
                "id_course_annee",
                "M",
                "W")

# Reporting selected pairs in "long" race
keep_long <- pair_long %>%
  inner_join(match_pair,
             by = c("M",
                    "W",
                    "annee")) %>%
  dplyr::select("annee",
                "id_course_annee",
                "M",
                "W")


######################
## data formatting
###################### 

# Combining all results 
result_tot <- rbind(keep_short, keep_long)

######################
## Encoding df for MLM
######################

# Drop eventual duplicated pairs
result_tot <- result_tot[!(duplicated(result_tot)
                           | duplicated(result_tot,
                                        fromLast = TRUE))]

# Create a unique id for each pair
result_tot <- result_tot %>%
  group_by(M,
           W,
           annee) %>%
  mutate(id_pair = cur_group_id()) %>%
  ungroup()

# Joining pairs with results dataframe
result_tot <- result_tot %>%
  left_join(df %>% dplyr::select("id_course_annee", "catUTMB") %>% distinct,
            by = "id_course_annee" )

# Separation of pairs 
result_tot_M <- result_tot %>%
  dplyr::select("id_course_annee", id_coureur = "M", "id_pair") %>%
  distinct()
result_tot_F <- result_tot %>%
  dplyr::select("id_course_annee", id_coureur = "W", "id_pair") %>%
  distinct()
dataset <- rbind(result_tot_F, result_tot_M) 

# Save results
fwrite(dataset, "3. MLM_fitting/pair_MLM_60s_unique.csv", row.names = FALSE)




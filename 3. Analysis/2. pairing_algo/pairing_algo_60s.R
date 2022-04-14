#******************************************************************************
## Description .......: Pairs all M/W pair per race and year and calculate speed differences Method 1 / 1bis
##
## Author ............: Franck Le Mat
## Date ..............: 2021-07-01
## License ...........: GPL
##
## Inputs ............: 'DRV_result.csv'
##                      
## Outputs ...........: '60s_pair_results.csv'
##                      '60s_pair_unique_results.csv'
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

km_effort <- c(0,25,45,75,115,155,210,Inf)
df$catUTMB <- cut(df$km_effort, km_effort, right = FALSE, include.lowest = TRUE)


# levels(df$catUTMB) <- c("[0,45)", "[0,45)", "[45,75)", "[75,115)", "[115,155)", "[155,210)", "[210,Inf)")

# keepRace <- c(32541, 45935, 30461, 44845)

# keepRace <- df %>% filter(nom_course == "WAY TOO COOL" | nom_course == "Western States Endurance Run") %>%
#     group_by(annee)%>%
#     summarise(id_course = unique(id_course_annee))
# df_small <- df %>%
#   filter(id_course_annee %in% keepRace$id_course)

df <- df %>%
  filter(catUTMB != "[0,25)") %>%
  group_by(id_course_annee) %>%
  mutate(best_diff = ifelse(sexe =="H", 
                            abs((vitesse_moy - max(vitesse_moy[sexe == "H"])) / max(vitesse_moy[sexe == "H"])), 
                            abs((vitesse_moy - max(vitesse_moy[sexe == "F"])) / max(vitesse_moy[sexe == "F"])))) %>%
  ungroup()

df$catUTMB <- factor(df$catUTMB)

gc(reset = TRUE)

######################
## Create all pairs
######################

library(parallel)

no.cores <- detectCores() -1

cl <- makeCluster(no.cores)

clusterEvalQ(cl, library(parallel))

clusterExport(cl=cl, varlist=c("setDT", "split", "CJ"))

ans <- parLapply(cl, split(setDT(df), by = "catUTMB", sorted = TRUE), function(x) lapply(split(setDT(x), by = "annee"), 
                                                                                  function(x) lapply(split(setDT(x), by = "id_course_annee"), 
                                                                                                     function(x) CJ(M = x[sexe == "H", id_coureur], W = x[sexe == "F", id_coureur]))))
######################
## Create pairs on short cat
######################

result_tot <- data.table()
stats <- data.table()
ptm <- proc.time()

for(i in seq(1,5)){
  gc(reset = TRUE)
 
  short <- levels(df$catUTMB)[i]
  # short <- levels(df$catUTMB)[1]

  pair_short <- data.table()
  pair_short <- rbindlist(lapply(ans[[short]], function(x) rbindlist(x, idcol = "id_course_annee")), idcol = "annee") %>%
    transform(id_course_annee = as.integer(id_course_annee), annee = as.integer(annee))
  pair_short <- pair_short[!(duplicated(pair_short) | duplicated(pair_short , fromLast = TRUE))]

##################
## filter on short cat
##################
  
  # nb_resultat_init <- nrow(pair_short)
  # nb_paires_distinct <- pair_short %>%
  #   select("annee", "W", "M") %>%
  #   n_distinct()

  df_short <- df %>%
    filter(catUTMB == short) %>%
    select("id_coureur","id_course_annee","temps_s","vitesse_moy", "best_diff")
  
##################
## Pair Model 1 -> 60s filter
##################

  pair_short <- pair_short %>%
    inner_join(df_short, by = c("M" = "id_coureur", "id_course_annee")) %>%
    inner_join(df_short, by = c("W" = "id_coureur", "id_course_annee"), suffix = c("_H", "_F")) %>%
    mutate(diff  = (vitesse_moy_H - vitesse_moy_F) / vitesse_moy_F)
    
  # nb_elimine_60s <- pair_short %>%
  #   filter(abs(temps_s_H - temps_s_F) < 60) %>%
  #   n_distinct()
    
  keep_pair <- pair_short %>%
    filter(abs(temps_s_H - temps_s_F) >= 60) %>%
    group_by(W, annee) %>%
    filter(abs(diff) == min(abs(diff))) %>%
    ungroup() %>%
    select("M", "W", "annee") %>%
    distinct()
    
  pair_short <- pair_short %>%
    inner_join(keep_pair, by = c("M", "W", "annee")) %>%
    select("annee", "id_course_annee", "M", "W")
    
  # stats <- rbind(stats, cbind(catUTMB = short,
    #                            nb_resultat_init,
    #                            nb_paires_distinct,
    #                            nb_elimine_60s,
    #                            nb_resultat_final = nrow(pair_short)
    #                            )
    #                )
    
  print('Time to create pairs on', short, 'category')
  print((proc.time() - ptm) /60)
  
  
##################
## create long cat pairs
##################
  
  result <- list()
  
  for(cat in seq(i+1,6)){ 

    long <- levels(df$catUTMB)[cat]
    # long <- levels(df$catUTMB)[3]
    
    pair_long <- data.table()
    pair_long <- rbindlist(lapply(ans[[long]], function(x) rbindlist(x, idcol = "id_course_annee")), idcol = "annee") %>%
      transform(id_course_annee = as.integer(id_course_annee), annee = as.integer(annee))
    pair_long <- pair_long[!(duplicated(pair_long ) | duplicated(pair_long , fromLast = TRUE))]
    
######################
## Match short/long pairs
######################
    
    match_pair <- pair_long %>%
      inner_join(keep_pair, by = c("M", "W", "annee")) %>%
      select("M", "W", "annee") %>%
      distinct()
    
    df_join <- df %>%
      select("id_coureur","id_course_annee", "age", "vitesse_moy")
    
    keep_short <- pair_short %>%
      inner_join(match_pair, by = c("M", "W", "annee")) %>%
      select("annee", "id_course_annee", "M", "W") %>%
      inner_join(df_join, by = c("M" = "id_coureur", "id_course_annee")) %>%
      inner_join(df_join, by = c("W" = "id_coureur", "id_course_annee"), suffix = c("_H", "_F")) %>%
      mutate(diff  = (vitesse_moy_H - vitesse_moy_F) / vitesse_moy_F)
  
    keep_long <- pair_long %>%
      inner_join(match_pair, by = c("M", "W", "annee")) %>%
      select("annee", "id_course_annee", "M", "W") %>%
      inner_join(df_join, by = c("M" = "id_coureur", "id_course_annee")) %>%
      inner_join(df_join, by = c("W" = "id_coureur", "id_course_annee"), suffix = c("_H", "_F")) %>%
      mutate(diff  = (vitesse_moy_H - vitesse_moy_F) / vitesse_moy_F)

    

######################
## stats calculation
###################### 
    
    keep_short <- keep_short %>%
      group_by(M,W, annee) %>%
      summarise(age_H = mean(age_H),
                age_F = mean(age_F),
                vitesse_moy_H = mean(vitesse_moy_H),
                vitesse_moy_F = mean(vitesse_moy_F),
                diff = mean(diff)) %>%
      ungroup()
    short_summary <- keep_short %>%
      summarise(pair = paste(short, "-", long),
                catUTMB = short,
                nb_match = n(),
                nb_H_distinct = length(unique(M)),
                nb_F_distinct = length(unique(W)),
                age_moy_H = mean(age_H),
                age_sd_H = sd(age_H),
                age_moy_F = mean(age_F),
                age_sd_F = sd(age_F),
                vit_moy_H = mean(vitesse_moy_H),
                vit_sd_H = sd(vitesse_moy_H),
                vit_moy_F = mean(vitesse_moy_F),
                vit_sd_F = sd(vitesse_moy_F),
                moy_diff = mean(diff, na.rm = TRUE) * 100,
                sd_diff = sd(diff, na.rm = TRUE) * 100,
                diff_moy = (mean(vitesse_moy_H) - mean(vitesse_moy_F)) / mean(vitesse_moy_F) * 100)


    keep_long <- keep_long %>%
      group_by(M,W, annee) %>%
      summarise(age_H = mean(age_H),
                age_F = mean(age_F),
                vitesse_moy_H = mean(vitesse_moy_H),
                vitesse_moy_F = mean(vitesse_moy_F),
                diff = mean(diff)) %>%
      ungroup()
    long_summary <- keep_long %>%
      summarise(pair = paste(short, "-", long),
                catUTMB = long,
                nb_match = n(),
                nb_H_distinct = length(unique(M)),
                nb_F_distinct = length(unique(W)),
                age_moy_H = mean(age_H),
                age_sd_H = sd(age_H),
                age_moy_F = mean(age_F),
                age_sd_F = sd(age_F),
                vit_moy_H = mean(vitesse_moy_H),
                vit_sd_H = sd(vitesse_moy_H),
                vit_moy_F = mean(vitesse_moy_F),
                vit_sd_F = sd(vitesse_moy_F),
                moy_diff = mean(diff, na.rm = TRUE) * 100,
                sd_diff = sd(diff, na.rm = TRUE) * 100,
                diff_moy = (mean(vitesse_moy_H) - mean(vitesse_moy_F)) / mean(vitesse_moy_F) * 100)
    
    res <- t.test(keep_short$diff, keep_long$diff, paired = TRUE)

    result <- cbind(rbind(short_summary, long_summary), p.value = res$p.value, conf.int.down = res$conf.int[1], conf.int.up = res$conf.int[2])

    result_tot <- rbind(result_tot, result)

    print('Time du match pairs from', paste(short, "vs", long))
    print((proc.time() - ptm) /60)
  }

}

result_tot <- result_tot %>%
  mutate(id = row_number())

# fwrite(result_tot, 'pairing_algo/60s_unique_pair_results.csv', row.names = FALSE)
# fwrite(result_tot, 'pairing_algo/60s_pair_results.csv', row.names = FALSE)


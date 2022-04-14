#******************************************************************************
## Description .......: graph book Gui
##
## Author ............: Franck Le Mat
## Date ..............: 2021-09-21
## License ...........: GPL
##
##                        
#******************************************************************************


######################
## Script initialisation
######################

## First specify the packages of interest
packages = c("tidyverse", 
             "ggthemes", 
             "ggpubr", 
             "scales", 
             "RColorBrewer", 
             "ggsci", 
             "data.table", 
             "rworldmap",
             "gridExtra")


## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

theme_set(theme_fivethirtyeight())

setwd("~/Project _dataScience/UTMB_dataMining/3. Analysis/graph_Gui/")

info_course <- fread('DRV_info_course.csv', stringsAsFactors = TRUE)
df <- fread('DRV_result_UTMB.csv', stringsAsFactors = TRUE)

coureur <- fread('RAW_coureur.csv')
coureur <- coureur[,c('id', 'nom', 'prenom')]

df_UTMB <- fread('info_course_UTMB.csv', stringsAsFactors = TRUE)

rslt_UTMB <- df %>%
  filter(nom_course == 'UTMB®')

rslt__UTMB <- rslt_UTMB %>%
  group_by(id_course_annee) %>%
  mutate(norm_vit = ifelse(sexe == "H", 
                            (vitesse_moy / max(vitesse_moy[sexe == "H"])),
                            (vitesse_moy / max(vitesse_moy[sexe == "F"])))) %>%
  ungroup()

max(rslt_UTMB$age, na.rm = TRUE)
age <- c(20,25,30,35,40,45,50,55,60, 65, 70, 75)
rslt__UTMB$catAGE <- cut(rslt__UTMB$age, age, right = FALSE, include.lowest = TRUE)

tab_top3 <- rslt__UTMB %>%
  group_by(id_course_annee, sexe) %>%
  slice_max(norm_vit, n = 3) %>% 
  left_join(coureur, by = c('id_coureur' = 'id'))
fwrite(tab_top3, file='perf_top3.csv')
rslt__UTMB %>%
  group_by(id_course_annee, sexe) %>%
  slice_max(norm_vit, n = 3) %>% filter(annee %in% c(2015, 2016, 2018, 2019)) %>%
  ggplot(aes(x = as.factor(catAGE), y = norm_vit, color = sexe)) +
  stat_summary(geom = "pointrange") + 
  stat_summary(geom = 'line', fun = 'mean', aes(group = sexe))  +
  labs(y = 'Coefficient de performance', x = 'Age (année)', color = NULL, title = 'Perf UTMB 2015-2019 TOP 3') + 
  scale_color_manual(values = c('orange', 'steelblue'), labels = c('Femmes', 'Hommes')) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_classic(base_size = 22)

rslt__UTMB %>%
  group_by(id_course_annee, sexe) %>%
  slice_max(norm_vit, n = 3) %>% 
  ggplot(aes(x = as.factor(annee), y = vitesse_moy, color = sexe)) +
  stat_summary(geom = "pointrange") + 
  stat_summary(geom = 'line', fun = 'mean', aes(group = sexe)) + 
  labs(x = 'Année', y = 'Vitesse moyenne', color = NULL, title = 'Perf UTMB Top 3') + 
  scale_color_manual(values = c('orange', 'steelblue'), labels = c('Femmes', 'Hommes')) + 
  theme_classic(base_size = 22)



?cut
ggplot(rslt__UTMB, aes(x = as.factor(annee), y = age, color = sexe)) +
  stat_summary(geom = "pointrange") + 
  stat_summary(geom = 'line', fun = 'mean', aes(group = sexe)) + 
  labs(x = 'Année', y = 'Age (année)', color = NULL, title = 'Age moyenne coureur UTMB') + 
  scale_color_manual(values = c('orange', 'steelblue'), labels = c('Femmes', 'Hommes')) + 
  theme_classic(base_size = 22)

rslt__UTMB %>% filter(annee %in% c(2015, 2016, 2018, 2019)) %>%
ggplot(aes(x = as.factor(catAGE), y = norm_vit, color = sexe)) +
  stat_summary(geom = "pointrange") + 
  stat_summary(geom = 'line', fun = 'mean', aes(group = sexe))  +
  labs(y = 'Coefficient de performance', x = 'Age (année)', color = NULL, title = 'Perf UTMB 2015-2019') + 
  scale_color_manual(values = c('orange', 'steelblue'), labels = c('Femmes', 'Hommes')) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_classic(base_size = 22)

rslt__UTMB %>% 
  filter(annee %in% c(2015, 2016, 2018, 2019)) %>%
  group_by(catAGE, sexe) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  mutate(freq = cnt / sum(cnt)) %>%
  ggplot(aes(x = as.factor(catAGE), y = freq, fill = sexe)) +
  geom_bar(stat = 'identity', position = position_dodge()) + 
  labs(y = 'Répartition (%)', x = 'Age (année)',fill = NULL, title = 'Répartition age UTMB 2015-2019') + 
  scale_fill_manual(values = c('orange', 'steelblue'), labels = c('Femmes', 'Hommes')) + 
  scale_y_continuous(breaks = pretty_breaks(), labels = scales::percent) +
  theme_classic(base_size = 22)



info_course %>%
  ggplot(aes(x = dist_final, y = rate_F)) +
  geom_point(position = 'jitter', size = 1, alpha = .5) +
  geom_smooth() +
  labs(title = "All UTMB DB trail race from 2010 to 2019") +
  scale_x_continuous('Race distance (km)', breaks = breaks_width(50), limits = c(0,400)) +
  scale_y_continuous('Female participation (%)', breaks = breaks_width(0.1), labels = label_percent(accuracy = 1)) +
  theme_classic()

df_UTMB %>%
  ggplot(aes(x = dist_final, y = rate_F)) +
  geom_point(position = 'jitter', size = 4, aes(color = nom_course)) +
  geom_smooth(alpha = .2) +
  labs(title = "UTMB races all recorded time") +
  scale_x_continuous('Race distance (km)', breaks = breaks_width(10)) +
  scale_y_continuous('Female participation (%)', breaks = breaks_width(0.02), labels = label_percent(accuracy = 1)) +
  scale_color_jco() +
  theme_classic() + 
  theme(legend.title = element_blank()) 

info_course %>% 
  filter(annee %in% c(2017:2019)) %>%
  ggplot() +
  geom_point(aes(x = dist_final, y = rate_DNF_M), position = 'jitter', size = 1, alpha = .5, color = 'blue') +
  geom_point(aes(x = dist_final, y = rate_DNF_W), position = 'jitter', size = 1, alpha = .5, color = 'red') +
  geom_smooth(aes(x = dist_final, y = rate_DNF_M), color = 'blue') +
  geom_smooth(aes(x = dist_final, y = rate_DNF_W), color = 'red') +
  labs(title = "DNF percentage for race from 2017 to 2019") +
  scale_x_continuous('Race distance (km)', breaks = breaks_width(50), limits = c(0,400)) +
  scale_y_continuous('DNF rate (%)', breaks = breaks_width(0.1), labels = label_percent(accuracy = 1)) +
  theme_classic()

df_UTMB %>% 
  ggplot() +
  geom_smooth(aes(x = dist_final, y = rate_DNF_M), color = 'blue', fill = 'blue', alpha = .2) +
  geom_smooth(aes(x = dist_final, y = rate_DNF_W), color = 'red', fill = 'red', alpha = .2) +
  geom_point(aes(x = dist_final, y = rate_DNF_M), position = 'jitter', size = 4, alpha = .5, color = 'blue') +
  geom_point(aes(x = dist_final, y = rate_DNF_W), position = 'jitter', size = 4, alpha = .5, color = 'red') +
  labs(title = "DNF percentage for race from 2017 to 2019 for UTMB race") +
  scale_x_continuous('Race distance (km)', breaks = breaks_width(50)) +
  scale_y_continuous('DNF rate (%)', breaks = breaks_width(0.1), labels = label_percent(accuracy = 1)) +
  theme_classic()

df_UTMB %>%
  filter()

df_UTMB %>%
  ggplot(aes(x = as.factor(annee), y = rate_F)) +
  geom_line(aes(group = nom_course, color = nom_course), size = 1.5) +
  labs(x = NULL)  + 
  scale_y_continuous('Female participation (%)', breaks = breaks_width(0.02), labels = label_percent(accuracy = 1)) +
  scale_color_jco() +
  theme_classic() + 
  theme(legend.title = element_blank(), legend.position = c(0.10, 0.87)) 

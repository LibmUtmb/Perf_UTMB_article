#******************************************************************************
## Description .......: descriptive analysis & visualization  for Fred
##
## Author ............: Franck Le Mat
## Date ..............: 2021-10-01
## License ...........: GPL
##
## Inputs ............: 'DRV_info_course.csv'
##                      'DRV_result.csv'     
##                      
## Outputs ...........: visualisation
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
             "rworldmap")

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

setwd("~/Project _dataScience/UTMB_dataMining/3. Analysis/")

info_course <- fread('DRV_info_course.csv', stringsAsFactors = TRUE)
df <- fread('DRV_result.csv', stringsAsFactors = TRUE)

km_effort <- c(0,25,45,75,115,155,210,Inf)
df$catUTMB <- cut(df$km_effort, km_effort, right = FALSE, include.lowest = TRUE)
info_course$catUTMB <- cut(info_course$km_effort, km_effort, right = FALSE, include.lowest = TRUE)

############# Graph Fred

df_H_best <- df %>%
  group_by(id_course_annee) %>%
  filter(sexe == "H" & temps_s == min(temps_s))


ggplot(df_H_best %>% filter(cote > 900), aes(x = km_effort, y = temps_s/3600)) + 
  geom_point(alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0,300)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  xlab("Distance (km effort)") +
  ylab("Duration (hours)") +
  theme_classic(base_size = 11)
ggsave("graph_Fred/Duration vs Distance 300_more 900.png")

ggplot(df_H_best %>% filter(cote > 900), aes(x = km_effort, y = temps_s/3600)) + 
  geom_point(alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0,100)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, 8)) +
  xlab("Distance (km effort)") +
  ylab("Duration (hours)") +
  theme_classic(base_size = 11)
ggsave("graph_Fred/Duration vs Distance 100_more 900.png")

ggplot(df_H_best %>% filter(cote > 900), aes(x = km_effort, y = temps_s/3600)) + 
  geom_point(alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0,200)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, 18)) +
  xlab("Distance (km effort)") +
  ylab("Duration (hours)") +
  theme_classic(base_size = 11)
ggsave("graph_Fred/Duration vs Distance 200_more 900.png")

ggplot(df_H_best %>% filter(cote > 900), aes(x = km_effort, y = km_effort /(temps_s/3600))) + 
  geom_point(alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0,300)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  xlab("Distance (km effort)") +
  ylab("Speed (km effort / hour)") +
  geom_smooth() +
  theme_classic(base_size = 11)
ggsave("graph_Fred/Speed vs Distance 300_more 900.png")

ggplot(df_H_best %>% filter(cote > 900), aes(x = km_effort, y = km_effort /(temps_s/3600))) + 
  geom_point(alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0,200)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  xlab("Distance (km effort)") +
  ylab("Speed (km effort / hour)") +
  geom_smooth() +
  theme_classic(base_size = 11)
ggsave("graph_Fred/Speed vs Distance 200_more 900.png")

ggplot(df_H_best %>% filter(cote > 900), aes(x = km_effort, y = km_effort /(temps_s/3600))) + 
  geom_point(alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(25,100)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  xlab("Distance (km effort)") +
  ylab("Speed (km effort / hour)") +
  geom_smooth() +
  theme_classic(base_size = 11)
ggsave("graph_Fred/Speed vs Distance 100_more 900.png")


ggplot(df_H_best %>% filter(cote > 900), aes(x = temps_s/3600, y = km_effort /(temps_s/3600))) + 
  geom_point(alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +
  xlab("Duration (hours)") +
  ylab("Speed (km effort / hour)") +
  geom_smooth() +
  theme_classic(base_size = 11)
ggsave("graph_Fred/Duration vs Speed_more 900.png")

ggplot(df_H_best %>% filter(cote > 900), aes(x = temps_s/3600, y = km_effort /(temps_s/3600))) + 
  geom_point(alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, 25)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  xlab("Duration (hours)") +
  ylab("Speed (km effort / hour)") +
  geom_smooth() +
  theme_classic(base_size = 11)
ggsave("graph_Fred/Duration vs Speed 50_more 900.png")

df_H_best$cat_cote <- cut(df_H_best$cote, seq(0,1000, by = 50), right = FALSE, include.lowest = TRUE)

ggplot(df_H_best %>% filter(cote > 750 & cote < 950), aes(x = temps_s/3600, y = km_effort /(temps_s/3600))) + 
  geom_point(aes(color = cat_cote), alpha = .5, size = 2) + 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, 25)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  xlab("Duration (hours)") +
  ylab("Speed (km effort / hour)") +
  geom_smooth() +
  theme_classic(base_size = 11) + 
  facet_wrap(~cat_cote)
ggsave("graph_Fred/Duration vs Speed ITRA score.png")
#******************************************************************************
## Description .......: Pairs all M/W pair per cote 
##                      
##
## Author ............: Franck Le Mat
## Date ..............: 2021-10-14
## License ...........: GPL
##
## Inputs ............: 'DRV_result.csv'
##                      
## Outputs ...........: 'x_pair.csv' 
##                    
#******************************************************************************

library(tidyverse)
library(data.table)
library(funModeling)
library(pander)
library(scales)
library(lme4)
library(car)
library(scattermore)
library(fitdistrplus)
library(mgcv)
library(MASS)
library(ggsci)

memory.limit(size = 999999999) #Increasing the memory use of Rstudio otherwise it take to much time (Windows only)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/3. Analysis/")
race_result <- fread('DRV_result.csv')
df_race <- fread('DRV_info_course.csv')
df_race_info <- fread('WRK_info_course.csv')

df_race_info <- df_race_info %>%
  dplyr::select("id_course_annee", "pct_route", "pct_piste", "pct_sentier", "nb_ravito", "alt_min", "alt_max", "longitude", "latitude")
df_race <- inner_join(df_race, df_race_info, by = "id_course_annee")

df_race <- df_race[complete.cases(df_race), ]

record <- data.table(dist = c(5, 10, 21.1, 25, 42.2, 100, 290.221), time_s = c(771, 1584, 3452,  4278, 7294, 22154, 86400))
record <- record %>% 
  mutate(speed = dist / (time_s/3600))
  
record %>% ggplot(aes(y = speed, x = dist)) +
  geom_point() +
  scale_x_continuous(breaks = breaks_width(10)) +
  geom_smooth() +
  theme_classic()

#fit polynomial regression models up to degree 5
fit1 <- lm(speed~dist, data=record)
fit2 <- lm(speed~poly(dist,2,raw=TRUE), data=record)
fit3 <- lm(speed~poly(dist,3,raw=TRUE), data=record)
fit4 <- lm(speed~poly(dist,4,raw=TRUE), data=record)
fit5 <- lm(speed~poly(dist,5,raw=TRUE), data=record)
fit6 <- lm(speed~log(dist), data=record)

#create a scatterplot of x vs. y
plot(record$dist, record$speed, pch=19, xlab='Distance (km)', ylab='Speed (km/h)')

#define x-axis values

x_axis <- seq(1, 300, by = 1)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(dist=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(dist=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(dist=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(dist=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(dist=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(dist=x_axis)), col='pink')

#calculated adjusted R-squared of each model
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)


#create a scatterplot of x vs. y
plot(record$dist, record$speed, pch=19, xlab='Distance (km)', ylab='Speed (km/h)')

#define x-axis values
x_axis <- seq(1, 800, by = 10)

#add curve of each model to plot

lines(x_axis, predict(fit6, data.frame(dist=x_axis)), col='red')

race_result <- race_result %>%
  mutate(km_effort_plus = dist_final + deniv_final/100 + deniv_neg_final/300,
         vbest_vitesse = 29.5621 - 2.8369*log(km_effort_plus),
         calc_cote = (vitesse_moy / (vbest_vitesse * .9)) *1000,
         id_coureur = as.factor(id_coureur))
df_race <- df_race %>%
  mutate(vitB = dist_final /(temps_best /3600),
         vitB_vir = 29.5621 - 2.8369*log(dist_final),
         log_dist = log(dist_final))

ggplot(data = df_race, aes(y = vitB, x = log(km_effort))) +
  geom_point(alpha = .5, size = 3) + 
  geom_function(fun = function(x) 29.5621 - 2.8369*log(x), color = "blue") +
  scale_x_continuous(breaks =  breaks_pretty()) +
  scale_y_continuous(breaks = breaks_pretty()) +
  geom_smooth() +
  labs(y = "Vitesse (km/h)", x = "Distance (km)") 
  theme_classic(base_size = 22)
  
ggplot(data = df_race, aes(y = vitB, x = (dist_final + deniv_final/100 + deniv_neg_final/300))) +
  geom_point(alpha = .5, size = 3) + 
  geom_function(fun = function(x) 29.5621 - 2.8369*log(x), color = "blue") +
  scale_x_continuous(breaks =  breaks_pretty()) +
  scale_y_continuous(breaks = breaks_pretty()) +
  labs(y = "Vitesse (km/h)", x = "Distance (km)") 
theme_classic(base_size = 22)
png('avg_speedMvsF.png', width = 1024, height = 768)
print(p1) 
dev.off()

race_result <- race_result %>% filter(is.na(cote) == FALSE)

race_result <- race_result %>%
  mutate(km_effort_plus = dist_final + deniv_final/100 + deniv_neg_final/300,
         vbest_vitesse = 29.5621 - 2.8369*log(km_effort_plus),
         calc_cote = (vitesse_moy / (vbest_vitesse * .9)) *1000,
         id_coureur = as.factor(id_coureur))

  
descdist(race_result$cote, discrete = FALSE)  
plot(fitdist(race_result$cote, distr="lnorm")) 

library(caTools)
set.seed(123)

race_result_small = race_result[race_result$id_course_annee %in% sample.int(race_result$id_course_annee, 10000),]
race_result_small$id_course_annee = as.factor(race_result_small$id_course_annee)

`%!in%` <- Negate(`%in%`)

split = sample.int(race_result_small$id_course_annee, 8000)
training_set = race_result_small[race_result_small$id_course_annee %in% split,]
training_set$id_course_annee = as.factor(training_set$id_course_annee)

test_set = race_result_small[race_result_small$id_course_annee %!in% split,]
test_set$id_course_annee = as.factor(test_set$id_course_annee)

split = sample.split(df_race$id_course_annee, SplitRatio = 0.8)
training_set = subset(df_race, split == TRUE)
test_set = subset(df_race, split == FALSE)

ggplot(df_race, aes(x = alt_min, y = vitB)) +
  geom_scattermore()

x <- df_race %>%
      dplyr::select("log_dist", "deniv_final", "pct_route", "pct_piste", "pct_sentier", "nb_ravito", "alt_min", "alt_max", "longitude", "latitude") %>%
      mutate(deniv_final = log(deniv_final))
x <- as.matrix(x)

y <- df_race$vitB


model1 <- lm(vitB ~ log(dist_final) + log(deniv_final)  , data = df_race)
model2 <- lmer(cote ~ calc_cote + (1 + calc_cote|id_course_annee) , data = training_set, REML = F)
model2_bis <- lmer(cote ~ vitesse_moy + (1 + vitesse_moy |id_course_annee) , data = training_set, REML = F)
model3 <- lmer(cote ~ calc_cote + (0 + calc_cote|id_course_annee) , data = training_set, REML = F)

summary(model1)
vif(model1)

coef(model2)

coef(model2_bis)
AIC(model2, model2_bis)
plot(model2)

summary(model2_bis)
test_set$y_pred <- round(predict(model2, newdata = test_set, allow.new.levels = TRUE), 2)
plot((test_set$cote - test_set$y_pred), test_set$cote)
sqrt(mean((test_set$cote - test_set$y_pred)^2, na.rm = TRUE))

log(df_race$dist_final)

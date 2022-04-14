#******************************************************************************
## Description .......: EDA and MLM model fitting and testing on selected pairs
##
## Author ............: Franck Le Mat
## Date ..............: 2021-18-11
## License ...........: GPL
##
## Inputs ............: 'pair_MLM_40%_unique'
##                      'pair_MLM_60s_unique'
##                      
## Outputs ...........: Data table and Visualization
##                    
#******************************************************************************


library(tidyverse)
library(data.table)
library(funModeling)
library(pander)
library(scales)
library(lme4)
library(car)
library(fitdistrplus)
library(mgcv)
library(MASS)
library(ggsci)
library(caTools)
library(glmmTMB)
library(Metrics)
library(MuMIn)
library(ggpubr)

######################
## Dataset load 
######################

setwd("~/Project_dataScience/UTMB_dataMining/3. Analysis/")
race_result <- fread('DRV_result.csv')

select_coureur_relatif <- fread('3. MLM_fitting/pair_MLM_4%_unique.csv')
select_coureur_absolue <- fread('3. MLM_fitting/pair_MLM_60s_unique.csv')

######################
## Variable encoding
######################

df_relatif <- inner_join(race_result, select_coureur_relatif, by = c("id_course_annee", "id_coureur"))

df_relatif <- df_relatif %>%
  group_by(id_coureur,annee) %>%
  mutate(id_coureur_annee = cur_group_id()) %>%
  ungroup() %>%
  transform(id_coureur_annee = as.factor(id_coureur_annee),
            id_course_annee = as.factor(id_course_annee),
            id_pair = as.factor(id_pair),
            sexe = as.factor(sexe))

df_relatif <- df_relatif %>%
  filter((id_pair != 639) & (id_pair != 214))

df_absolue <- inner_join(race_result, select_coureur_absolue, by = c("id_course_annee", "id_coureur"))

df_absolue <- df_absolue %>%
  group_by(id_coureur,annee) %>%
  mutate(id_coureur_annee = cur_group_id()) %>%
  ungroup() %>%
  transform(id_coureur_annee = as.factor(id_coureur_annee),
            id_course_annee = as.factor(id_course_annee),
            id_pair = as.factor(id_pair),
            sexe = as.factor(sexe))

######################
## Data distribution visualization
######################

plotdist(df_relatif$vitesse_moy)
# dev.copy(png,'3. MLM_fitting/model_fitting_viz/vit_distribution_rel.png',
#          height = 619,
#          width = 1089)
# dev.off()

plotdist(df_absolue$vitesse_moy)
# dev.copy(png,'3. MLM_fitting/model_fitting_viz/vit_distribution_abs.png',
#          height = 619,
#          width = 1089)
# dev.off()


descdist(df_relatif$vitesse_moy, boot = 100)
# dev.copy(png,'3. MLM_fitting/model_fitting_viz/skewvskurt_rel.png',
#          height = 619,
#          width = 1089)
# dev.off()

descdist(df_absolue$vitesse_moy, boot = 100) 
# dev.copy(png,'3. MLM_fitting/model_fitting_viz/skewvskurt_abs.png',
#          height = 619,
#          width = 1089)
# dev.off()

dist_plot <- function(data){
  fg <- fitdist(data,"gamma")
  fln <- fitdist(data,"lnorm")
  fn <- fitdist(data,"norm")
  par(mfrow=c(2, 2))
  denscomp(list(fn,fln,fg), legendtext=c("normal", "lognormal", "gamma"))
  qqcomp(list(fn,fln,fg), legendtext=c("normal", "lognormal", "gamma"))
  cdfcomp(list(fn,fln,fg), legendtext=c("normal", "lognormal", "gamma"))
  ppcomp(list(fn,fln,fg), legendtext=c("normal", "lognormal", "gamma"))
}

dist_plot(df_relatif$vitesse_moy)
# dev.copy(png,'3. MLM_fitting/model_fitting_viz/dist_rel_comparison.png',
#          height = 619,
#          width = 1089)
# dev.off()

dist_plot(df_absolue$vitesse_moy)
# dev.copy(png,'3. MLM_fitting/model_fitting_viz/dist_abs_comparison.png',
#          height = 619,
#          width = 1089)
# dev.off()

######################
## Data viz
######################

p1 <- ggplot(df_relatif, aes(x = km_effort, y = vitesse_moy)) +
  geom_point(alpha = .5, size = .5) +
  scale_x_continuous(breaks =  breaks_pretty()) +
  scale_y_continuous(breaks = breaks_pretty()) +
  scale_color_lancet()+
  labs(y = "Speed (km/h)", x = "km effort (a.u.)", title = "Pair selection using relative speed") +
  theme_classic()
p1

p2 <- ggplot(df_absolue, aes(x = km_effort, y = vitesse_moy)) +
  geom_point(alpha = .5, size = .5) +
  scale_x_continuous(breaks =  breaks_pretty()) +
  scale_y_continuous(breaks = breaks_pretty()) +
  scale_color_lancet()+
  labs(y = "Speed (km/h)", x = "km effort (a.u.)", title = "Pair selection using absolute speed") +
  theme_classic()

print(ggarrange(p1, p2, ncol = 1, align = "hv"))
ggsave('3. MLM_fitting/model_fitting_viz/Speed_kmEffort_scatterplot.png')

######################
## Class variable creation
######################

class_building <- function(x){
  df_rank <- x %>%
    filter(km_effort >= 25 & km_effort <= 45) %>%
    group_by(id_pair) %>%
    summarise(vitesse_moy_kmE = mean(km_effort)/mean(temps_s/3600), 
              vitesse_moy = mean(vitesse_moy), 
              km_effort = mean(km_effort))
  
  l_model <- lm(vitesse_moy_kmE ~ km_effort, data = df_rank)
  
  rank <- df_rank %>% 
    mutate(vitesse_moy_adj = (l_model$coefficients[2] * (km_effort - min(km_effort))) + vitesse_moy_kmE)
  
  rank <- rank %>%
    mutate(class = cut(x = vitesse_moy_adj, breaks = quantile(rank$vitesse_moy_adj, probs = seq(0, 1, 1/4)), labels = c("Q4", "Q3", "Q2", "Q1"), include.lowest = TRUE)) %>%
    dplyr::select("id_pair", "class") %>%
    distinct()
  
  x <- x %>%
    inner_join(rank, by = "id_pair")
  return(x)
}

df_rel <- class_building(df_relatif)
df_abs <- class_building(df_absolue)

######################
## Exploratory analysis
######################

abs_description <- df_abs %>%
  group_by(sexe) %>%
  summarise(nbr_runner = n(), 
            mean_speed = paste(round(mean(vitesse_moy, na.rm = TRUE), 2), '+/-', round(sd(vitesse_moy), 2)), 
            median_rank_scratch = median(place, na.rm = TRUE), 
            mean_cote = paste(round(mean(cote, na.rm = TRUE), 2), '+/-', round(sd(cote, na.rm = TRUE), 2)), 
            age = paste(round(mean(age, na.rm = TRUE), 2), '+/-', round(sd(age), 2)), 
            .groups = 'drop')
pander(abs_description, caption = "Absolute selection dataset description")


relat_description <- df_rel %>%
  group_by(sexe) %>%
  summarise(nbr_runner = n(), 
            mean_speed = paste(round(mean(vitesse_moy, na.rm = TRUE), 2), '+/-', round(sd(vitesse_moy), 2)), 
            median_rank_scratch = median(place, na.rm = TRUE), 
            mean_cote = paste(round(mean(cote, na.rm = TRUE), 2), '+/-', round(sd(cote, na.rm = TRUE), 2)), 
            age = paste(round(mean(age, na.rm = TRUE), 2), '+/-', round(sd(age), 2)), 
            .groups = 'drop')

pander(relat_description, caption = "Relative selection dataset description")


######################
## Random effect selection 
######################

data <- df_rel

gamma_glmm1 <- glmmTMB(vitesse_moy ~ km_effort,
                       data = data, 
                       family=Gamma(link="log"), 
                       control = glmmTMBControl(parallel = 7))

gamma_glmm2 <- glmmTMB(vitesse_moy ~ poly(km_effort, 2),
                       data = data, 
                       family=Gamma(link="log"), 
                       control = glmmTMBControl(parallel = 7))
summary(gamma_glmm1)
summary(gamma_glmm2)
AIC(gamma_glmm1, gamma_glmm2)
  
gamma_glmm1 <- glmmTMB(vitesse_moy ~ km_effort*sexe*class  
                       + (1|id_coureur_annee),
                       data = data, 
                       family=Gamma(link="log"), 
                       control = glmmTMBControl(parallel = 7))

gamma_glmm2 <- glmmTMB(vitesse_moy ~ km_effort*sexe*class  
                       + (1|id_course_annee),
                       data = data, 
                       family=Gamma(link="log"), 
                       control = glmmTMBControl(parallel = 7))

gamma_glmm3 <- glmmTMB(vitesse_moy ~ km_effort*sexe*class  
                       + (1|id_coureur_annee) 
                       + (1|id_course_annee),
                       data = data, 
                       family=Gamma(link="log"), 
                       control = glmmTMBControl(parallel = 7))

gamma_glmm4 <- glmmTMB(vitesse_moy ~ poly(km_effort, 2)*sexe*class
                       + (1|id_coureur_annee) 
                       + (1|id_pair) 
                       + (1|id_course_annee),
                       data = data, 
                       family=Gamma(link="log"), 
                       control = glmmTMBControl(parallel = 7))

gamma_glmm4bis <- glmmTMB(vitesse_moy ~ poly(km_effort, 3)*sexe*class
                       + (1|id_coureur_annee) 
                       + (1|id_pair) 
                       + (1|id_course_annee),
                       data = data, 
                       family=Gamma(link="log"), 
                       control = glmmTMBControl(parallel = 7))

gamma_glmm4ter <- glmmTMB(vitesse_moy ~ poly(km_effort, 4)*sexe*class
                          + (1|id_coureur_annee) 
                          + (1|id_pair) 
                          + (1|id_course_annee),
                          data = data, 
                          family=Gamma(link="log"), 
                          control = glmmTMBControl(parallel = 7))

gamma_glmm4.4 <- glmmTMB(vitesse_moy ~ poly(km_effort, 5)*sexe*class
                          + (1|id_coureur_annee) 
                          + (1|id_pair) 
                          + (1|id_course_annee),
                          data = data, 
                          family=Gamma(link="log"), 
                          control = glmmTMBControl(parallel = 7))

AIC(gamma_glmm4ter, gamma_glmm4.4)


gamma_glmm5 <- glmmTMB(vitesse_moy ~ km_effort*sexe*class  
                       + (1|id_pair) 
                       + (1|id_course_annee),
                       data = data, 
                       family=Gamma(link="log"), 
                       control = glmmTMBControl(parallel = 7))

AIC(gamma_glmm1, gamma_glmm2, gamma_glmm3, gamma_glmm4, gamma_glmm5)

# The model with the lowest AIC is gamma_glmm4
summary(gamma_glmm4.4)


######################
## Fix effect selection 
######################

library(parallel)

clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 7), type = clusterType))

clusterExport(clust, c("glmmTMB", 'glmmTMBControl', 'data'))
invisible(clusterCall(clust, "library", "stats4", character.only = TRUE))

options(na.action = "na.fail")
model_dredge<- MuMIn::pdredge(cluster = clust, global.model = gamma_glmm4, rank = "AIC")

stopCluster(clust)

print(model_dredge)

######################
## Best model fitting and evaluation 
######################
model <- gamma_glmm4.4
model <- glmmTMB(vitesse_moy ~ km_effort*sexe*class  
                 + (1|id_coureur_annee) 
                 + (1|id_pair) 
                 + (1|id_course_annee),
                 data = df_rel, 
                 family=Gamma(link="log"), 
                 control = glmmTMBControl(parallel = 7))
summary(model)

library(parallel)

clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 7), type = clusterType))

clusterExport(clust, c("glmmTMB", 'glmmTMBControl', 'data'))
invisible(clusterCall(clust, "library", "stats4", character.only = TRUE))

model_profil <- profile(model, 
        parallel = 'multicore',
        cl = clust)

stopCluster(clust)

confint(model_profil)

ggplot(model_profil,aes(.focal,sqrt(value))) +
  geom_point() + geom_line()+
  facet_wrap(~.par,scale="free_x")+
  geom_hline(yintercept=1.96,linetype=2)

library(simr)
# power analysis with simr for effect "sex"
power <- powerSim(model, test = fixed("sexe"), nsim = 100)
power
summary(model)

power <- powerCurve(model, test = fixed("sexe"), along = "id_pair", breaks = c(1000, 2000), nsim = 2)
power
library(mixedpower)

fixed_effects <- c("km_effort", "sexe",  "class")

power <- R2power(model = model, data = df_rel,
                 fixed_effects, simvar = "id_pair",
                 steps = c(1000, 2000,3260),
                 critical_value = 2, n_sim = 2,
                 R2var = "id_course_annee", R2level = 3143)

library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
simulationOutput$scaledResiduals
plot(simulationOutput)
testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput)

re <- ranef(model)$cond$id_coureur_annee
qqnorm(re$`(Intercept)`)
qqline(re$`(Intercept)`)

re <- ranef(model)$cond$id_pair
qqnorm(re$`(Intercept)`)
qqline(re$`(Intercept)`)

re <- ranef(model)$cond$id_course_annee
qqnorm(re$`(Intercept)`)
qqline(re$`(Intercept)`)

r.squaredGLMM(model)

library(ggeffects)
mydf <- ggpredict(model, terms = c("km_effort [all]", "sexe"))

ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(colour = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = .1,
              show.legend=FALSE) + 
  labs(
    y = "Speed (km/h)",
    x = "km effort (a.u.)",
    colour = "Gender",
    title = "Marginal effect of sexe and km effort on speed"
  ) +
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_fill_lancet() +
  scale_color_lancet() +
  theme_classic() 


mydf2 <- ggpredict(model, terms = c("km_effort [all]", "sexe", "class"))
ggplot(mydf2, aes(x, predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) + 
  labs(
    y = get_y_title(mydf),
    x = get_x_title(mydf),
    colour = get_legend_title(mydf)
  ) +
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(breaks = pretty_breaks()) + 
  facet_wrap(~facet)

# Equation calculation 

#Q4

a1 = model$fit$par[2]
b1 = model$fit$par[1]
a2 = model$fit$par[7] 
b2 = model$fit$par[3]

kme = 100
exp(b1 + a1*kme) * (1-exp(b2+a2*kme))

q=0

a1 = model$fit$par[10] + model$fit$par[2]
b1 = model$fit$par[1] + model$fit$par[6]
a2 = model$fit$par[7] + model$fit$par[16]
b2 = model$fit$par[10] + model$fit$par[2]

exp()

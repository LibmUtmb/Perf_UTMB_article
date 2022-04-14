#******************************************************************************
## Description .......: MLM model selection and testing on full dataset
##
## Author ............: Franck Le Mat
## Date ..............: 2021-09-11
## License ...........: GPL
##
## Inputs ............: 'DRV_result.csv'
##                      
## Outputs ...........: Visualization
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
library(caTools)
library(glmmTMB)
library(caret)
library(Metrics)
library(MuMIn)

memory.limit(size = 999999999) #Increasing the memory use of Rstudio otherwise it take to much time (Windows only)

######################
## Dataset load 
######################

setwd("~/Project_dataScience/UTMB_dataMining/3. Analysis/")
race_result <- fread('DRV_result.csv')

######################
## Data viz on full results
######################

p1 <- ggplot(race_result, aes(x = km_effort,
                              y = vitesse_moy)) +
  geom_scattermore(alpha = .5) +
  scale_x_continuous(breaks =  breaks_pretty()) +
  scale_y_continuous(breaks = breaks_pretty()) +
  scale_color_lancet()+
  labs(y = "Vitesse (km/h)",
       x = "km_effort") +
  geom_smooth(aes(color = sexe)) +
  theme_classic()
print(p1) 
# ggsave('3. MLM_fitting/model_selection_viz/avg_speedMvsF.png')

######################
## Distribution evaluation of vitesse_moy variable
######################

plotdist(race_result$vitesse_moy)
# dev.copy(png,'3. MLM_fitting/model_selection_viz/vit_distribution.png',
#          height = 619,
#          width = 1089)
# dev.off()

descdist(race_result$vitesse_moy, boot = 100)
# dev.copy(png,'3. MLM_fitting/model_selection_viz/vit_distribution_skewvskurt.png',
#          height = 619,
#          width = 1089)
# dev.off()


fg <- fitdist(race_result$vitesse_moy,"gamma")
fln <- fitdist(race_result$vitesse_moy,"lnorm")
fn <- fitdist(race_result$vitesse_moy,"norm")
par(mfrow=c(2, 2))
denscomp(list(fn,fln,fg), legendtext=c("normal", "lognormal", "gamma"))
qqcomp(list(fn,fln,fg), legendtext=c("normal", "lognormal", "gamma"))
cdfcomp(list(fn,fln,fg), legendtext=c("normal", "lognormal", "gamma"))
ppcomp(list(fn,fln,fg), legendtext=c("normal", "lognormal", "gamma"))

# dev.copy(png,'3. MLM_fitting/model_selection_viz/vit_distribution_comparaison.png',
#          height = 619,
#          width = 1089)
# dev.off()

######################
## Dataset clustering Kfold cross validation
######################

# Adding a variable for each runner / per year
race_result <- race_result %>%
  group_by(id_coureur,
           annee) %>%
  mutate(id_coureur_annee = cur_group_id()) %>%
  ungroup()

# Creation of a smaller dataset containing race from 2019 and runner with at least 2 races
race_result_small <- race_result %>%
  dplyr::select(c("id_course_annee",
                  "id_coureur_annee",
                  "km_effort",
                  "sexe",
                  "vitesse_moy",
                  "annee")) %>%
  filter(annee == 2019) %>%
  group_by(id_coureur_annee) %>%
  filter(n() > 1) %>%
  ungroup()

# Keep randomly 1000 runners
race_sample <- sample(unique(race_result_small$id_coureur_annee), 1000)
df <- race_result_small %>%
  filter(id_coureur_annee %in% race_sample) %>%
  transform(id_coureur_annee = as.factor(id_coureur_annee),
            id_course_annee = as.factor(id_course_annee),
            sexe = as.factor(sexe)) 

######################
## Random effect selection
######################

gc(reset=TRUE)

KfoldCV_glmmTMB <- function(formula, data, name, kfolds){
  
  folds <- createFolds(data$vitesse_moy, k = kfolds)
  result <- lapply(folds, function(x){
      data_train <- data[-x,] %>%
      mutate(id_coureur_annee = droplevels(id_coureur_annee),
             id_course_annee = droplevels(id_course_annee))
    
    data_validaton <- data[x,] %>%
      mutate(id_coureur_annee = droplevels(id_coureur_annee),
             id_course_annee = droplevels(id_course_annee))
    
    model <- glmmTMB(formula,
                     data = data_train, 
                     family = Gamma(link = "log"), 
                     control = glmmTMBControl(parallel = 7))
    
    y_pred_train = predict(model,
                           newdata = data_train,
                           type = "response",
                           allow.new.levels = TRUE)
    
    y_pred_validation = predict(model,
                                newdata = data_validaton,
                                type = "response",
                                allow.new.levels = TRUE)

    
    rmse_train = sqrt(mean((data_train$vitesse_moy - y_pred_train)^2))
    rmse = sqrt(mean((data_validaton$vitesse_moy - y_pred_validation)^2))
    mae_train = mae(data_train$vitesse_moy, y_pred_train)
    mae = mae(data_validaton$vitesse_moy, y_pred_validation)
    R2m = r.squaredGLMM(model)[3,1]
    R2c = r.squaredGLMM(model)[3,2]
    aic = AIC(model)
    
    return(data.table(rmse_train, rmse,mae_train, mae, aic, R2m, R2c))
    
  })
  
  result <- data.table(rbindlist(result)) %>%
    summarise(rmse_train = mean(rmse_train,
                                na.rm = TRUE),
              rmse = mean(rmse,
                          na.rm = TRUE),
              mae_train = mean(mae_train,
                               na.rm = TRUE),
              mae = mean(mae,
                         na.rm = TRUE),
              R2m = mean(R2m,
                         na.rm = TRUE),
              R2c = mean(R2c,
                         na.rm = TRUE),
              aic = mean(aic,
                         na.rm = TRUE))%>%
    mutate(model = name)
  
  return(result)
}

# incerpet + slope random effect of id_coureur_annee and id_course_annee
system.time(gamma_glmm1 <- KfoldCV_glmmTMB(formula = vitesse_moy ~ km_effort*sexe 
                               + (1 + km_effort | id_coureur_annee) 
                               + (1 + km_effort | id_course_annee), 
                               data = df,
                               kfolds = 5,
                               name = 'gamma_glmm1'))

# incerpet only random effect of id_coureur_annee and id_course_annee
gamma_glmm2 <- KfoldCV_glmmTMB(formula = vitesse_moy ~ km_effort*sexe 
                               + (1 | id_coureur_annee) 
                               + (1 | id_course_annee),
                               data = training_set,
                               kfolds = 5,
                               name = 'gamma_glmm2')


# incerpet + slope random effect of id_coureur_annee and incerpet only on id_course_annee
gamma_glmm3 <- KfoldCV_glmmTMB(formula = vitesse_moy ~ km_effort*sexe 
                               + (1 + km_effort| id_coureur_annee) 
                               + (1 | id_course_annee),
                               data = training_set,
                               kfolds = 5,
                               name = 'gamma_glmm3')


# incerpet only random effect of id_coureur_annee and incerpet + slope on id_course_annee
gamma_glmm4 <- KfoldCV_glmmTMB(formula = vitesse_moy ~ km_effort*sexe 
                               + (1 | id_coureur_annee) 
                               + (1  + km_effort | id_course_annee),
                               data = training_set,
                               kfolds = 5,
                               name = 'gamma_glmm4')

# Visualize result

metrics_plot <- function(data){
  
  plot <- list()
  
  for(i in 1:(ncol(data)-1)){
    plot[[i]] <- ggplot(data = data) +
      geom_bar(aes(x = model,
                   y = data[, i]),
               stat = "identity",
               alpha = .3,
               color = 'steelblue') + 
      labs(x = "Model",
           y = colnames(data)[i]) +
      theme_bw()
  }
  
 ggpubr::ggarrange(plotlist = plot,
                   align = 'hv')
}

result_tot <- rbind(gamma_glmm1, 
                    gamma_glmm2,
                    gamma_glmm3,
                    gamma_glmm4)

metrics_plot(result_tot)
ggsave('3. MLM_fitting/model_selection_viz/metrics_random_selection.png')

######################
## Fixed effect selection
######################

# incerpet only 
gamma_glmm2.1 <- KfoldCV_glmmTMB(formula = vitesse_moy ~ 1
                                 + (1 | id_coureur_annee)
                                 + (1 | id_course_annee),
                                 data = training_set,
                                 kfolds = 5,
                                 name = 'gamma_glmm2.1')

# km_effort only fixed effect
gamma_glmm2.2 <- KfoldCV_glmmTMB(formula = vitesse_moy ~ km_effort
                                 + (1 | id_coureur_annee)
                                 + (1 | id_course_annee),
                                 data = training_set,
                                 kfolds = 5,
                                 name = 'gamma_glmm2.2')

# km_effort + sexe fixed effect
gamma_glmm2.3 <- KfoldCV_glmmTMB(formula = vitesse_moy ~ km_effort + sexe
                                 + (1 | id_coureur_annee)
                                 + (1 | id_course_annee),
                                 data = training_set,
                                 kfolds = 5,
                                 name = 'gamma_glmm2.3')

# sexe only fixed effect
gamma_glmm2.4 <- KfoldCV_glmmTMB(formula = vitesse_moy ~ sexe
                                 + (1 | id_coureur_annee)
                                 + (1 | id_course_annee),
                                 data = training_set,
                                 kfolds = 5,
                                 name = 'gamma_glmm2.4')

result_tot <- rbind(gamma_glmm2,
                    gamma_glmm2.1,
                    gamma_glmm2.2,
                    gamma_glmm2.3,
                    gamma_glmm2.4)

metrics_plot(result_tot)

options(na.action = "na.fail")
model_dredge <- MuMIn::pdredge(global.model = gamma_glmm2, rank = "AIC")

print(model_dredge)

ggsave('MLM_fitting/model_selection_viz/metrics_fixed_selection.png')

######################
## Learning curve
######################

# Initiate object for learning curve
result_tot <- data.table()

# Create a pool of all runner
pool <- sample(unique(race_result_small$id_coureur_annee))

# model use for the leanring curve
formula = vitesse_moy ~ km_effort*sexe + (1 | id_coureur_annee) + (1 | id_course_annee)

# cross validation with 5 folds
cv = 5

# SPlit in 15 sample size
lseq <- function(from=1, to=100000, length.out=6) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))
}
splits = lseq(100, round(length(unique(race_result_small$id_coureur_annee))), length.out = 15)

# Loop to each increasing split 
for(split in splits) {
  
  ptm <- proc.time()

  sample <- pool[1:split]
  
  #Encode dataset
  data <- race_result_small %>%
    filter(id_coureur_annee %in% sample) %>%
    group_by(id_coureur_annee) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    transform(id_coureur_annee = as.factor(id_coureur_annee),
              id_course_annee = as.factor(id_course_annee),
              sexe = as.factor(sexe))
  
  # Create folds
  folds <- createFolds(data$vitesse_moy, k = cv)
  
  # Cross validation on each samples
  result <- lapply(folds, function(x){
    data_train <- data[-x,] %>%
      mutate(id_coureur_annee = droplevels(id_coureur_annee),
             id_course_annee = droplevels(id_course_annee))
    
    data_validaton <- data[x,] %>%
      mutate(id_coureur_annee = droplevels(id_coureur_annee),
             id_course_annee = droplevels(id_course_annee))

    model <- glmmTMB(formula,
                     data = data_train,
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(parallel = 7))
      
    s <- summary(model)
    
    # y_pred = predict(model, 
    #                  newdata = data_validaton, 
    #                  type = "response", 
    #                  allow.new.levels = TRUE)
    
    result <-  data.table(
      # rmse = sqrt(mean((data_validaton$vitesse_moy - y_pred)^2)),
      # mae = mae(data_validaton$vitesse_moy, y_pred),
      R2m = r.squaredGLMM(model)[3,1],
      R2c = r.squaredGLMM(model)[3,2],
      aic = AIC(model),
      beta_intercept = s$coefficients$cond[1],
      sd_intercept = s$coefficients$cond[1, 2],
      pvalue_intercept = s$coefficients$cond[1, 4],
      beta_kmE = s$coefficients$cond[2],
      sd_kmE = s$coefficients$cond[2, 2],
      pvalue_kmE = s$coefficients$cond[2, 4],
      beta_sexeH = s$coefficients$cond[3],
      sd_sexeH = s$coefficients$cond[3, 2],
      pvalue_sexeH = s$coefficients$cond[3, 4],
      beta_interaction = s$coefficients$cond[4],
      sd_interaction = s$coefficients$cond[4, 2],
      pvalue_interaction = s$coefficients$cond[4, 4]
    )
  })
  
  value = c("R2m", "R2c", "aic", "beta_intercept", "beta_kmE", "beta_sexeH", "beta_interaction")
  
  # results shaping
  result_mean <- rbindlist(result) %>%
    summarise(R2m = mean(R2m, na.rm = TRUE),
              R2c = mean(R2c, na.rm = TRUE),
              aic = mean(aic, na.rm = TRUE),
              beta_intercept = mean(beta_intercept, na.rm = TRUE),
              beta_kmE = mean(beta_kmE, na.rm = TRUE),
              beta_sexeH = mean(beta_sexeH, na.rm = TRUE),
              beta_interaction = mean(beta_interaction, na.rm = TRUE)) 
    
  result_time <- data.table(nbr_coureur = rep(length(unique(levels(data$id_coureur_annee))), 7),
                            nbr_data = rep(length(data$id_coureur_annee), 7),
                            time = rep(print((proc.time() - ptm) / 60)[3], 7))
  
  result_sd <- rbindlist(result) %>%
    summarise(R2m = NA,
              R2c = NA,
              aic = NA,
              beta_intercept = mean(sd_intercept, na.rm = TRUE),
              beta_kmE = mean(sd_kmE, na.rm = TRUE),
              beta_sexeH = mean(sd_sexeH, na.rm = TRUE),
              beta_interaction = mean(sd_interaction, na.rm = TRUE))
  
  result_pvalue <- rbindlist(result) %>% 
    summarise(R2m = NA,
              R2c = NA,
              aic = NA,
              beta_intercept = mean(pvalue_intercept, na.rm = TRUE),
              beta_kmE = mean(pvalue_kmE, na.rm = TRUE),
              beta_sexeH =mean(pvalue_sexeH, na.rm = TRUE),
              beta_interaction = mean(pvalue_interaction, na.rm = TRUE))
  
  result_tot <- rbind(result_tot,
                      cbind(value = value,
                            mean = t(result_mean)[, 1],
                            sd = t(result_sd)[, 1],
                            p_value = t(result_pvalue)[, 1],
                            result_time))
  print(result_time[1,])
  next
  return(result_tot)
}

# Write results to avoid to redo loop
fwrite(result_tot, 'MLM_fitting/learning_curve_data.csv', row.names = FALSE)


# Visualisation of learning curves

# Learning curve of beat coefficient
plot <- split(result_tot %>% filter(value %in% value[4:7]), by ="value")

plot_coef <- lapply(seq_along(plot), function(x){
  plot[[x]] %>%
    ggplot(aes(x = nbr_data,
               y = mean)) +
    geom_ribbon(aes(ymin= mean - sd,
                    ymax=mean + sd),
                alpha = .5,
                color = 'grey') +
    geom_point() + 
    geom_line(group = 1) + 
    scale_y_continuous(breaks = pretty_breaks()) + 
    scale_x_continuous(breaks = pretty_breaks(),
                       trans = 'log')+
    labs(x = 'Sample size',
         y = names(plot[x])) + 
    theme_bw()
})

plot_pvalue <- lapply(seq_along(plot), function(x){
  plot[[x]] %>%
    ggplot(aes(x = nbr_coureur,
               y = p_value)) +
    geom_point() + 
    geom_line(group = 1,
              linetype = 'dotted') + 
    geom_hline(yintercept = 0.05,
               color = 'red') + 
    scale_y_continuous(breaks = pretty_breaks()) + 
    scale_x_continuous(breaks = pretty_breaks(),
                       trans = 'log')+
    labs(x = 'Sample size',
         y = 'p_value') + 
    theme_bw()
})

ggpubr::ggarrange(ggpubr::ggarrange(plotlist = plot_coef, ncol = 1, align = 'hv'),
                  ggpubr::ggarrange(plotlist = plot_pvalue, ncol = 1, align = 'hv'),
                  ncol = 2,
                  align = 'hv')

# Leanring curve of model evaluation

plot_lc <- split(result_tot %>% filter(value %in% value[1:3]), by ="value")

plot_learning_curve <- lapply(seq_along(plot_lc), function(x){
  plot_lc[[x]] %>%
    ggplot(aes(x = nbr_data,
               y = mean)) +
    geom_point() +
    geom_line(group = 1) +
    scale_x_continuous(breaks = pretty_breaks(),
                       trans = 'log10' )+ 
    scale_y_continuous(breaks = pretty_breaks())+
    labs(x = 'Sample size',
         y = names(plot_lc[x])) + 
    theme_bw()
})

ggpubr::ggarrange(plotlist = plot_learning_curve,
                  align = 'hv')


######################
## Final model buidling and evaluation
######################

set.seed(123)


# Keep only runner which race 2 races
df <- race_result %>%
  group_by(id_coureur_annee) %>%
  filter(n() > 1) %>%
  ungroup() 

# The learning curve showed that the results do not evolve after 10000 runners
df <- df %>%
  filter(id_coureur_annee %in% sample(unique(df$id_coureur_annee), 50000)) %>%
  transform(id_coureur_annee = as.factor(id_coureur_annee),
            id_course_annee = as.factor(id_course_annee),
            sexe = as.factor(sexe))

model <- glmmTMB(vitesse_moy ~ km_effort*sexe + (1 | id_coureur_annee) + (1 | id_course_annee),
                 data = df, 
                 family = Gamma(link = "log"), 
                 control = glmmTMBControl(parallel = 7))
summary(model)


# Plot fitted vs rseiduals
plot(fitted(model), residuals(model, type = "response"))

library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
residuals(simulationOutput)

re <- ranef(model)$cond$id_coureur_annee
qqnorm(re$`(Intercept)`, main = 'Normal Q-Q Plot of runner random effect')
qqline(re$`(Intercept)`)

re <- ranef(model)$cond$id_course_annee
qqnorm(re$`(Intercept)`, main = 'Normal Q-Q Plot of race random effect')
qqline(re$`(Intercept)`)


# Marginal effect plotting
library(ggeffects)
mydf <- ggemmeans(model, terms = c("km_effort", "sexe"))
ggplot(mydf, aes(x, predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) + 
  labs(
    y = get_y_title(mydf),
    x = get_x_title(mydf),
    colour = get_legend_title(mydf)
  ) +
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(breaks = pretty_breaks())


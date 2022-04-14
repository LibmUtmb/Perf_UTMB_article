#******************************************************************************
## Description .......: descriptive analysis & visualization
##
## Author ............: Franck Le Mat
## Date ..............: 2021-09-21
## License ...........: GPL
##
## Inputs ............: 'DRV_info_course.csv'
##                      'DRV_result.csv'     
##                      
## Outputs ...........: visualization
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

setwd("~/Project _dataScience/UTMB_dataMining/3. Analysis/")

info_course <- fread('DRV_info_course.csv', stringsAsFactors = TRUE)
df <- fread('DRV_result.csv', stringsAsFactors = TRUE)

df %>% group_by(annee) %>%
  filter(dist_final > 43) %>%
  summarise(length(unique(id_coureur)))

df %>% group_by(sexe) %>%
  summarise(mean(age),
            quantile(age, .25),
            quantile(age, .75),
            IQR(age))

info_course %>%
  summarise(median(dist_final),
            IQR(dist_final),
            median(deniv_final),
            IQR(deniv_final))

km_effort <- c(0,25,45,75,115,155,210,Inf)
df$catUTMB <- cut(df$km_effort, km_effort, right = FALSE, include.lowest = TRUE)
info_course$catUTMB <- cut(info_course$km_effort, km_effort, right = FALSE, include.lowest = TRUE)

df <- df %>%group_by(id_course_annee) %>% 
mutate(perc_rank = (place - min(place)) / (max(place) - min(place)))

quantile <- c(0,.25,.50,.75,1)
df$quantile <- cut(df$perc_rank, quantile, right = FALSE, include.lowest = TRUE)

general_count <- df %>%
  group_by(catUTMB) %>%
  summarise(course = length(unique(id_course_annee)),
            coureur = length(unique(id_coureur)),
            homme = length(unique(id_coureur[sexe == "H"])),
            femme = length(unique(id_coureur[sexe == "F"])))

png("visualisation/data_output.png", height=10, width=20, units = "cm", res = 200)
grid.table(general_count)
dev.off()

######################
## Descriptive analysis
######################

# Show the amount of race in total
length(unique(info_course$id_course_annee))

# Show how many unique runner in total
length(unique(df$id_coureur))

summary(df$age)
sd(df$age)

######################
## Participation
######################

# Participation details
partic <- df %>% 
  group_by(year = as.factor(annee)) %>%
  summarise(nb_runner = n(), 
            nb_runner_unique = length(unique(id_coureur)), 
            nb_race = length(unique(id_course_annee)),
            .groups = 'drop') %>%
  mutate(ratio = nb_runner / nb_race, pct = nb_runner/sum(nb_runner)) %>%
  ungroup()

p <- ggplot(data = partic, aes(x = year, y = nb_runner)) +
  geom_line(group = 1, size = 1, color = "steelblue") +
  scale_y_continuous(breaks = breaks_width(100000), labels = label_number()) + 
  labs(x = NULL, y = NULL, title = 'Number of participant') +
  theme(plot.title = element_text(hjust = 0.5, size = 10))
print(p)
ggsave("visualisation/nbParticipant_year.png")
round((partic$nb_runner[partic$year == 2019] - partic$nb_runner[partic$year == 2010]) / partic$nb_runner[partic$year == 2010] * 100, 2)


# Calculate the number of race / year / participant
partic_race <- df %>% 
  group_by(year = as.factor(annee), id_coureur) %>%
  summarise(nb_race = n(), 
            .groups = 'drop') %>%
  mutate(nb_race = ifelse(nb_race > 6, '>6', nb_race)) %>%
  group_by(year, nb_race) %>%
  summarise(nb_runner = n(), 
            .groups = 'drop_last') %>%
  mutate(pct = nb_runner/sum(nb_runner)) %>%
  ungroup()

# Calculate number of participant per km_effort categories
partic_cat <- df %>%
  group_by(year = as.factor(annee), catUTMB) %>%
  summarise(nb_runner = n(), 
            nb_race = length(unique(id_course_annee)),
            .groups = 'drop_last') %>%
  mutate(ratio = nb_runner / nb_race, 
         pct = nb_runner/sum(nb_runner)) %>%
  ungroup()

p1 <- ggplot(data = partic_cat, aes(x = year, y = nb_race)) + #Faced bar chart of number of race per UTBM category
  geom_bar(stat = 'identity', 
           aes(fill = catUTMB), 
           position = position_stack(reverse = TRUE)) + 
  scale_y_continuous(breaks = breaks_pretty(), 
                     labels = label_number()) + 
  scale_fill_jco() +
  labs(x = NULL, 
       y = NULL, 
       title = 'Number of race by years and UTMB km effort categories', fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom") 
print(p1)
ggsave("visualisation/raceEvolution_year.png")

p2 <- ggplot(data = partic_cat, aes(x = year, y = nb_race)) + #Faced bar chart of number of race per UTBM category
  geom_bar(stat = 'identity', 
           aes(fill = catUTMB), 
           position = position_fill(reverse = TRUE)) + 
  scale_y_continuous(breaks = breaks_pretty(), 
                     labels = label_number()) + 
  scale_fill_jco() +
  labs(x = NULL, 
       y = NULL, 
       title = 'Number of race by years and UTMB km effort categories', fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), legend.position = "bottom") 
print(p2)
ggsave("visualisation/raceEvolution_year_normalized.png")

p3 <- ggplot(data = partic_cat, 
             aes(x = year, y = nb_runner)) + #Count the ration between number of trail race participation / number of race per year
  geom_line(aes(group = catUTMB, color = catUTMB), 
            size = 1) +
  scale_color_jco() +
  scale_y_continuous(breaks = breaks_pretty(), 
                     labels = label_number()) + 
  labs(x = NULL, 
       y = NULL, 
       title = 'Number of runner by UTMB km effort categories', 
       color = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom")
print(p3)
ggsave("visualisation/runnerEvolution_year.png")

p4 <- ggplot(data = partic_cat, 
             aes(x = year, y = ratio)) + #Count the number of trail race participant per year
  geom_line(aes(group = catUTMB, color = catUTMB),
            size = 1) +
  scale_color_jco() +
  scale_y_continuous(breaks = breaks_pretty(), 
                     labels = label_number()) + 
  labs(x = NULL, 
       y = NULL, 
       title = 'Average runner per race by UTMB km effort categories', 
       color = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom")
print(p4)
ggsave("visualisation/runnerPerRaceEvolution_year_line.png")



# Calculate number of participant per distance > or < 80 km 
partic_dist <- df %>%
  group_by(year = as.factor(annee), dist_cat = ifelse(km_effort > 75, "Over 75 km effort", "Under 75 km effort")) %>%
  summarise(nb_runner = n(), 
            nb_race = length(unique(id_course_annee)),
            .groups = 'drop_last') %>%
  mutate(ratio = nb_runner / nb_race, 
         pct = nb_runner/sum(nb_runner)) %>%
  ungroup()
partic_dist$dist_cat <- factor(partic_dist$dist_cat, levels = c("Under 75 km effort", "Over 75 km effort"))

p1 <- ggplot(data = partic_dist, 
             aes(x = year, y = nb_runner)) + #Count number of trail race participant per year (short v long distance)
  geom_line(aes(group = dist_cat, 
                color = dist_cat), 
            size = 1) +
  scale_color_lancet() +
  scale_y_continuous(breaks = breaks_pretty(), 
                     labels = label_number()) + 
  labs(x = NULL, 
       y = NULL, 
       title = 'Number of runner by distance', 
       color = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom")
print(p1)
ggsave("visualisation/runner_distance.png")

p2 <- ggplot(data = partic_dist, 
             aes(x = year, y = ratio)) + #Count the ratio between number of trail race participation / number of race per year (short v long distance)
  geom_line(aes(group = dist_cat, 
                color = dist_cat), 
            size = 1) +
  scale_color_lancet() +
  scale_y_continuous(breaks = breaks_pretty(), 
                     labels = label_number()) + 
  labs(x = NULL, 
       y = NULL,
       title = 'Average runner per race by distance', color = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom")
print(p2)
ggsave("visualisation/runnerPerRace_distance.png")

# Calculate the number of participation per year per runner
p1 <- ggplot(partic_cat, 
             aes(x = year, 
                 y = pct, 
                 fill = catUTMB)) + 
  geom_bar(stat = 'identity', 
           position = position_stack(reverse = TRUE)) +
  labs(title =  "Distance distribution participation", 
       fill = "UTMB categories") +
  scale_y_continuous(labels = percent) + 
  scale_fill_jco() +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10), 
        legend.position = "bottom")
print(p1)
ggsave("visualisation/distanceDistrib_year.png")

p2 <- ggplot(data = partic_dist, aes(x = year, 
                                     y = pct, 
                                     fill = dist_cat)) + 
  geom_bar(stat = 'identity', 
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = percent(round(pct, 3))), 
            position = position_stack(reverse = TRUE), 
            color = 'white', 
            vjust = 3) +
  scale_fill_lancet(name = NULL) +
  labs(title = "Distance distribution participation") +
  scale_y_continuous(labels = percent) + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10), 
        legend.position = "bottom")
print(p2)
ggsave("visualisation/distanceDistrib_year2.png")

round(partic_dist$nb_runner[partic_dist$year == 2019 & partic_dist$dist_cat == "Under 80 km"] / 
        partic_dist$nb_runner[partic_dist$year == 2010 & partic_dist$dist_cat == "Under 80 km"], 2)

round(partic_dist$nb_runner[partic_dist$year == 2019 & partic_dist$dist_cat == "Over 80 km"] / 
        partic_dist$nb_runner[partic_dist$year == 2010 & partic_dist$dist_cat == "Over 80 km"], 2)


# Calculate participation partition per country
partic_country <- df %>% 
  group_by(cio) %>% 
  summarise(nb_runner = n(), 
            nb_race = length(unique(id_course_annee)), 
            .groups = 'drop_last') %>%
  mutate(ratio = nb_runner / nb_race, 
         pct = nb_runner/sum(nb_runner))
  # slice_max(order_by = nb_runner, n = 20)

p1 <- ggplot(partic_country %>%
               slice_max(order_by = nb_runner, n = 20), 
             aes(x = pct, 
                 y = reorder(cio, pct))) + 
  geom_bar(stat = 'identity', 
           fill = 'steelblue') +
  geom_text(aes(label = scales::percent(pct, accuracy = .1L))) +
  theme(axis.title=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        plot.title = element_text(hjust = 0.5, 
                                  size = 10), 
        legend.position = "bottom") +
  scale_x_continuous(labels = percent) + 
  labs(title = "Race country repartition (Top 20)")
print(p1)
ggsave("visualisation/country_repartition.png")

pays <- fread('~/Project _dataScience/UTMB_dataMining/2. Prepared Data/localisation/WRK_pays.csv')

df <- inner_join(df, pays, by = "cio")

#< 80km

partic_country <- df %>% 
  filter(km_effort < 75) %>%
  group_by(alpha3) %>% 
  summarise(nb_runner = n(), 
            nb_race = length(unique(id_course_annee)), 
            .groups = 'drop_last') %>%
  mutate(ratio = nb_runner / nb_race, 
         pct = round(nb_runner/sum(nb_runner) * 100, 2))

mapped_data <- joinCountryData2Map(partic_country, joinCode = "ISO3", nameJoinColumn = "alpha3")
par(mai=c(2,1,1,1),xaxs="i",yaxs="i")
colourPalette <- brewer.pal(8, 'Blues')

mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "pct", mapTitle = 'Under 75 km effort participation rate', 
                            colourPalette = colourPalette, missingCountryCol = 'white', aspect = 1, catMethod = 'pretty')
do.call(addMapLegend, c( mapParams, legendLabels="all"))
ggsave("visualisation/map_participation.png")

#< 80km

partic_country <- df %>% 
  filter(km_effort > 75) %>%
  group_by(alpha3) %>% 
  summarise(nb_runner = n(), 
            nb_race = length(unique(id_course_annee)), 
            .groups = 'drop_last') %>%
  mutate(ratio = nb_runner / nb_race, 
         pct = round(nb_runner/sum(nb_runner) * 100, 2))

mapped_data <- joinCountryData2Map(partic_country, joinCode = "ISO3", nameJoinColumn = "alpha3")
par(mai=c(2,1,1,1),xaxs="i",yaxs="i")
colourPalette <- brewer.pal(8, 'Blues')

mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "pct", mapTitle = 'Over 75 km effort participation rate', 
                            colourPalette = colourPalette, missingCountryCol = 'white', aspect = 1, catMethod = 'pretty')
do.call(addMapLegend, c( mapParams, legendLabels="all"))



format(partic$nb_runner_unique[partic$year == 2010], big.marks = " ")

format(partic$nb_runner_unique[partic$year == 2019], big.marks = " ")

format((partic$nb_runner_unique[partic$year == 2019] - partic$nb_runner_unique[partic$year == 2010]) / 
         partic$nb_runner_unique[partic$year == 2010] * 100, big.marks = " ")

# Plot for unique participant

p1 <- ggplot(data = partic, 
             aes(x = year, y = nb_runner_unique)) + #Count the number of trail participation per year
  geom_line(group = 1, 
            size = 1, 
            color = "steelblue") +
  scale_y_continuous(breaks = breaks_width(100000), 
                     labels = label_number()) + 
  labs(x = NULL, 
       y = NULL, 
       title = 'Number of unique participant') +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10))
print(p1)
ggsave("visualisation/nbRunner_unique.png")

p2 <- ggplot(partic_race, 
             aes(x = year, 
                 y = pct, 
                 fill = factor(nb_race, 
                               levels = c("1", "2", "3", "4", "5", "6", ">6")))) + 
  geom_bar(stat = 'identity', 
           position = position_fill(reverse = TRUE)) +
  scale_fill_jco() +
  scale_y_continuous(labels = percent) + 
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom") +
  labs(title = "Distribution of participants by number of races per years", fill = "Number of race")
print(p2)
ggsave("visualisation/RacePerRunner_year.png")

# Calculate participation per gender per UTMB cat
partic_gender <- df %>%
  group_by(catUTMB, sexe) %>%
  summarise(nb_runner = n(), 
            nb_race = length(unique(id_course_annee)), 
            .groups = 'drop_last') %>%
  mutate(ratio = nb_runner / nb_race, 
         pct = nb_runner/sum(nb_runner)) %>%
  ungroup()

df_q <- df %>%
  group_by(id_course_annee, quantile, sexe) %>%
  summarise(nb_runner = n(), 
            km_effort = mean(km_effort),
            .groups = 'drop_last') %>%
  mutate(ration = nb_runner / sum(nb_runner)) %>%
  filter(sexe == "F" & is.na(quantile) == FALSE) %>%
  ungroup()

p1 <- df_q %>%
  ggplot(aes(x = km_effort, 
             y = ration)) +
  geom_point(position = 'jitter', size = 1, alpha = .5) +
  geom_smooth() +
  scale_x_continuous('Race km effort (a.u.)', 
                     breaks = breaks_width(50), 
                     limits = c(0,400)) +
  scale_y_continuous('Female particpation (%)', 
                     breaks = breaks_width(0.1), 
                     labels = label_percent(accuracy = 1)) +
  theme_classic() +
  facet_wrap(~quantile)
print(p1)
ggsave("visualisation/Wrate_kmEffort_quartile.png")
p1 <- info_course %>%
  ggplot(aes(x = km_effort, 
             y = rate_F)) +
  geom_point(position = 'jitter', size = 1, alpha = .5) +
  geom_smooth() +
  scale_x_continuous('Race km effort (a.u.)', 
                     breaks = breaks_width(50), 
                     limits = c(0,400)) +
  scale_y_continuous('Female particpation (%)', 
                     breaks = breaks_width(0.1), 
                     labels = label_percent(accuracy = 1)) +
  theme_classic()
print(p1)
ggsave("visualisation/Wrate_kmEffort.png")

p2 <- ggplot(data = partic_gender, 
             aes(x = catUTMB, 
                 fill = sexe)) + 
  geom_bar(aes(y = pct), 
           stat = 'identity',
           position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = scales::percent(pct, 1L), y = pct / 2), 
            position = position_fill(reverse = TRUE), 
            color = 'white', 
            vjust = 2) +
  scale_fill_lancet(name = NULL) +
  labs(title = "Gender distribution per km effort category") +
  scale_y_continuous(labels = percent) + 
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom")
print(p2)
ggsave("visualisation/Wrate_UTMBcat.png")

#< 80km
info_course <- inner_join(info_course, pays, by = "cio")

count_country <- info_course %>% filter(km_effort < 75 ) %>% group_by(alpha3) %>% 
  summarise(WrateMean = round(mean(rate_F, na.rm = TRUE) * 100, 2))

mapped_data <- joinCountryData2Map(count_country, joinCode = "ISO3", nameJoinColumn = "alpha3")
par(mai=c(2,1,1,1),xaxs="i",yaxs="i")
colourPalette <- brewer.pal(8, 'Blues')[2:8]

mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "WrateMean", mapTitle = 'Under 70 km effort female participation (%)', 
                            colourPalette = colourPalette, missingCountryCol = 'white', aspect = 1, catMethod = 'pretty')
do.call(addMapLegend, c( mapParams, legendLabels="all"))

#> 80km
count_country <- info_course %>% filter(dist_final > 70) %>% group_by(alpha3) %>% 
  summarise(WrateMean = round(mean(rate_F, na.rm = TRUE) * 100, 2))

mapped_data <- joinCountryData2Map(count_country, joinCode = "ISO3", nameJoinColumn = "alpha3")
par(mai=c(2,1,1,1),xaxs="i",yaxs="i")
colourPalette <- brewer.pal(8, 'Blues')[1:6]

mapParams <- mapCountryData(mapped_data, nameColumnToPlot = "WrateMean", mapTitle = 'Over 70 km effort female participation (%)', 
                            colourPalette = colourPalette, missingCountryCol = 'white', aspect = 1, catMethod = 'pretty')
do.call(addMapLegend, c( mapParams, legendLabels="all"))


# Calculate participation per gender per year
partic_gender_year <- df %>%
  group_by(year = as.factor(annee), sexe) %>%
  summarise(nb_runner = n(), 
            nb_race = length(unique(id_course_annee)), 
            .groups = 'drop_last') %>%
  mutate(ratio = nb_runner / nb_race, 
         pct = nb_runner/sum(nb_runner)) %>%
  ungroup()

p3 <- ggplot(data = partic_gender_year, 
             aes(x = year, 
                 fill = sexe)) + 
  geom_bar(aes(y = pct), 
           stat = 'identity', 
           position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = scales::percent(pct, 1L), y = pct / 2), 
            position = position_fill(reverse = TRUE), 
            color = 'white', 
            vjust = 2) +
  scale_fill_lancet(name = NULL) +
  labs(title = "Gender distribution per year") +
  scale_y_continuous(labels = percent) + 
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom")
print(p3)
ggsave("visualisation/Wrate_year.png")

# Sex difference in performance
p1 <- info_course %>%
  ggplot(aes(x = dist_final, y = diff_mean, color = rate_F)) +
  geom_point(position = 'jitter', size = 1, alpha = .5) +
  geom_smooth() +
  labs(title = "All", color = "Female participation (%)")  +
  scale_x_continuous('Race km effort (a.u.)', breaks = breaks_width(50), limits = c(0,650)) +
  scale_y_continuous('Sex difference in performance (%)', breaks = breaks_width(0.1), labels = label_percent(accuracy = 1),
                     limits = c(-.25,1.65)) +
  scale_color_continuous(labels = percent) +
  theme_classic() +
  geom_hline(aes(yintercept =  0), color = 'orange', size = 1, linetype = 6)
p1
ggsave("visualisation/DiffHvsF_all.png")

length(info_course$id_course_annee[which(info_course$diff_mean < 0)]) / length(info_course$id_course_annee) *100

p2 <- info_course %>%
  ggplot(aes(x = km_effort, y = diff_best, color = rate_F)) +
  geom_point(position = 'jitter', size = 1, alpha = .5) +
  geom_smooth() +
  labs(title = "Top1", color = "Female participation (%)")  +
  scale_x_continuous('Race km effort (a.u.)', breaks = breaks_width(50), limits = c(0,650)) +
  scale_y_continuous('Sex difference in performance (%)', breaks = breaks_width(0.1), labels = label_percent(accuracy = 1),
                     limits = c(-.25,1.65)) +
  scale_color_continuous(labels = percent) +
  theme_classic() +
  geom_hline(aes(yintercept =  0), color = 'orange', size = 1, linetype = 6)
p2
ggsave("visualisation/DiffHvsF_Top1.png")

length(info_course$id_course_annee[which(info_course$diff_best < 0)]) / length(info_course$id_course_annee) *100

p3 <- info_course %>%
  ggplot(aes(x = km_effort, y = diff_5best, color = rate_F)) +
  geom_point(position = 'jitter', size = 1, alpha = .5) +
  geom_smooth() +
  labs(title = "Top5", color = "Female participation (%)")  +
  scale_x_continuous('Race km effort (a.u.)', breaks = breaks_width(50), limits = c(0,650)) +
  scale_y_continuous('Sex difference in performance (%)', breaks = breaks_width(0.1), labels = label_percent(accuracy = 1),
                     limits = c(-.25,1.65)) +
  scale_color_continuous(labels = percent) +
  theme_classic() +
  geom_hline(aes(yintercept =  0), color = 'orange', size = 1, linetype = 6)
length(info_course$id_course_annee[which(info_course$diff_5best < 0)]) / length(info_course$id_course_annee) *100
p3
ggsave("visualisation/DiffHvsF_Top5.png")

ggarrange(p1, p2, p3, common.legend = TRUE, legend = "bottom")
ggsave("visualisation/DiffHvsF.png")

speed_year <- df %>%
  group_by(year = as.factor(annee), 
           sexe,
           dist_cat = ifelse(km_effort > 75, "Over 75 km effort", "Under 75 km effort")) %>%
  summarise(avg_speed = mean(vitesse_moy, na.rm = TRUE), 
            .groups = 'drop') %>%
  ungroup()
speed_year$dist_cat <- factor(speed_year$dist_cat, levels = c("Under 75 km effort", "Over 75 km effort"))

speed_year_cat <- df %>%
  group_by(year = as.factor(annee), 
           sexe,
           catUTMB) %>%
  summarise(avg_speed = mean(vitesse_moy, na.rm = TRUE), 
            .groups = 'drop') %>%
  ungroup()

##Average speed

speed_year %>%
  ggplot(aes(x = year, y = avg_speed)) +
  stat_summary(fun = mean, geom = 'line', 
               aes(group = 1), 
               lwd = 1, 
               linetype = 2) +
  geom_line(aes(group = dist_cat, 
                color = dist_cat)) + 
  facet_grid(~ sexe) +
  scale_color_lancet(name = NULL) +
  scale_y_continuous(breaks = breaks_width(.2)) +
  ylab("Speed (km/h)") +
  theme_classic()
ggsave("visualisation/speed_evolution.png")

speed_year_cat %>%
  ggplot(aes(x = year, y = avg_speed)) +
  stat_summary(fun = mean, geom = 'line', 
               aes(group = 1), 
               lwd = 1, 
               linetype = 2) +
  geom_line(aes(group = catUTMB, 
                color = catUTMB)) + 
  facet_grid(~ sexe) +
  scale_color_jco(name = NULL) + 
  scale_y_continuous(breaks = breaks_width(.2)) +
  ylab("Speed (km/h)") +
  theme_classic()
ggsave("visualisation/speed_evolution2.png")

# Age trends

age_trend <- df %>%
  group_by(year = as.factor(annee), catUTMB) %>%
  summarise(age_mean = mean(age, na.rm = TRUE),
            .groups = 'drop')

age_trend %>%
  ggplot(aes(x = year, y = age_mean)) +
  stat_summary(fun = mean, geom = 'line', 
               aes(group = 1), 
               lwd = 1, 
               linetype = 2) +
  geom_line(aes(group = catUTMB, 
                color = catUTMB)) +
  scale_color_jco(name = NULL)
ggsave("visualisation/age_trend.png")

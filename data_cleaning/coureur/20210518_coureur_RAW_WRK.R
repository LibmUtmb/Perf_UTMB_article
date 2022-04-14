#******************************************************************************
## Description .......: Clean the table "coureur" from the UTMB DB
##
## Author ............: Franck Le Mat
## Date ..............: 2021-05-21
## License ...........: GPL
##
## Inputs ............: 'RAW_coureur.csv" - an csv export of UTMB DB's "coureur" table
## Outputs ...........: 'outCaseNationalite_coureur.csv' 
##                      'misNationalite_coureur.csv'
##                      'misNaissance_coureur.csv'
##                      'OUTannee_coureur.csv'
##                      'WRK_coureur.csv'
##
#******************************************************************************

library(tidyverse)
library(data.table)
library(lubridate)
library(useful)
library(funModeling)
library(scales)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/2. Prepared Data/coureur") #set the working directory 
dataset <- fread('RAW_coureur.csv', na.strings = c("","NULL"))#load the csv file in a storage dataframe named "dataset"

coureur <- dataset[,c("id", "nom", "prenom", "nationalite", "sexe", "naissance")] #Keep variable of interest in coureur dataset 

str(coureur)
# > str(coureur)
# 'data.frame':	1881073 obs. of  6 variables:
#   $ id         : int  1 2 3 4 5 6 7 8 9 10 ...
# $ nom        : chr  "CLAVERY" "GAULT" "ANTOLINOS" "SAINT GIRONS" ...
# $ prenom     : chr  "Erik" "Manu" "Fabien" "Thomas" ...
# $ nationalite: chr  "FRA" "FRA" "FRA" "FRA" ...
# $ sexe       : chr  "H" "H" "H" "H" ...
# $ naissance  : chr  "1980-06-07" "1976-11-28" "1977-04-26" "1973-11-24" ...

status(coureur)
# > status(coureur)
# variable q_zeros      p_zeros   q_na         p_na q_inf p_inf      type  unique
# id                   id       0 0.000000e+00      0 0.000000e+00     0     0   integer 1881073
# nom                 nom       2 1.063223e-06     34 1.807479e-05     0     0 character  549770
# prenom           prenom      59 3.136508e-05     70 3.721280e-05     0     0 character  195145
# nationalite nationalite       0 0.000000e+00      3 1.594834e-06     0     0 character     221
# sexe               sexe       0 0.000000e+00     29 1.541673e-05     0     0 character       3
# naissance     naissance       0 0.000000e+00 134782 7.165166e-02     0     0 character   23118

######################
## Nationalite variable cleaning 
######################

nationalite <- levels(as.factor(coureur$nationalite)) #create a list of all possible nationality 
print(nationalite)
# [1] ""    "AFG" "ALB" "ALG" "AND" "ANG" "ANT" "ARG" "ARM" "ARU" "ASA" "AUS" "Aut" "AUT" "AZE" "BAH" "BAN" "BAR" "BDI" "Bel" "BEL" "BEN" "BER" "BHU" "BIH" "BIZ"
# [27] "BLR" "BOL" "BOT" "BRA" "BRN" "BRU" "BUL" "BUR" "CAF" "CAM" "CAN" "CAY" "CGO" "CHA" "CHI" "chn" "CHN" "CIV" "CMR" "COD" "COK" "Col" "COL" "COM" "CPV" "CRC"
# [53] "Cro" "CRO" "CUB" "CYP" "CZE" "DEN" "DJI" "DMA" "DOM" "ECU" "EGY" "EPS" "ERI" "ESA" "esp" "Esp" "ESP" "EST" "ETH" "FIJ" "FIN" "fra" "Fra" "FRA" "GAB" "GAM"
# [79] "GBR" "GBS" "GEO" "GEQ" "ger" "Ger" "GER" "GHA" "GIB" "GRE" "GRN" "GUA" "GUI" "GUM" "GUY" "HAI" "HKG" "HON" "Hun" "HUN" "INA" "Ind" "IND" "IRI" "IRL" "IRQ"
# [105] "ISL" "ISR" "ISV" "ITA" "IVB" "JAM" "JOR" "JPN" "KAZ" "KEN" "KGZ" "KOR" "KOS" "KSA" "KUW" "LAO" "LAT" "LBN" "LBR" "LCA" "LES" "LIE" "LTU" "LUX" "MAC" "MAD"
# [131] "Mar" "MAR" "MAS" "MAW" "MDA" "MDV" "MEX" "MGL" "MKD" "MLI" "MLT" "MNE" "MON" "MOZ" "MRI" "MTN" "MYA" "NAM" "NCA" "NED" "NEP" "NGR" "NIG" "NMI" "Nor" "NOR"
# [157] "NZL" "OMA" "PAK" "PAN" "PAR" "Per" "PER" "PHI" "PLE" "PNG" "Pol" "POL" "Por" "POR" "PRK" "PUR" "PYF" "QAT" "ROU" "RSA" "RUS" "RWA" "SAM" "SEN" "SEY" "SGP"
# [183] "SKN" "SLE" "SLO" "SMR" "SOL" "SOM" "SRB" "SRI" "STP" "SUD" "Sui" "SUI" "SUR" "SVK" "SWE" "SWZ" "SYR" "TAN" "THA" "TJK" "TKM" "TKS" "TLS" "TOG" "TPE" "TRI"
# [209] "TUN" "TUR" "UAE" "UGA" "UKR" "URU" "USA" "UZB" "VAN" "VEN" "VIE" "YEM" "ZAM" "ZIM"

coureur %>% #list all nationality which are not written in UpperCase
  filter(upper.case(nationalite) == FALSE) %>%
  group_by(nationalite) %>%
  summarise(n()) 
# # A tibble: 20 x 2
# nationalite `n()`
# <chr>       <int>
# 1 ""              3
# 2 "Aut"           1
# 3 "Bel"           2
# 4 "chn"           1
# 5 "Col"           2
# 6 "Cro"           1
# 7 "esp"           1
# 8 "Esp"          21
# 9 "fra"           2
# 10 "Fra"         150
# 11 "ger"           1
# 12 "Ger"           2
# 13 "Hun"           1
# 14 "Ind"           4
# 15 "Mar"           2
# 16 "Nor"           1
# 17 "Per"           1
# 18 "Pol"           2
# 19 "Por"         150
# 20 "Sui"           3

outCase_nationalite <- coureur %>% #store lines with a miswritten nationality 
  filter(upper.case(dataset$nationalite) == FALSE & is.na(dataset$nationalite) == FALSE)

out_nationalite <- coureur %>% #store lines with a missing nationality 
  filter(is.na(dataset$nationalite) == TRUE)

write.csv(outCase_nationalite, file = 'outCaseNationalite_coureur.csv', row.names = FALSE) #Create a csv file with miswritten nationality line 
write.csv(out_nationalite, file = 'misNationalite_coureur.csv', row.names = FALSE) #Create a csv file with missing  nationality line

coureur$nationalite <- toupper(coureur$nationalite) #Convert all lowerCase nationality to UpperCase

coureur <- coureur %>% #Get ride of all line with missing nationality 
  filter(is.na(nationalite) == FALSE)
#1881073 - 3 = 1881070

######################
## "naissance" variable cleaning 
######################

coureur %>% group_by(nchar(naissance)) %>% #count the number of data per length of character of "naissance"
  summarise(n()) 


missing_naissance <- dataset %>% #store data with missing ("NULL") date of birth
  filter(is.na(naissance) == TRUE)
#NA - 134782 values

write.csv(missing_naissance, file = 'misNaissance_coureur.csv', row.names = FALSE) #Create a csv file with all data without date of birth

# coureur <- coureur %>% #get ride of missing date of birth
#   filter(is.na(naissance) == FALSE) We keep NA 

jours <- sprintf("%02d", 1:31) #Create a vector a possible day number adding 0 in front of 1 to 9 
mois <- sprintf("%02d", 1:12) #Create a vector a possible month number adding 0 in front of 1 to 9 
annee <- sprintf("%02d", 1920:2002) #Create a vector of possible year of birth 

'%notin%' <- Negate(`%in%`) #Create an inverse function of %in%

dataset %>% #Show all impossible date of birth - 66 lines
  filter(substring(naissance, 1, 4) %notin% annee | substring(naissance, 6, 7) %notin% mois | substring(naissance, 9, 10) %notin% jours ) %>%
  arrange(naissance)

coureur %>% group_by(annee = substring(naissance, 1, 4)) %>% #Bar chart of year of birth 
  summarise(count = n()) %>% 
  ungroup %>%
  mutate(pct = count / sum(count)) %>%
  ggplot(aes(x = annee, y = pct)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 60))


coureur %>% group_by(mois = substring(naissance, 6, 7), jours = substring(naissance, 9, 10)) %>% #bar chart of month of birth
  summarise(count = n()) %>% 
  ungroup %>%
  mutate(pct = count / sum(count)) %>%
  ggplot(aes(x = jours, y = pct)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent) + 
  theme(axis.text.x = element_text(angle = 60)) + 
  facet_wrap(~mois, scales = "free_y")

coureur %>% group_by(jours = substring(naissance, 9, 10)) %>% #bar chart of day of birth 
  summarise(count = n()) %>%
  ggplot(aes(x = jours, y = count)) +
  geom_bar(stat = 'identity')

firstJanNaissance <- coureur %>% #count the number of runner born on January 01
  filter(substring(naissance, 6, 7) == "01" & substring(naissance, 9, 10) == "01" ) %>%
  arrange(naissance)


coureur %>% #Keep only possible year of birth
filter(substring(naissance, 1, 4) %notin% annee) %>%
  count
# # # - 1740827 remaining observation

coureur %>% #Keep only possible year of birth 
filter(substring(naissance, 1, 4) %in% annee)
# # - 1740827 remaining observation

coureur$naissance[which(substring(coureur$naissance, 1, 4) %notin% annee)] <- NA


OUTannee <- dataset %>% #Store dataset line with impossible date of birth
  filter(substring(naissance, 1, 4) %notin% annee & is.na(naissance) == FALSE)

coureur$naissance <- substring(coureur$naissance, 1, 4) #Keep only year of birth 

write.csv(OUTannee, file = 'OUTannee_coureur.csv', row.names = FALSE) #Create a csv file with wrong date of birth

######################
## "sex" variable cleaning 
######################

coureur %>% #check all sexe possibility 
  group_by(sexe) %>%
  summarise(n())

coureur$sexe <- toupper(coureur$sexe) #change lowerCase to UpperCase

coureur <- coureur[,c("id", "nationalite", "sexe", "naissance")] #Anonymising
summary(coureur)
status(coureur)
write.csv(coureur, file = 'WRK_coureur.csv', row.names = FALSE) #Create csv file with the clean table 

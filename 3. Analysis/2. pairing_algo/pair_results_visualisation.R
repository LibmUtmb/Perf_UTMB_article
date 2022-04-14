#******************************************************************************
## Description .......: Visualisation a pairing results
##
## Author ............: Franck Le Mat
## Date ..............: 2021-07-22
## License ...........: GPL
##
## Inputs ............: 'x_pair_result.csv'
##                      
## Outputs ...........: visualisation 
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
library(ggrepel)
library(foreach)

######################
## Dataset load 
######################

setwd("~/Project _dataScience/UTMB_dataMining/3. Analysis/2. pairing_algo/")
fnames <- dir("~/Project _dataScience/UTMB_dataMining/3. Analysis/2. pairing_algo/", pattern = "results.csv")

read_data <- function(z){
  dat <- fread(z)
}

datalist <- sapply(fnames, read_data, simplify = FALSE, USE.NAMES = TRUE)

######################
## Variable encoding 
######################
datalist <- lapply(datalist, function(x){ x %>%
  mutate(x %>%
           separate(pair, c("short", "long"), " - "), 
           dist = ifelse(catUTMB == short, "short", "long"), 
           short = factor(short, levels = c("[0,25)",
                                            "[25,45)",
                                            "[45,75)",
                                            "[75,115)",
                                            "[115,155)",
                                            "[155,210)")), 
           long = factor(long, levels = c("[25,45)",
                                          "[45,75)",
                                          "[75,115)",
                                          "[115,155)",
                                          "[155,210)",
                                          "[210,Inf]")),
           catUTMB = factor(catUTMB, levels = c("[0,25)",
                                                "[25,45)",
                                                "[45,75)",
                                                "[75,115)",
                                                "[115,155)",
                                                "[155,210)",
                                                "[210,Inf]")), 
           pair = factor(pair, levels = unique(pair)))
} )

######################
## Results table
######################

table_result <- lapply(datalist, function(x){ t(cbind(x %>%
          group_by(pair) %>%
          summarise(nb_match = mean(nb_match),
                    nb_H_distinct = mean(nb_H_distinct),
                    nb_F_distinct = mean(nb_F_distinct),
                    age_moy = paste0('F = ', mean(round(age_moy_F, 2)), ' +/- ', mean(round(age_sd_F, 2)), ', ', 
                                     'H = ', mean(round(age_moy_H, 2)), ' +/- ', mean(round(age_sd_H, 2)))
          ),
        
        x %>%
          arrange(pair) %>%
          filter(dist == "short") %>%
          summarise('vitesse_moy (km/h)' = paste0(catUTMB, " - ", 'F = ', round(vit_moy_F, 2), ' +/- ', round(vit_sd_F, 2), ', ', 
                                                  'H = ', round(vit_moy_H, 2), ' +/- ', round(vit_sd_H, 2),", ", "diff = ", round(diff_moy, 2), ' %' )
          ),
        
        x %>%
          arrange(pair) %>%
          filter(dist == "long") %>%
          summarise('vitesse_moy (km/h)' = paste0(catUTMB, " - ", 'F = ', round(vit_moy_F, 2), ' +/- ', round(vit_sd_F, 2), ', ', 
                                                  'H = ', round(vit_moy_H, 2), ' +/- ', round(vit_sd_H, 2), ", ", "diff = ", round(diff_moy, 2), ' %' )
          ),
        x %>%
          arrange(pair) %>%
          filter(dist == "short") %>%
          summarise('différence moyenne' = paste0(catUTMB, ": ",round(moy_diff, 2), ' +/- ', round(sd_diff, 2), ' %')
          ),
        
        x %>%
          arrange(pair) %>%
          filter(dist == "long") %>%
          summarise('différence moyenne' = paste0(catUTMB, ": ",round(moy_diff, 2), ' +/- ', round(sd_diff, 2), ' %'),
                    t.test.result = paste0("P-value = ", format(p.value, scientific = TRUE), ', ', 'conf.int = [', round(conf.int.down * 100, 4)
                                          , ' - ', round(conf.int.up * 100, 4), ']')
          )
)
)
})

lapply(table_result[[11]], print)
table_result[[12]]

######################
## Average of speed difference plot
######################
plot_tot <- lapply(seq_along(datalist), function(x){
  result.stat <- as.data.frame(datalist[[x]] %>%
                                 group_by(group1 = short, group2 = long) %>%
                                 summarise(p = paste0('n = ', mean(nb_match), ', p = ', format(mean(p.value), scientific = TRUE)), y.position = max(moy_diff) + 1)) %>%
    mutate(xmin = factor(group1, levels = c("[25,45)", "[45,75)", "[75,115)", "[115,155)", "[155,210)")), 
           xmax = factor(group2, levels = c("[25,45)", "[45,75)", "[75,115)", "[115,155)", "[155,210)", "[210,Inf]")),
           groups = list(  c("[25,45)", "[45,75)"), c("[25,45)", "[75,115)"), c("[25,45)", "[115,155)"), c("[25,45)", "[155,210)"), c("[25,45)", "[210,Inf]"),
                           c("[45,75)","[75,115)"), c("[45,75)", "[115,155)"), c("[45,75)", "[155,210)"), c("[45,75)","[210,Inf]"),
                           c("[75,115)", "[115,155)"), c("[75,115)", "[155,210)"), c("[75,115)", "[210,Inf]"),
                           c("[115,155)","[155,210)"), c("[115,155)", "[210,Inf]"),
                           c("[155,210)", "[210,Inf]"))) %>%
    ungroup() %>%
    group_by(group1) %>%
    mutate(y.position = seq(from = max(datalist[[x]]$moy_diff + 1) , to = max(datalist[[x]]$moy_diff + 1) + n() - 1 )) %>%
    ungroup()
  
  ######################
  ## plot creation
  ######################
  
  text <- str_replace(names(datalist)[[x]], pattern = "_results.csv", replacement = "")
  # Create a text grob
  tgrob <- text_grob(text,size = 20, just = "top")
  # Draw the text
  plot_0 <- as_ggplot(tgrob)
  
  plot <- list()
  
  foreach(cat = levels(datalist[[x]]$catUTMB)[2:6]) %do%  { 
    
    plot[[cat]] <- datalist[[x]] %>%
      filter(short == cat) %>%
      ggplot(aes(x = catUTMB, y = moy_diff, group = pair)) + 
      geom_point() +
      geom_line() +
      geom_text_repel(aes(label = paste0(round(moy_diff, 2), ' +/- ', round(sd_diff, 2))), size = 3) +
      stat_pvalue_manual(result.stat %>% 
                           filter(group1 == cat), size = 3) +
      scale_y_continuous(limits = c(ymin = min(datalist[[x]]$moy_diff) - 1, ymax = max(result.stat$y.position) + 1),
                         breaks = pretty_breaks(), 
                         name = "Moyenne de différence de vitesse (%)") +
      theme_classic()
  }
  
  ggarrange(plot[[1]], plot[[2]] , plot[[3]], plot[[4]], plot[[5]],plot_0, ncol = 3, nrow = 2)
})

lapply(plot_tot, print)
plot_tot[[5]]

######################
## plot printing
######################

lapply(seq_along(plot_tot), function(x){
  ggsave(plot = plot_tot[[x]], 
         filename = str_replace(str_replace(names(datalist)[[x]], pattern = "_results.csv", replacement = ".png"), "%", ""), 
         height = 10, 
         width = 20,
         path = "~/Project _dataScience/UTMB_dataMining/3. Analysis/pairing_algo/visualisation/")
  })

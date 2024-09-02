
rm()

##############################################################################
# packages and data
##############################################################################
library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(fmsb)
library(dplyr)
library(patchwork)
library(ggradar)
library(devtools)
library(ggplotify)

dat <- read.csv("/Users/tg625/Downloads/Data-LC-Allvariable.csv")

##############################################################################
# PANSS
##############################################################################

##############################################################################
# 
##############################################################################

depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
missing <- "#636363"


plotdat <-dat %>%
  `[`(213:242) %>%
  drop_na() %>%
  mutate(across(everything(), ~replace(., . ==  888, 0))) %>%
  #rownames_to_column("person") %>%
  as_tibble %>%
  #group_by(person=as.factor(person) )%>%
  summarise(Positive = rowSums(across(starts_with("p"))/42), Negative = rowSums(across(starts_with("n"))/42), General = rowSums(across(starts_with("g"))/96)) %>%
  #summarise(mean_=across(c(pTotalpart, nTotalpart, gTotalpart), mean))
  pivot_longer(cols=Positive:General, values_to = "Score", names_to = "Subscale") %>%
  group_by(Subscale=as.factor(Subscale)) 
  
boxplot<-  plotdat %>%
  ggplot(aes(x=Subscale, y=Score)) + 
  geom_boxplot(aes(fill=Subscale)) +
  scale_fill_manual(values=c(col_like_alot, col_neutral, depn_insti)) +
    geom_jitter(color="black", size=2, alpha=0.3) +
    #theme_ipsum() +
    theme_minimal() +
    theme(axis.title.x=element_blank()) +
    theme(axis.text.x=element_blank()) +
    theme(legend.position="top") +
    theme(legend.title=element_blank())
    
violins<-  plotdat %>%
  ggplot(aes(x=Subscale, y=Score)) + 
  geom_violin(aes(fill=Subscale), alpha=0.8) +
  scale_fill_manual(values=c(col_like_alot,col_neutral, depn_insti)) +
    geom_jitter(color="black", size=0.4, alpha=0.5) +
    theme_minimal() +
    theme(axis.title.x=element_blank()) +
     theme(axis.text.x=element_blank()) +
    theme(legend.position="none") +
    theme(legend.title=element_blank())

combined<- boxplot / violins

#ggsave(combined, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/PANSSscore_box&Violinplots.pdf")

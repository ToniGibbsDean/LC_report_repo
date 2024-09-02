set.seed(1)
rm()

##################################################################
# packages and data
##################################################################

library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(fmsb)
library(dplyr)
library(patchwork)
library(ggplotify)
library(dplyr)
library(ggplot2)
library(stringr)

dat <- read.csv("Data/RedCapData-LC-Allvariable.csv")

#dat <- dat_full[-c(1,19:22), ]

##################################################################
#wrangling
##################################################################

  depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
missing <- "#636363"

theme_set(theme_minimal())

nPeople<-dat %>%
  `[`(61:62) %>%
  nrow()


plotdat_householdInc <- dat %>%
  `[`(61:62) %>%
  #mutate(totalP = nrow(.)) %>%
  pivot_longer(cols = household_income:participant_income, values_to = "category",
               names_to = "incomeCat") %>% 
  group_by(incomeCat, category) %>% 
  summarise(n=n()) %>%
  mutate(category = recode(category, 
                                    "1" = "Less than $10,000",
                                    "2" = "$10,000 to $19,999",
                                    "3" = "$20,000 to $39,999",
                                    "4" = "$40,000 to $59,999",
                                    "5" = "$60,000 to $99,999",
                                    "6" = "$100,000 and above",
                                    "7" = "Don't know",
                                    "8" = "Prefer not to answer")) %>%
  mutate(percent=(n/nPeople)*100) %>%
  mutate(Percentage = round(percent, 0)) %>%
  mutate(bar_text = paste0(Percentage, "%")) %>%
  ungroup() %>% drop_na()
  #mutate(category=as.factor(category)) %>%
  #mutate(incomeCat=as.factor(incomeCat)) 

order<- plotdat_householdInc %>% 
  dplyr::filter(incomeCat=="household_income") %>%
  #mutate(incomeCat = as.factor(incomeCat)) %>%
  #fct_rev(.) %>%
  pull(category)

df_odered <- plotdat_householdInc %>% 
  mutate(category = factor(category, levels = order)) %>% 
  drop_na() %>%
  mutate(incomeCat = recode(incomeCat,
                            "household_income" = "Household Income",
                            "participant_income" = "Personal Income"))

incomeplot1 <- ggplot(data=df_odered, aes(y=category, x=Percentage, fill=incomeCat)) +
  geom_bar(position = "dodge", stat="identity") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("Household Income" = col_like_alot,
                               "Personal Income" = col_like))
  
  
incomeplot <- incomeplot1 + scale_x_continuous(labels=function(x) paste0(x,"%"))
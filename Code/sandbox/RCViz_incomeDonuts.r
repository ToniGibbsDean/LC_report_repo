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

dat <- read.csv("/Users/tg625/Downloads/Data-LC-Allvariable_2024-07-09.csv")

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
   #pivot_longer(cols = household_income, values_to = "value",
            #   names_to = "incomeCat") %>%
  #mutate(household_income = as.factor(household_income)) %>%
  group_by(household_income) %>%
  summarise(`Total`=n()) %>%
  ungroup() %>%
  mutate(household_income = recode(household_income, 
                                    "1" = "Less than $10,000",
                                    "2" = "$10,000 to $19,999",
                                    "3" = "$20,000 to $39,999",
                                    "4" = "$40,000 to $59,999",
                                    "5" = "$60,000 to $99,999",
                                    "6" = "$100,000 and above",
                                    "7" = "Don't know",
                                    "8" = "Prefer not to answer"))
  
household_plot<- plotdat_householdInc %>% drop_na() %>% 
ggplot( 
       aes(x = 2, y = Total, fill = household_income))+
       geom_bar(stat = "identity") +
  coord_polar("y", start = 2) +
  geom_text(aes(y = Total, label = paste(Total, sep = "")), col = "white") +
  theme_void() +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette = "Dark2")+
  xlim(0,4) +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5, 
                                  margin = margin(t = 10, b = 20, unit = "pt"))) +
  labs(title="My Nice Graph") + 
  theme(plot.title = element_text(vjust=0.1))
  ggtitle("Household Income") +
  guides(col = guide_legend(nrow = 3)) +
  theme(plot.title = element_text(vjust=0.1))
    #  scale_fill_manual(values = c(`Less than $10,000` = col_like_alot,
     #                          `$10,000 to $19,999"` = col_like,
      #                         `$20,000 to $39,999` = col_neutral,
       #                        `$40,000 to $59,999` = col_dislike,
        #                       `$60,000 to $99,999` = depn_insti,
         #                      `$100,000 and above` = missing,
          #                     `Prefer not to answer` = "pink"))
  


  plotdat_personalInc <- dat %>%
  `[`(63:64) %>%
  #mutate(totalP = nrow(.)) %>%
   #pivot_longer(cols = household_income, values_to = "value",
            #   names_to = "incomeCat") %>%
  #mutate(household_income = as.factor(household_income)) %>%
  group_by(participant_income) %>%
  summarise(`Total`=n()) %>%
  ungroup() %>%
  mutate(participant_income = recode(participant_income, 
                                    "1" = "Less than $10,000",
                                    "2" = "$10,000 to $19,999",
                                    "3" = "$20,000 to $39,999",
                                    "4" = "$40,000 to $59,999",
                                    "5" = "$60,000 to $99,999",
                                    "6" = "$100,000 and above",
                                    "7" = "Don't know",
                                    "8" = "Prefer not to answer"))

personal_plot<-  plotdat_personalInc %>% drop_na() %>% 
ggplot( 
       aes(x = 2, y = Total, fill = participant_income))+
       geom_bar(stat = "identity") +
  coord_polar("y", start = 2) +
  geom_text(aes(y = Total, label = paste(Total, sep = "")), col = "white") +
  theme_void() +
  scale_fill_brewer(palette = "Dark2")+
  xlim(0,4) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.1, vjust = 0.5,
                                  margin = margin(t = 10, b = 20, unit = "pt"))) +
  ggtitle("Personal Income") +
   # scale_fill_manual(values = c(`Less than $10,000` = depn_insti, 
    #                           `$10,000 to $19,999"` = col_dislike, 
     #                          `$20,000 to $39,999` = col_dislike,
      #                         `$40,000 to $59,999` = col_like_alot,
       #                        `$60,000 to $99,999` = col_dislike,
        #                       `$100,000 and above` = col_dislike,
         #                      `Prefer not to answer` = col_dislike))

  c<-personal_plot | household_plot

ggsave(c, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/income_donuts.pdf")
ggsave(personal_plot, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/personalIncome_donut.pdf")
ggsave(personal_plot, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/personalIncome_donut.pdf")

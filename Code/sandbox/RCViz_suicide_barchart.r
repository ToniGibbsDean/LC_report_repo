##################################################################
# packages and data
##################################################################

library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(fmsb)
library(dplyr)
library(patchwork)
library(ggplotify)

library(echarts4r)
library(echarts4r.assets)

dat <- read.csv("/Users/tg625/Downloads/LC-Allvariable_2024-06-10_1544.csv")

##################################################################
#wrangling
##################################################################

nPeople<-dat %>%
  `[`(131:133) %>%
  drop_na() %>%
  nrow()

plotdat<-dat %>%
  `[`(131:133) %>%
  drop_na() %>%
  #mutate(totalP = nrow(.)) %>%
   pivot_longer(cols = suicidal_ideation:nonsuicidal_selfinjury, values_to = "value",
               names_to = "Suicide") %>%
  filter(value==1) %>%
  group_by(Suicide) %>%
  summarise(`Total`=n()) %>%
  mutate(percent=(Total/nPeople)*100) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(bar_text = paste0(percent, "%")) %>%
  ungroup()

##################################################################
#plot
##################################################################
  
depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
missing <- "#636363"

theme_set(theme_classic()())

gg1 <- plotdat %>%
  mutate(Suicide = recode(Suicide, 
                          "suicide_attempts" = "Suicide attempts",
                          "suicidal_ideation" = "Suicidal ideation",
                          "nonsuicidal_selfinjury" = "Self injury (non-suicidal)")) %>%
  ggplot(aes(x = Suicide, y = percent, fill = Suicide)) + 
  geom_col(width = 0.8,  position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + 
  theme(plot.margin = unit(rep(0.7, 4), "cm")) +
  scale_fill_manual(values = c(`Suicide attempts` = depn_insti,
                               `Suicidal ideation` = col_dislike_alot,
                               `Self injury (non-suicidal)` = col_dislike)) +
  #scale_x_continuous(expand = c(0, 0), labels = paste0(seq(0, 100, 25), "%")) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(axis.text = element_text(color = "black", size = 20), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 20))

gg2 <- gg1 + geom_text(aes(label = bar_text),
                       position = position_stack(vjust = 0.5, reverse = TRUE),
                       colour="white",
                       size = 15)

##################################################################
#save
##################################################################

ggsave(gg2, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/suicide_barchart.pdf")
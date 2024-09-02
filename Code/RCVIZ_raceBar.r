
set.seed(1)
rm()

##################################################################
# packages and data
##################################################################

require(tidyverse)
require(lubridate, warn.conflicts = FALSE)
require(fmsb)
require(dplyr)
require(patchwork)
require(ggplotify)
require(dplyr)
require(ggplot2)
require(stringr)

dat <- read.csv("/Users/tg625/Downloads/Data-LC-Allvariable.csv")

#dat <- dat_full[-c(1,19:22), ]

##################################################################
#wrangling
##################################################################

totalP<-dat %>%
  `[`(33:44) %>%
  #drop_na() %>%
  nrow()

plotdat<-dat %>%
  `[`(33:44) %>%
  #drop_na() %>%
  pivot_longer(cols = race___1:race___11, values_to = "value",
               names_to = "Race") %>%
  filter(value==1) %>%
  group_by(Race) %>%
  summarise(`Total`=n()) %>%
  mutate(Percentage=(Total/totalP)*100) %>%
  mutate(Percentage = round(Percentage, 0)) %>%
  mutate(bar_text = paste0(Percentage, "%")) %>%
  ungroup() %>%
  mutate(Race = recode(Race,
                       "race___1" = "First Nations",
                       "race___2" = "East Asian",
                       "race___3" = "Southeast Asian",
                       "race___4" = "South Asian",
                       "race___5" = "Black",
                       "race___6" = "Central/ South American",
                       "race___7" = "Central Asia\n& Middle East",
                       "race___8" = "White",
                       "race___9" = "Hawaiian or\nPacific Islander",
                       "race___10" = "Interracial"))

##################################################################
#plot
##################################################################

theme_set(theme_minimal())

depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
darkerstill <- "#142c43"
missing <- "#636363"


gg3<-ggplot(data=plotdat, aes(x=Race, y=Percentage, fill=Race)) +
  geom_bar(stat="identity") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = c(missing, col_like, depn_insti,col_neutral, darkerstill, col_dislike_alot))

 gg4<- gg3 + scale_y_continuous(labels=function(x) paste0(x,"%"))
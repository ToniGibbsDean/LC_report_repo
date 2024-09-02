
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
library(waffle)

dat <- read.csv("Data/RedCapData-LC-Allvariable.csv")


#dat <- dat_full[-c(1,19:22), ]

##################################################################
#wrangling
##################################################################

totalP<-dat %>%
  select(bls_code) %>%
  drop_na() %>%
  nrow(.)

plotdat<-dat %>%
  select(bls_code) %>%
  mutate(bls_code = recode(bls_code,
                           "1" = "Employed F/T",
                           "2" = "Employed P/T",
                           "3" = "Supported employment/training",
                           "4" = "Working without pay",
                           "5" = "Unemployed & searching",
                           "6" = "Temporarily laid off",
                           "7" = "Student F/T",
                           "8" = "Caregiver F/T",
                           "9" = "Not in labour force")) %>%
  #drop_na() %>%
  group_by(bls_code) %>%
  summarise(Total = n()) %>%
  mutate(percent=(Total/totalP)*100) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(bar_text = paste0(percent, "%")) %>%
  ungroup() 

theme_set(theme_minimal())

##################################################################
#plot
##################################################################

depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
darkerstill <- "#142c43"
missing <- "#636363"


gg1<-plotdat %>%
  ggplot(aes(fill = bls_code, values = Total)) +
  geom_waffle(#n_col = 5,
              n_rows = 4, 
              size = 0.2, 
              colour = "white", 
              make_proportional = FALSE, 
              radius = unit(10, "pt")) +
  scale_fill_manual(name = NULL,
                    values = c(depn_insti, col_dislike, col_like, darkerstill)) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(color = "black", size = 8)) + labs(x="Test")

#gg2 <- gg1 + annotate("text", x = 2, y = -0.25, label = "proportional", 
 #                     size = 1)


#ggsave(gg2, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/bls_waffle.pdf")

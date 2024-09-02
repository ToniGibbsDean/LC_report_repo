
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
                       "race___2" = "Asian",
                       "race___3" = "Southeast Asian",
                       "race___4" = "South Asian",
                       "race___5" = "Black",
                       "race___6" = "Central/ South American",
                       "race___7" = "Central Asia\n& Middle East",
                       "race___8" = "White",
                       "race___9" = "Hawaiian or\nPacific Islander",
                       "race___10" = "Interracial"))

theme_set(theme_minimal())

depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
darkerstill <- "#142c43"
missing <- "#636363"


gg1 <- plotdat %>% ggplot +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 10),
    color = "#7f7a7a"
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(Race, 10), Total),
      y = Percentage,
      fill = Percentage
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9,
  ) +

  # Add dots to represent the mean gain
  geom_point(
    aes(x = reorder(str_wrap(Race, 10), sum(Total)/Total),
      y = Percentage
    ),
    size = 1,
    color = "gray12"
  ) +
   geom_segment(
    aes(
      x = reorder(str_wrap(Race, 10), Total),
      y = 0,
      xend = reorder(str_wrap(Race, 10), Total),
      yend = Percentage
    ),
    linetype = "dashed",
    color = "#0b0b0b"
  ) + 
  theme(axis.text = element_text(color = "black", size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "right",
        legend.text = element_text(color = "black", size = 5),
        legend.title=element_text(size = 5)) +
        #legend.position = "none",
        #legend.title = element_blank()) +
  # Make it circular!
  coord_polar()

 gg2 <- gg1 + scale_fill_gradient2(
                            name = waiver(),
                            low = darkerstill,
                            mid = depn_insti,
                            high = col_dislike,
                            #midpoint = 0,
                            #space = "Lab",
                            #na.value = "grey50",
                            transform = "identity",
                            guide = "colourbar",
                            aesthetics = "fill"
                          ) 

gg3 <- gg1 +
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 40, barheight = .5, title.position = "right", title.hjust = .5
    ))

 gg4 <- gg2 + scale_fill_gradientn(
    "%",
     colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195")) +   
     theme(legend.position = "top") +
     guides(fill = guide_colourbar(theme = theme(
         legend.key.height  = unit(0.5, "lines"))))

#ggsave(gg4, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/race_test.pdf")

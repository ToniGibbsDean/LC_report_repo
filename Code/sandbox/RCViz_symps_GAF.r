##################################################################
# packages and data
##################################################################

library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(fmsb)
library(dplyr)
library(patchwork)
library(ggplotify)

dat <- read.csv("/Users/tg625/Downloads/LC-Allvariable_2024-06-10_1544.csv")

##################################################################
#wrangling
##################################################################

plotdat <- dat %>%
  select(gaf_12_months, current_gaf) %>%
  drop_na() %>%
  rownames_to_column("person") %>%
  group_by(person) %>%
  #mutate(dropPoints=(gaf_12_months-current_gaf)) %>%
  #mutate(scorePlus30Perc=current_gaf + (current_gaf*0.33)) %>%
  mutate(scorePlus30Perc = gaf_12_months * 0.7) %>%
  mutate(decreaseOf30Perc = scorePlus30Perc > current_gaf) %>%
  pivot_longer(cols = gaf_12_months:current_gaf, values_to = "score",
               names_to = "GAF") %>%
  mutate(GAF = as.factor(GAF))

mean <- plotdat %>%
  ungroup() %>%
  select(GAF, score) %>%
  group_by(GAF) %>%
  summarise(mean = mean(score))

##################################################################
#plot
##################################################################

p <- plotdat %>%
  ggplot(aes(y = score, x = GAF)) +
  geom_point(aes(color = decreaseOf30Perc), size = 7) +
  geom_path(aes(group = person, colour = decreaseOf30Perc), linewidth=2, alpha=0.15) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.justification = "right",
        legend.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=15),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

p1 <- p + scale_x_discrete(limits = rev(levels(plotdat$GAF)))
scale_x_discrete()

#p2<-p1 + stat_summary(fun.data = "mean_cl_boot", colour = "black", linewidth = 1, size = 0.7, linetype="dashed", alpha=0.6)

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, colour = "grey", geom = geom, width = 0.1, ...)
}

p2<- p1 + stat_sum_df("mean_cl_boot", mapping = aes(group = GAF))


p3<-p2 + scale_color_manual(values=c("#edca4a", "#ed784a"))

##################################################################
#print
##################################################################

ggsave(p3, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/GAFtimeDifference.pdf")

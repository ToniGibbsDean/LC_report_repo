
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

dat <- read.csv("/Users/tg625/Downloads/LC-Allvariable_2024-06-10_1544.csv")

depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
missing <- "#636363"

##############################################################################
# PANSS
##############################################################################

##############################################################################
# Histogram showing distribution of severity in scores across
#different subscales
##############################################################################
x <- dat %>%
  `[`(215:244) %>%
  drop_na() %>%
  mutate(across(everything(), ~replace(., . ==  888, 0))) %>%
  rownames_to_column("person") %>%
  as_tibble

names(x)[2:length(names(x))] <- substr(names(x), 1, 1)[2:length(names(x))]

PANSSseverity_hist <- x %>% # nolint
  pivot_longer(-person, values_to = "score",
               names_to = "subscale") %>%
  group_by(person, subscale, score) %>%
  tally() %>%
  ggplot(aes(x = score)) +
  geom_histogram(aes(fill = subscale))

PANSSseverit_liney <- x %>%
  pivot_longer(-person, values_to = "score",
               names_to = "subscale") %>%
  group_by(subscale, score) %>%
  tally() %>%
  ggplot(aes(x = score, y = n)) +
  geom_path(aes(colour = subscale),
            linewidt = 2.5,
            lineend = "round",
            alpha = 0.8) +
  geom_point(aes(colour = subscale),
             size = 5, alpha = 0.8) +
  geom_vline(xintercept = 3,
             linetype = "dashed") +
  theme_classic()

PathPANSSscorePlot <- PANSSseverit_liney +
  scale_color_manual(values = c("#ed784a", "#edca4a", "#ed4a6e"))

##############################################################################
# Radar lots for each subscale: overview of item distribution
##############################################################################
#################
#data wrangling
#################

Pradarplot <- dat %>%
  `[`(215:244) %>%
  drop_na() %>%
  mutate(across(everything(), ~replace(., . ==  888, 0))) %>%
  summarise(p1 = mean(p1),
            p2 = mean(p2),
            p3 = mean(p3),
            p4 = mean(p4),
            p5 = mean(p5),
            p6 = mean(p6),
            p7 = mean(p7))

max_min <- data.frame(p1 = c(6, 0),
                      p2 = c(6, 0),
                      p3 = c(6, 0),
                      p4 = c(6, 0),
                      p5 = c(6, 0),
                      p6 = c(6, 0),
                      p7 = c(6, 0))

rownames(max_min) <- c("Max", "Min")
df_p <- rbind(max_min, Pradarplot)

radarp1_df <- df_p[c("Max", "Min", "1"), ]
radarp1 <- radarchart(radarp1_df)

NradarPlot <- dat %>%
  `[`(215:244) %>%
  drop_na() %>%
  mutate(across(everything(), ~replace(., . ==  888 , 0))) %>%
  summarise(n1 = mean(n1),
            n2 = mean(n2),
            n3 = mean(n3),
            n4 = mean(n4),
            n5 = mean(n5),
            n6 = mean(n6),
            n7 = mean(n7))

max_min <- data.frame(n1 = c(6, 0),
                      n2 = c(6, 0),
                      n3 = c(6, 0),
                      n4 = c(6, 0),
                      n5 = c(6, 0),
                      n6 = c(6, 0),
                      n7 = c(6, 0))

rownames(max_min) <- c("Max", "Min")
df_n <- rbind(max_min, NradarPlot)

radarp2_df <- df_n[c("Max", "Min", "1"), ]
radarp2<-radarchart(radarp2_df)

GradarPlot <- dat %>%
  `[`(215:244) %>%
  drop_na() %>%
  mutate(across(everything(), ~replace(., . ==  888 , 0))) %>%
  summarise(g1=mean(g1), g2 = mean(g2), g3 = mean(g3), g4 = mean(g4),
            g5 = mean(g5), g6 = mean(g6), g7 = mean(g7), g8 = mean(g8),
            g9 = mean(g9), g10 = mean(g10), g11 = mean(g11), g12 = mean(g12),
            g13 = mean(g13), g14 = mean(g14), g15 = mean(g15), g16 = mean(g16))

max_min<- data.frame(g1 = c(6, 0), g2 = c(6, 0),
                     g3 = c(6, 0), g4 = c(6, 0),  g5 = c(6, 0),  g6 = c(6, 0),
                     g7 = c(6, 0),  g8 = c(6, 0), g9 = c(6, 0), g10 = c(6, 0),
                     g11 = c(6, 0), g12 = c(6, 0), g13 = c(6, 0), g14 = c(6, 0),
                     g15 = c(6, 0), g16 = c(6, 0))

rownames(max_min) <- c("Max", "Min")
df_g <- rbind(max_min, GradarPlot)

radarp3_df <- df_g[c("Max", "Min", "1"), ]
radarp3 <- radarchart(radarp3_df)

##################################
#fuction for making pretty
#################################
create_beautiful_radarchart <- function(data, color = "#00AFBB",
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...) {
  plot <- radarchart(data, axistype = 1, pcol = color,
    pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "grey", 
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title
  )

  return(plot)
}

########################
#Print  plots
######################$
#radars
pdf("/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/testingradars.pdf")
par(mfrow = c(1, 3))

op_p <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(radarp1_df, color= depn_insti,
                            caxislabels = c(1, 2, 3, 4, 5, 6, 7))
par(op_p)

op_g <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(radarp3_df, color=col_like_alot, 
                            caxislabels = c(1, 2, 3, 4, 5, 6, 7))
par(op_g)

op_n <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(radarp2_df, color=col_neutral,
                            caxislabels = c(1, 2, 3, 4, 5, 6, 7))
par(op_n)

dev.off()

#path plot
ggsave(PathPANSSscorePlot, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/PathPANSSscorePlot.pdf")



col_like_alot, col_neutral, depn_insti


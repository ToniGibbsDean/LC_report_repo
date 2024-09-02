
library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(fmsb)
library(dplyr)
library(patchwork)
library(ggplotify)


dat <- read.csv("/Users/tg625/Downloads/LC-Allvariable_2024-06-10_1544.csv")

#gender - 2 NA (?)

x <- dat %>%
  dplyr::filter(!grepl("TEST", record_id)) %>%
  filter(!record_id == "1") %>%
  mutate(identified_gender=case_when(identified_gender == "1" ~ "Male",
                                     identified_gender == "2" ~ "Female")) %>%
  select(record_id, identified_gender, date_of_birth) %>%
  drop_na() %>%
  mutate(DOB=lubridate::as_date(date_of_birth)) %>%
  mutate(age=lubridate::interval(DOB, lubridate::now()) / years(1)) %>%
  mutate(ageGroups = case_when(age > 16 & age < 21 ~ "16-21yrs",
                               age > 21 & age < 25 ~ "22-26yrs",
                               age > 25 & age < 30 ~ "27-31yrs",
                               age > 30 & age < 36 ~ "32-35yrs")) %>%
  group_by(identified_gender, ageGroups) %>%
  summarise(pop = n(), frac = pop / nrow(.))


nudge_fun <- function(df) {
  ifelse(df$identified_gender == "Male", (sd(df$pop) / 3) * - 1, sd(df$pop) / 3)
}

xlimits <- c((length(x2$identified_gender)) * -1, length(x2$identified_gender))

x2 <- x %>%
  mutate(
         pop = ifelse(identified_gender == "Male", pop * (-1), pop * 1),
         frac = ifelse(identified_gender == "Male", frac * (-1), frac * 1),
         share = paste0(abs(round(frac * 100, 1)), "%"),
         identified_gender = as.factor(identified_gender))


x2 %>%  ggplot(data = ., aes(x = pop, y = ageGroups, label = share)) +
  geom_col(aes(fill = identified_gender)) +
  geom_text(aes(label = share),
            position = position_nudge(x = nudge_fun(x2)),
            size = 4) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "black", size = 1.5) +
  scale_fill_manual("", values = c("#990099", "#009900")) +
  scale_x_continuous("", breaks = scales::pretty_breaks(n = 6),
    labels = function(br) {
      ifelse(abs(br) >= 1000,
             paste0(abs(br) / 1000, "k"),
             abs(br))
    }
  ) +
  labs(x = "No. of people", y = "Age") +
  xlim(xlimits) +
  theme_classic() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank())


test<-dat %>%
  select(race___1, race___2,race___3, race___4, race___5, race___6,
         race___7, race___8, race___9, race___10, race___11) %>%
  pivot_longer(
    cols = starts_with("race"),
    names_to = "race",
    names_prefix = "race___",
    values_to = "raceno") %>% 
  mutate(race=as.factor(race)) %>%
  group_by(race) %>%
  summarise(racecount=sum(raceno)) 
  
x<-ggplot(test, aes(race))

x+geom_bar()
  
  %>%
  ggplot(aes(x=race)) +
  geom_bar()




#PANSS
Pradarplot <- dat %>%
#select([1])
`[`(215:244) %>%
drop_na() %>%
mutate(across(everything(), ~replace(., . ==  888 , 0))) %>%
summarise(p1=mean(p1),
        p2=mean(p2),
        p3=mean(p3),
        p4=mean(p4),
        p5=mean(p5),
        p6=mean(p6),
        p7=mean(p7))

max_min<- data.frame(
    p1 = c(6, 0), p2 = c(6, 0), p3 = c(6, 0), p4 = c(6, 0),  
    p5 = c(6, 0), p6 = c(6, 0), p7 = c(6, 0))

rownames(max_min) <- c("Max", "Min")
df_p <- rbind(max_min, Pradarplot)

radarp1_df <- df_p[c("Max", "Min", "1"), ]
radarp1<-radarchart(radarp1_df)


#PANSS
NradarPlot <- dat %>%
#select([1])
`[`(215:244) %>%
drop_na() %>%
mutate(across(everything(), ~replace(., . ==  888 , 0))) %>%
summarise(n1=mean(n1),
        n2=mean(n2),
        n3=mean(n3),
        n4=mean(n4),
        n5=mean(n5),
        n6=mean(n6),
        n7=mean(n7))

max_min<- data.frame(n1 = c(6, 0),
    n2 = c(6, 0), n3 = c(6, 0), n4 = c(6, 0),  n5 = c(6, 0), 
    n6 = c(6, 0), n7 = c(6, 0))

rownames(max_min) <- c("Max", "Min")
df_n <- rbind(max_min, NradarPlot)

radarp2_df <- df_n[c("Max", "Min", "1"), ]
radarp2<-radarchart(radarp2_df)

#PANSS
GradarPlot <- dat %>%
#select([1])
`[`(215:244) %>%
drop_na() %>%
mutate(across(everything(), ~replace(., . ==  888 , 0))) %>%
summarise( g1=mean(g1), g2=mean(g2), g3=mean(g3), g4=mean(g4),
        g5=mean(g5), g6=mean(g6), g7=mean(g7), g8=mean(g8),
        g9=mean(g9), g10=mean(g10), g11=mean(g11), g12=mean(g12),
        g13=mean(g13), g14=mean(g14), g15=mean(g15), g16=mean(g16))

max_min<- data.frame( g1 = c(6, 0),  g2 = c(6, 0), 
    g3 = c(6, 0), g4 = c(6, 0),  g5 = c(6, 0),  g6 = c(6, 0),
    g7 = c(6, 0),  g8 = c(6, 0), g9 = c(6, 0), g10 = c(6, 0), 
    g11 = c(6, 0), g12 = c(6, 0), g13 = c(6, 0), g14 = c(6, 0), 
    g15 = c(6, 0), g16 = c(6, 0))

rownames(max_min) <- c("Max", "Min")
df_g <- rbind(max_min, GradarPlot)

radarp3_df <- df_g[c("Max", "Min", "1"), ]
radarp3<-radarchart(radarp3_df)

plots <- radarp1 | radarp2 | radarp3





create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  plot<-radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title
    )
  
  return(plot)
}

par(mfrow = c(3, 1)) 

op_p <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(radarp1_df, color= "#ed784a", caxislabels = c(1, 2, 3, 4, 5, 6, 7))
par(op_p)
#ed784a

op_g <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(radarp3_df, color="#edca4a", caxislabels = c(1, 2, 3, 4, 5, 6, 7))
par(op_g)
	#edca4a

op_n <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(radarp2_df, color="#ed4a6e", caxislabels = c(1, 2, 3, 4, 5, 6, 7))
par(op_n)
#ed4a6e


#op_n <- par(mar = c(1, 2, 2, 1))

test<-as.grob(
    
    create_beautiful_radarchart(radarp2_df, caxislabels = c(1, 2, 3, 4, 5))

par(op_n)

op_p | op_n | op_g

myPlots = list(op_p, op_n, op_g)
ggpubr::ggarrange(plotlist = myPlots, nrow = 1)
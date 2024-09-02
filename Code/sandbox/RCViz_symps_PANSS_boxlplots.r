
install.packages("plotly")
library(plotly)

library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(fmsb)
library(dplyr)
library(patchwork)
library(ggplotify)
library(dplyr)
library(ggplot2)
library(stringr)

dat_full <- read.csv("/Users/tg625/Downloads/LC-Allvariable_2024-06-10_1544.csv")

dat <- dat_full[-c(1,19:22), ]


x <- dat %>%
  `[`(215:244) %>%
  drop_na() %>%
  mutate(across(everything(), ~replace(., . ==  888, 0))) %>%
  rownames_to_column("person") %>%
  as_tibble %>%
    group_by(person) %>%
    pivot_longer(p1:p7, values_to = "Pscore",
               names_to = "Psubscale") %>%
    pivot_longer(n1:n7, values_to = "Nscore",
               names_to = "Nsubscale") %>%  
    pivot_longer(g1:g16, values_to = "Gscore",
               names_to = "Gsubscale") %>%
  group_by(person) %>%
  summarise(pscoresum=sum(Pscore))


threeDdat <- dat %>%
  `[`(215:244) %>%
  drop_na() %>%
  mutate(across(everything(), ~replace(., . ==  888, 0))) %>%
  rownames_to_column("person") %>%
  as_tibble %>%
  group_by(person=as.factor(person) )%>%
  summarise(pTotal = rowSums(across(starts_with("p"))), nTotal = rowSums(across(starts_with("n"))), gTotal = rowSums(across(starts_with("g"))))%>%
  #summarise(pTotal=sum(p1:p7), nTotal=sum(n1:n7), gTotal=sum(g1:g16)) %>%
  #group_by(person) %>%
  mutate(total=(pTotal/42+nTotal/42+gTotal/96))

plot_ly(threeDdat, x = ~gTotal, y = ~nTotal, z = ~pTotal,   size = ~total,
                    marker = list(color = ~total, sizemode = 'diameter', colorscale = c('#FFE1A1', '#683531'), sizes = c(5, 6), showscale = TRUE))

, color = ~am, colors = c('#BF382A', '#0C4B8E'))


  plot_ly(x=pTotal, y=nTotal, z=gTotal, type="scatter3d", mode="markers", color=subscale)

  plot_ly(threeDdat, x = ~subscale, y = ~n, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E'))

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E'))


##################################################################
# packages and data
##################################################################

library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(fmsb)
library(dplyr)
library(patchwork)
library(ggplotify)

dat <- read.csv("/Users/tg625/Downloads/Data-LC-Allvariable.csv")

#dat <- dat_full[-c(1,19:22), ]

################################
# data wrangling
################################
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

xlimits <- c((length(x$identified_gender)*1.5) * -1, length(x$identified_gender)*1.5)

x2 <- x %>%
  mutate(
         pop = ifelse(identified_gender == "Male", pop * (-1), pop * 1),
         frac = ifelse(identified_gender == "Male", frac * (-1), frac * 1),
         share = paste0(abs(round(frac * 100, 1)), "%"),
         identified_gender = as.factor(identified_gender))

######################
#make plot
######################

theme_set(theme_minimal())

pyramid_plot <- x2 %>%
  ggplot(data = ., aes(x = pop, y = ageGroups, label = share)) +
  geom_col(aes(fill = identified_gender)) +
  geom_text(aes(label = share),
            position = position_nudge(x = nudge_fun(x2)),
            size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "black", size = 1.5) +
  scale_fill_manual("", values = c("#829cb2", "#e36c33")) +
  scale_x_continuous("", breaks = scales::pretty_breaks(n = 6),
    labels = function(br) {
      ifelse(abs(br) >= 1000,
             paste0(abs(br) / 1000, "k"),
             abs(br))
    }
  ) +
  #labs(x = "No. of people", y = "Age Bands") +
  #xlim(xlimits) +
  theme(legend.position = "bottom", 
        legend.justification = "left",
        legend.text = element_text(size=8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=8),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

######################
#save plot
######################

#ggsave(pyramid_plot, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/ageXgenderPyramidplot.pdf")

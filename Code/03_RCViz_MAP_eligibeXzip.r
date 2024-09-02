
library(tidyverse)
library(terra)
library(tidyterra)

# load spatial data
svi<-terra::rast("/Users/tg625/Documents/PDA/Directory/LearningCollabMap/Outputs/svi.tif")
towns<-terra::vect("/Users/tg625/Documents/PDA/Directory/LearningCollabMap/Outputs/towns")
zip<-terra::vect("/Users/tg625/Documents/PDA/Directory/LearningCollabMap/Data/zip")

# load clinical data
dat_temp <- read.csv("/Users/tg625/Downloads/Data-LC-Allvariable.csv")
#dat_temp <- dat_full[-c(1,19:22), ]
datrdce <- dat_temp %>% 
  select("current_zipcode") %>%
  group_by(current_zipcode) %>%
  summarise(`No. Patients` = n()) %>%
  drop_na() %>%
  mutate(current_zipcode = paste0("0", as.character(current_zipcode)))

# adding clinical columns to zipcode spatvector
temp <- as_tibble(values(zip)) %>%
  select(zcta5ce10) %>%
  rownames_to_column("ID") %>%
  rename(current_zipcode = zcta5ce10) %>%
  left_join(., datrdce) %>%
  mutate(`No. Patients` = replace_na(as.numeric(`No. Patients`), 0))

e <- extract(svi, zip, layer="overall_wgs84.tif", fun=mean, ID=TRUE, na.rm=TRUE) %>% as_tibble %>% mutate(ID = as.character(ID))

finaldf <- left_join(temp, e) %>% 
  mutate(OutreachResponse = (`No. Patients` + 1)/value,
         patients = `No. Patients` > 0)

zip_wClinical<-zip
values(zip_wClinical)<-finaldf


p1_eligibleZips <- ggplot() +
  geom_spatvector(data=zip_wClinical, aes(fill=`No. Patients`), colour="lightgrey") +
  #scale_fill_manual(values=c("#f9570c","#3e6487")) #+
  theme_minimal() + 
  scale_fill_gradient2(high="#f9570c", mid="#3e6487", low="#3e6487", 
                       na.value="transparent") +
  theme(legend.text = element_text(size = 5)) +
  theme(legend.title = element_text(size = 5)) +
  theme(legend.position = "top") +
  guides(fill = guide_colourbar(theme = theme(
         legend.key.height  = unit(0.5, "lines"))))

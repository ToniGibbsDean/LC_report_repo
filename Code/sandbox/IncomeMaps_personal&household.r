
library(tidyverse)
library(terra)
library(tidyterra)

# load spatial data
svi<-terra::rast("/Users/tg625/Documents/PDA/Directory/LearningCollabMap/Outputs/svi.tif")
towns<-terra::vect("/Users/tg625/Documents/PDA/Directory/LearningCollabMap/Outputs/towns")
zip<-terra::vect("/Users/tg625/Documents/PDA/Directory/LearningCollabMap/Data/zip")

# load clinical data
dat_full <- read.csv("/Users/tg625/Downloads/LC-Allvariable_2024-06-10_1544.csv")
dat_temp <- dat_full[-c(1,19:22), ]
datrdce <- dat_temp %>% 
                select("current_zipcode", "participant_income", "household_income") %>% 
                group_by(current_zipcode) %>% 
                summarise(n=n(), 
                          Med_participant_income=median(participant_income),
                          Med_household_income=median(household_income)) %>% 
                drop_na() %>% 
                mutate(current_zipcode = paste0("0",as.character(current_zipcode))) 


# adding clinical columns to zipcode spatvector
temp <- as_tibble(values(zip)) %>% 
        select(zcta5ce10) %>% 
        rownames_to_column("ID") %>% 
        rename(current_zipcode = zcta5ce10)  %>% 
        left_join(., datrdce) %>% 
        mutate(Med_participant_income = replace_na(as.numeric(Med_participant_income), 0)) %>%
        mutate(Med_household_income = replace_na(as.numeric(Med_household_income), 0)) %>%
        mutate(`No. Patients` = replace_na(as.numeric(n), 0))

zip_wClinical<-zip
values(zip_wClinical)<-temp

p1b_eligibleZips_PincomeWpoints <- ggplot(data=zip_wClinical) +
  geom_sf(aes(fill=Med_participant_income), colour="lightgrey") +
  #scale_fill_manual(values=c("#f9570c","#3e6487")) +
  geom_point(aes(#color = patients, 
                  size = `No. Patients`, 
                  geometry = geometry),
                  stat = "sf_coordinates", alpha=0.5) +
  theme_minimal() + 
  scale_fill_gradient2(high="#f9570c", mid="#3e6487", low="#3e6487", 
                      na.value="transparent") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)), alpha = FALSE) 

p1b_eligibleZips_HincomeWpoints <- ggplot(data=zip_wClinical) +
  geom_sf(aes(fill=Med_household_income), colour="lightgrey") +
  #scale_fill_manual(values=c("#f9570c","#3e6487")) +
  geom_point(aes(#color = patients, 
                  size = `No. Patients`, 
                  geometry = geometry),
                  stat = "sf_coordinates", alpha=0.5) +
  theme_minimal() + 
  scale_fill_gradient2(high="#f9570c", mid="#3e6487", low="#3e6487", 
                      na.value="transparent") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)), alpha = FALSE) 



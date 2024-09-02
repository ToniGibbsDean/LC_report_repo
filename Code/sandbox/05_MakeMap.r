
library(tidyverse)
library(terra)
library(tidyterra)

svi<-terra::rast("Outputs/svi.tif")
towns<-terra::vect("Outputs/towns")

ggplot() +
  geom_spatraster(data = select(svi, overall_wgs84.tif))+
  geom_spatvector(data=towns, fill=NA)+
  scale_fill_continuous(low="thistle2", high="darkred", 
                       guide="colorbar",na.value="transparent")


ggplot() +
  geom_spatraster(data = svi) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(suffix = "º"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = "Average temperature in Castille and Leon (Spain)",
    subtitle = "Months of April, May and June"
  )
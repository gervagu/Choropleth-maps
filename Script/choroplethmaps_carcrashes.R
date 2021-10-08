#Crete a simple choropleth map and an animated choropleth map using the car crashes dataset

#Load packages
library(tidyverse)
library(rgdal)
library(sf)
library(sp)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
library(gganimate)
library(gifski)
library(png)
library(magick)


#Convert shapefile to a dataframe
gadm36_NIC_1_sf <- fortify(gadm36_NIC_1_sf)

# Construct variables
gadm36_NIC_1_sf <- gadm36_NIC_1_sf %>% 
  mutate(
    department = NAME_1, 
    department = case_when(department == "Atlántico Norte" ~ "RACCN",
                           department == "Atlántico Sur" ~ "RACCS",
                           TRUE ~ as.character(department))
    )

#Estimate incidence rate expressed in number of people affected per 100,000 of the population per year  
accident <- accidentnic %>% 
  mutate(
    percapita2000 = (accident2000 / pob2000) * 100000
    percapita2005 = (accident2005 / pob2005) * 100000
    percapita2010 = (accident2010 / pob2010) * 100000
    percapita2015 = (accident2015 / pob2015) * 100000
    percapita2020 = (accident2020 / pob2020) * 100000
    percapita2000 = round(percapita2000, digits = 2),
    percapita2005 = round(percapita2005, digits = 2),
    percapita2010 = round(percapita2010, digits = 2),
    percapita2015 = round(percapita2015, digits = 2),
    percapita2020 = round(percapita2020, digits = 2),
    department = case_when(department == "RAAN" ~ "RACCN",
                           department == "RAAS" ~ "RACCS",
                           TRUE ~ as.character(department))
  ) 

# Join the two datasets 
joined_df <- gadm36_NIC_1_sf %>% 
  select(geometry, department) %>% 
  left_join(accident, by = "department") %>% 
  filter(department != "Lago Nicaragua")

#Reshape the dataset
joined_df_reshape <- joined_df %>% 
  gather (percapita, value, percapita2000, percapita2005, percapita2010, percapita2015, percapita2020, factor_key = T)

#Change labels for the map
year_names <- c(
  `percapita2000` = "2000",
  `percapita2005` = "2005",
  `percapita2010` = "2010",
  `percapita2015` = "2015",
  `percapita2020` = "2020"
)

#Create choropleth map
mapnic <-joined_df_reshape %>% 
  ggplot(aes(fill = value), fill="gray", alpha=1) + 
  geom_sf() +
  facet_wrap(percapita  ~ ., labeller = as_labeller(year_names))+
  scale_fill_viridis_b(
    option = "magma", 
    direction = -1, 
    trans = "log", 
    breaks = c(40,80,160,320,640,1200)
  ) +
  labs(
    fill = "Car crashes",
    title = "Car crashes have increased in the last 2 decades",
    subtitle = "Car accidents per 100,000 inhabitants",
    caption = "Source: National Police Yearbooks & Population Estimation from the 2005 Census Data\n Design: Gersán Vásquez Gutiérrez"
  ) + 
  theme_ipsum_rc()+
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title=element_text(hjust = 0.5, size = 20, face="bold"),
    plot.title.position = "plot",
    plot.subtitle=element_text(face="italic", hjust = 0.5, size=17),
    plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e"),
    strip.text = element_text(hjust = 0.5, size = 14,vjust = 0.75,face="bold")) 

print(mapnic)
ggsave("carcrash.png", plot = last_plot(), width = 40, height = 25, units = "cm", dpi=600)

#Create animated choropleth map

#Create yea variable
joined_df_reshape$year <- NA
joined_df_reshape$year[joined_df_reshape$percapita == "percapita2000"] <- 2000
joined_df_reshape$year[joined_df_reshape$percapita == "percapita2005"] <- 2005
joined_df_reshape$year[joined_df_reshape$percapita == "percapita2010"] <- 2010
joined_df_reshape$year[joined_df_reshape$percapita == "percapita2015"] <- 2015
joined_df_reshape$year[joined_df_reshape$percapita == "percapita2020"] <- 2020


animated_map <-joined_df_reshape %>% 
  ggplot(aes(fill = value)) + 
  geom_sf() +
  transition_manual(year)+
  scale_fill_viridis_b(
    option = "magma", 
    direction = -1, 
    trans = "log", 
    breaks = c(40,80,160,320,640,1200)
  ) +
  labs(
    fill = "Car crashes",
    title = "Car crashes have increased in the last 2 decades",
    subtitle = "Car accidents per 100,000 inhabitants in {current_frame}",
    caption = "Source: National Police Yearbooks & Population Estimation from the 2005 Census Data\n Design: Gersán Vásquez Gutiérrez"
  ) + 
  theme_ipsum_rc()+
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title=element_text(hjust = 0.5, size = 20, face="bold"),
    plot.title.position = "plot",
    plot.subtitle=element_text(face="italic", hjust = 0.5, size=10),
    plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e"),
    strip.text = element_text(hjust = 0.5, size = 14,vjust = 0.75,face="bold"))

anim_save("map.gif", animated_map, width = 1000, height = 1000, res = 110)

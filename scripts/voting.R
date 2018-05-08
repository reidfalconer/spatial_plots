library(tidyverse)
library(broom)
library(rgeos)
library(ggthemes)
library(readr)
library(sf)
library(ggplot2)

df <- read_delim("data/IEC_data.csv", col_names = T, delim = ";" )
head(df)
str(df)
summary(df)

df %>%
  ggplot(aes(x=ANC_share_2016)) +
  geom_histogram(binwidth = 0.1)

sa <- st_read("shapefile/Local_Municipalities_2016.shp", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(4326)


# merge the data
sf_data_new <- left_join(df %>% as.data.frame(), sa %>% as.data.frame(), by = c("Admin_Level_2016"="CAT_B")) %>% 
  st_as_sf() 


# merge the data
sf_data <- sp::merge(df %>% as.data.frame(), sa %>% as.data.frame(), by = intersect("Admin_Level_2016", "CAT_B"))

head(sf_data)

ggplot() +
  geom_sf(data = sf_data, aes(fill = Change), colour = "white") +
  coord_sf(crs = 4326, datum = NA) +
  theme_void() +
  theme(legend.position = c(0.8, 0.9), legend.direction = "horizontal")

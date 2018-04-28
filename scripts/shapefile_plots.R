# load libraries
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(plyr)
library(tidyverse)
if (!require(gpclib)) install.packages("gpclib", type="source")

# read data into a SpatialPolygonsDataFrame object
sa_dc <- readOGR(dsn = "shapefile", layer = "ZAF_adm2")

# add to data a new column termed "id" composed of the rownames of data
sa_dc@data$id <- rownames(sa_dc@data)
head(sa_dc)

# Create new unemploymnet varible - this is where things could get tricky given the NIDS structure:
# I've randomly generated an umemployment metric and matching it with 'id' but you'll have to look 
# at the NIDS data and see if there is any information that you could harness to easily match on. 
# I remember this being an issue for me. 

id <- as.vector(rownames(sa_dc@data))
unemployment <-  as.vector(runif(52, 0, 20))
unemployment_df <- as.data.frame(cbind(id, unemployment))
head(unemployment_df)

#unemployment_df$unemployment=as.numeric(levels(unemployment_df$unemployment))[unemployment_df$unemployment]

# attaching unemployment metric to shapefile attributes 
sa_dc@data  <- join(sa_dc@data, unemployment_df, by="id")

# create a data.frame from spatial object
dc_points <- fortify(sa_dc, region = "id")

# merge the "fortified" data with the data from spatial object
sa_dc_df <- merge(dc_points, sa_dc@data, by = "id")
head(sa_dc_df)

# Or be tidy: 
# library(plyr)
# watershedDF <- join(watershedPoints, dataProjected@data, by = "id")

# keep the necessary varibles 
sa_dc_df <- sa_dc_df %>% 
  select(id, long, lat, piece, group, hole, ID_1, NAME_1, ID_2, NAME_2, CCA_2, unemployment)

head(sa_dc_df)

## Plotting 
map <- ggplot(data = sa_dc_df, aes(x=long, y=lat, group = group,
                                   fill = unemployment)) +
  geom_polygon()  +
  geom_path(data = sa_dc_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', size = .2) +
  coord_equal()

map_projected <- map +
  coord_map()

print(map_projected)


# Using ssplot instead of ggplot - this means one doesnt have to make a df. 
spplot(sa_dc, "unemployment", main = "District Councils", sub = "Unemployment", col = "transparent")

#color palette  
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 8, name = "Blues") #color selection no.8 #of blues
spplot(sa_dc, "unemployment", col.regions = my.palette, cuts = 6, col = "transparent") #6 shades of blue




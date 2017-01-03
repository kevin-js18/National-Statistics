library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maptools)
library(ggmap)
library(scales)
library(RColorBrewer)
library(Cairo)

setwd("~/President-Obama-Evaluation/Data files/Immigration data")

dd_data <- read_csv("DAPA-DACA.csv")
dd_df <- data.frame(dd_data)

state.shp <- readShapePoly("Shapefiles/cb_2015_us_state_20m/cb_2015_us_state_20m.shp")
coord.df <- data.frame(coordinates(state.shp))
names(coord.df) <- c("Longitude", "Latitude")
for (i in 1:length(state.shp$NAME)) {
  state.shp$Long[i] <- coord.df$Longitude[i]
  state.shp$Lat[i] <- coord.df$Latitude[i]
}
state.shp <- subset(state.shp, state.shp$NAME != "Alaska")
state.shp <- subset(state.shp, state.shp$NAME != "Hawaii")
state.shp <- subset(state.shp, state.shp$NAME != "Puerto Rico")
state.shp <- subset(state.shp, state.shp$NAME != "District of Columbia")

names(state.shp)[6] <- "id"
names(dd_data)[1] <- "id"

state.shp.f <- fortify(state.shp, region = "id")

merge.shp <- merge(state.shp.f, dd_data, by = "id", all.x = TRUE)
final.plot <- merge.shp[order(merge.shp$order), ]
for (i in 1:length(final.plot$id)) {
  final.plot$clong[i] <- state.shp$Long[grep(final.plot$id[i], state.shp$id)]
  final.plot$clat[i] <- state.shp$Lat[grep(final.plot$id[i], state.shp$id)]
}

ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = Percent), 
               col = "white") +
  coord_map() +
  scale_fill_gradient(low = "white", high = "#2093ea", limits = c(0, 60)) +
  geom_text(data = final.plot, aes(x = clong, y = clat, label = paste(Percent, "%", sep="")), size = 3) +
  theme_nothing(legend = TRUE)


# SOURCES:

# DAPA/DACA data: Migration Policy Institute, http://www.migrationpolicy.org/article/frequently-requested-statistics-immigrants-and-immigration-united-states
# Shapefile: US Census Bureau, https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html

library(ggplot2)
library(maptools)
library(dplyr)
library(tidyr)
library(readr)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Election data")

elec_data <- read_csv("elec_data.csv")
elec_df <- data.frame(elec_data)

# Create separate data frames for Alaska, as the data is organized by Congressional District rather than County
alaska_df08 <- subset(elec_df, elec_df$year == 2008 & elec_df$state == "AK")
alaska_df12 <- subset(elec_df, elec_df$year == 2012 & elec_df$state == "AK")

# Assign each row in Alaska data frames a number 1-40, each representing a unique Legislative District
for (i in 1:40) {
  alaska_df08$id[i] <- i
  alaska_df12$id[i] <- i
}

# Filter Alaskan results from the main dataframe, as Alaskan results shown by Legislative District rather than county

elec_df08 <- subset(elec_df, elec_df$year == 2008 & elec_df$state != "AK")
elec_df12 <- subset(elec_df, elec_df$year == 2012 & elec_df$state != "AK")

# Handle naming issues in election dataframe

city_ind_1 <- grep("Charles City", elec_df08$county)
city_ind_2 <- grep("James City", elec_df08$county)
city_ind_3 <- grep("Carson City", elec_df08$county)

city_ind_4 <- grep("Charles City", elec_df12$county)
city_ind_5 <- grep("James City", elec_df12$county)
city_ind_6 <- grep("Carson City", elec_df12$county)

# Keep only the name of the county, remove extraneous info e.g. "County, Parish, City"
elec_df08$county <- sapply(strsplit(elec_df08$county, " Count"), "[", 1)
elec_df08$county <- sapply(strsplit(elec_df08$county, " Cit"), "[", 1)
elec_df08$county <- sapply(strsplit(elec_df08$county, " Paris"), "[", 1)

elec_df12$county <- sapply(strsplit(elec_df12$county, " Cit"), "[", 1)
elec_df12$county <- sapply(strsplit(elec_df12$county, " Paris"), "[", 1)

elec_df08$county[city_ind_1] <- "Charles City"
elec_df08$county[city_ind_2] <- "James City"
elec_df08$county[city_ind_3] <- "Carson City"

elec_df12$county[city_ind_4] <- "Charles City"
elec_df12$county[city_ind_5] <- "James City"
elec_df12$county[city_ind_6] <- "Carson City"

# Handle naming discrepencies
# "Shannon" to "Oglala Lakota"
elec_df08$county[2389] <- "Oglala Lakota"
elec_df12$county[2388] <- "Oglala Lakota"

# "La Salle" or "Lasalle" to "LaSalle" (for IL and LA) or "La Salle" (for TX)
elec_df08$county[1114] <- "LaSalle"
elec_df08$county[616] <- "LaSalle"
elec_df08$county[2636] <- "La Salle"

elec_df12$county[615] <- "LaSalle"
elec_df12$county[1113] <- "LaSalle"
elec_df12$county[2635] <- "La Salle"

# "Dona Ana" to "Doña Ana"
elec_df08$county[1774] <- "Doña Ana"
elec_df12$county[1773] <- "Doña Ana"

# "Saint" to "St."
elec_df08$county <- gsub("Saint", "St.", elec_df08$county)
elec_df08$county[1548] <- "Ste. Genevieve"

elec_df12$county <- gsub("Saint", "St.", elec_df12$county)
elec_df12$county[1547] <- "Ste. Genevieve"

# "DeBaca" to "De Baca"
elec_df08$county[1773] <- "De Baca"
elec_df12$county[1772] <- "De Baca"

# Other misc fixes
elec_df12$county[58] <- "St. Clair"
elec_df12$county[144] <- "St. Francis"
elec_df12$county[713] <- "LaPorte"
elec_df12$county[1229] <- "Grand Traverse"
elec_df12$county[1485] <- "DeKalb"
elec_df12$county[1593] <- "Lewis and Clark"
elec_df12$county[608] <- "Jo Daviess"
elec_df12$county[585] <- "De Witt"
elec_df12$county[2141] <- "Le Flore"
elec_df12$county[2838] <- "King and Queen"

# Change working directory
setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Election data/Shapefiles/cb_2015_us_county_20m")

# Read in shapefiles. Shapefiles will provide coordinates for each county to be plotted
election.shp <- readShapeSpatial("cb_2015_us_county_20m.shp")
election.shp <- subset(election.shp, election.shp$STATEFP != 72)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Election data/Shapefiles/cb_2015_02_sldl_500k")
alaska.district.shp <- readShapeSpatial("cb_2015_02_sldl_500k.shp")

# Change name of Alaska shapefile column that gives Congressional District numbers so we can merge data properly. 
names(alaska.district.shp)[5] <- "id"

# Resolve case issues
elec_df12$county <- tolower(elec_df12$county)
elec_df08$county <- tolower(elec_df08$county)
election.shp$NAME <- tolower(election.shp$NAME)

# Combining county and state names into one column to give each county a unique id in the format of "County, State" that can be used when we populate the map
elec_df08 <- unite(elec_df08, id, county, state, sep = ", ")
elec_df12 <- unite(elec_df12, id, county, state, sep = ", ")

# Giving negative values to Republican percent victories. This will be the basis of the color scale used in the graph
for(i in 1:length(elec_df08$pct_winner)) {
  if (elec_df08$winner[i] == "rep") {
    elec_df08$pct_winner[i] <- -1 * elec_df08$pct_winner[i]
  }
}

for(i in 1:length(elec_df12$pct_winner)) {
  if (elec_df12$winner[i] == "rep") {
    elec_df12$pct_winner[i] <- -1 * elec_df12$pct_winner[i]
  }
}

for(i in 1:length(alaska_df08$pct_winner)) {
  if (alaska_df08$winner[i] == "rep") {
    alaska_df08$pct_winner[i] <- -1 * alaska_df08$pct_winner[i]
  }
}

for (i in 1:length(alaska_df12$pct_winner)) {
  if (alaska_df12$winner[i] == "rep") {
    alaska_df12$pct_winner[i] <- -1 * alaska_df12$pct_winner[i]
  }
}

# Add state codes to county names in the shapefile to make a unique identifier for each county in the format of "County, State"
state_df <- data.frame(unique(election.shp$STATEFP))
state_df <- arrange(state_df, unique.election.shp.STATEFP.)
state_df$alpha <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", 
                    "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", 
                    "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
                    "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
                    "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", 
                    "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", 
                    "WV", "WI", "WY")

state_df$unique.election.shp.STATEFP. <- as.character(state_df$unique.election.shp.STATEFP.)
election.shp$STATEFP <- as.character(election.shp$STATEFP)
election.shp$NAME <- as.character(election.shp$NAME)

election.shp.df <- data.frame(election.shp)

for(i in 1:length(election.shp$STATEFP)) {
  election.shp.df$State[i] <- state_df$alpha[grep(election.shp$STATEFP[i], state_df$unique.election.shp.STATEFP.)]
}

election.shp.df <- unite(election.shp.df, id, NAME, State, sep = ", ")
election.shp$id <- election.shp.df$id

# Preparing Alaska files

# Turn the shapefile into a plottable dataframe
alaska.districts.f <- fortify(alaska.district.shp, region = "id")

# Some points in Alaska and Hawaii have coordinate values that are positive when they should be negative. This fixes the issue
for (i in 1:length(alaska.districts.f$long)) {
  if (alaska.districts.f$long[i] > 100) {
    alaska.districts.f$long[i] <- -alaska.districts.f$long[i]
  }
}

# Merge election data with shapefile on the unique identifer we created for each county. This will ensure that each county plotted will have it's corresponding percent value.
alaska08.shp <- merge(alaska.districts.f, alaska_df08, by = "id", all.x=TRUE)
alaska12.shp <- merge(alaska.districts.f, alaska_df12, by = "id", all.x=TRUE)

# Order the coordinates so that they will be mapped in the correct order.
alaska08.plot <- alaska08.shp[order(alaska08.shp$order), ]
alaska12.plot <- alaska12.shp[order(alaska12.shp$order), ]


# Preparing continental United States and Hawaii files

# Turn the shapefile into a plottable dataframe
election.shp.f <- fortify(election.shp, region = "id")

# Merge election data with shapefile on the unique identifer we created for each county. This will ensure that each county plotted will have it's corresponding percent value.
merge08.shp <- merge(election.shp.f, elec_df08, by = "id", all.x=TRUE)
merge12.shp <- merge(election.shp.f, elec_df12, by = "id", all.x=TRUE)

# Some points in Alaska and Hawaii have coordinate values that are positive when they should be negative. This fixes the issue
for (i in 787:length(merge08.shp$long)) {
  if (merge08.shp$long[i] > 100) {
    merge08.shp$long[i] <- -merge08.shp$long[i]
  }
}

for (i in 1:length(merge12.shp$long)) {
  if (merge12.shp$long[i] > 100) {
    merge12.shp$long[i] <- -merge12.shp$long[i]
  }
}

# Create subsets of the coordinate data so that we have separate files for continental United States and Hawaii
hawaii08.shp <- subset(merge08.shp, merge08.shp$long < -130 & merge08.shp$lat < 38)
hawaii12.shp <- subset(merge12.shp, merge12.shp$long < -130 & merge12.shp$lat < 38)
merge08.shp <- subset(merge08.shp, merge08.shp$long > - 130)
merge12.shp <- subset(merge12.shp, merge12.shp$long > - 130)

# Order the coordinates so that they will be mapped in the correct order.
final08.plot <- merge08.shp[order(merge08.shp$order), ]
final12.plot <- merge12.shp[order(merge12.shp$order), ]
hawaii08.plot <- hawaii08.shp[order(hawaii08.shp$order), ]
hawaii12.plot <- hawaii12.shp[order(hawaii12.shp$order), ]

# Plots
# ggplot() function will plot the coordinate points for each map and will fill 
# in the counties based on the election result. 
# We adjust the theme so that there are no visible axes or gridlines

ggplot() +
  geom_polygon(data = alaska08.plot,
               aes(x = long, y = lat, group = group, fill = pct_winner),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_gradient2(name = "Percent", low = "#ff3f3f", mid = "white", high = "#3c99fc", midpoint = 0, limits = c(-100, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Alaska Election Results, 2008")

ggplot() +
  geom_polygon(data = alaska12.plot,
               aes(x = long, y = lat, group = group, fill = pct_winner),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_gradient2(name = "Percent", low = "#ff3f3f", mid = "white", high = "#3c99fc", midpoint = 0, limits = c(-100, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Alaska Election Results, 2012")

ggplot() +
  geom_polygon(data = final08.plot,
               aes(x = long, y = lat, group = group, fill = pct_winner),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_gradient2(name = "Percent", low = "#ff3f3f", mid = "white", high = "#3c99fc", midpoint = 0, limits = c(-100, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("General Election Results, 2008")

ggplot() +
  geom_polygon(data = final12.plot,
               aes(x = long, y = lat, group = group, fill = pct_winner),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_gradient2(name = "Percent", low = "#ff3f3f", mid = "white", high = "#3c99fc", midpoint = 0, limits = c(-100, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("General Election Results, 2012")

ggplot() +
  geom_polygon(data = hawaii08.plot,
               aes(x = long, y = lat, group = group, fill = pct_winner),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_gradient2(name = "Percent", low = "#ff3f3f", mid = "white", high = "#3c99fc", midpoint = 0, limits = c(-100, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Hawaii Election Results, 2008")

ggplot() +
  geom_polygon(data = hawaii12.plot,
               aes(x = long, y = lat, group = group, fill = pct_winner),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_gradient2(name = "Percent", low = "#ff3f3f", mid = "white", high = "#3c99fc", midpoint = 0, limits = c(-100, 100)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Hawaii Election Results, 2012")


# ACKNOWLEDGEMENTS

# The methodology used to plot maps using ggplot2 was based on this blog post: 
# https://www.r-bloggers.com/mapping-with-ggplot-create-a-nice-choropleth-map-in-r/

# All shapefiles came from the United States Census Bureau's MAF/TIGER geographic database

# Election results for each county came from the following Github repository:
# https://github.com/helloworlddata/us-presidential-election-county-results







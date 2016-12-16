library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Election data")

elec_data <- read_csv("elec_data.csv")
elec_df <- data.frame(elec_data)

elec_df08 <- subset(elec_df, elec_df$year == 2008 & elec_df$state != "AK")
elec_df12 <- subset(elec_df, elec_df$year == 2012 & elec_df$state != "AK")

city_ind_1 <- grep("Charles City", elec_df08$county)
city_ind_2 <- grep("James City", elec_df08$county)
city_ind_3 <- grep("Carson City", elec_df08$county)

city_ind_4 <- grep("Charles City", elec_df12$county)
city_ind_5 <- grep("James City", elec_df12$county)
city_ind_6 <- grep("Carson City", elec_df12$county)

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

# "DeBaca" to "De Baca"
elec_df08$county[1773] <- "De Baca"
elec_df12$county[1772] <- "De Baca"

# Change working directory
setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Election data/Shapefiles/cb_2015_us_county_20m")

# Read in shapefiles
election.shp <- readShapeSpatial("cb_2015_us_county_20m.shp")
election.shp <- subset(election.shp, election.shp$STATEFP != 72)

# Combining county and state names into one column to give each county a unique id that can be used when we populate the map
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

election.shp.f <- fortify(election.shp, region = "id")

merge08.shp <- merge(election.shp.f, elec_df08, by = "id", all.x=TRUE)
merge12.shp <- merge(election.shp.f, elec_df12, by = "id", all.x=TRUE)

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


merge08.shp <- subset(merge08.shp, merge08.shp$long > - 130)
merge12.shp <- subset(merge12.shp, merge12.shp$long > - 130)

final08.plot <- merge08.shp[order(merge08.shp$order), ]
final12.plot <- merge12.shp[order(merge12.shp$order), ]

ggplot() +
  geom_polygon(data = final08.plot,
               aes(x = long, y = lat, group = group, fill = pct_winner),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_gradient2(name = "Percent", low = "#ff3f3f", mid = "white", high = "#3c99fc", midpoint = 0) +
  theme_nothing(legend = TRUE) +
  ggtitle("General Election Results, 2008")

ggplot() +
  geom_polygon(data = final12.plot,
               aes(x = long, y = lat, group = group, fill = pct_winner),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_gradient2(name = "Percent", low = "#ff3f3f", mid = "white", high = "#3c99fc", midpoint = 0) +
  theme_nothing(legend = TRUE) +
  ggtitle("General Election Results, 2012")











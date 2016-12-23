library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Energy, Environment data/CAIT_Country_GHG_Emissions_-_csv_1216/CAIT Country GHG Emissions - csv 1216")

emissions <- read_csv("CAIT Country CO2 Emissions.csv", skip = 1)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Energy, Environment data")

coal <- read_csv("MER_T06_02.csv")
coal <- subset(coal, coal$YYYYMM >= 197013 & coal$Description == "Coal Consumption, Total" & coal$YYYYMM <= 201513)
coal$YYYYMM <- as.integer(coal$YYYYMM / 100)
coal$Value <- as.double(coal$Value)

coal <- coal %>%
  group_by(YYYYMM) %>%
  summarise(total = sum(Value))

usa_emissions <- subset(emissions, emissions$Country == "United States" & emissions$Year >= 1970)

ggplot(usa_emissions, aes(x = usa_emissions$Year, y = usa_emissions$`Total CO2 Emissions Excluding Land-Use Change and Forestry (MtCO2)`)) +
  geom_line() +
  geom_vline(xintercept = 2009)

ggplot(coal, aes(x = YYYYMM, y = total)) +
  geom_line() +
  geom_vline(xintercept = 2009)

# SOURCES

# Emissions data: World Resources Institute: http://www.wri.org/resources/data-sets/cait-historical-emissions-data-countries-us-states-unfccc
# Coal consumption data: US Energy Information Administration: http://www.eia.gov/coal/data.php#consumption


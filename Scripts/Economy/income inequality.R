# This graph shows the change in percent share of wealth in America for the five socioeconomic quintiles. Lowest quintile
# represents the lowest 20% of earners whereas highest quintile represents the highest 20% of earners.

library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)

share <- read_csv("income inequality.csv")
share <- share[, -5]

share.g <- share %>%
  gather(Year, `Percent Share`, `2015`:`1967`)

share.g$Year <- as.numeric(share.g$Year)

names(share.g)[1] <- "Quintiles"

share.g$Quintiles <- factor(share.g$Quintiles, levels = c("Highest quintile percent share", 
                                                          "Fourth quintile percent share",
                                                          "Third quintile percent share", 
                                                          "Second quintile percent share",
                                                          "Lowest quintile percent share"))

share.g <- share.g[order(share.g$Quintiles), ]

ggplot(share.g, aes(x = Year, y = `Percent Share`, col = Quintiles, group = Quintiles)) +
  geom_line() 

# SOURCES

# US Census Bureau: http://www.census.gov/data/tables/2016/demo/income-poverty/p60-256.html

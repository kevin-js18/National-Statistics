library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Immigration data")

naturalizations <- read_csv("naturalization.csv")
naturalizations <- naturalizations[order(naturalizations$Year), ]

naturalizations <- gather(naturalizations, Status, Applications, Admitted:Denied)
naturalizations00 <- subset(naturalizations, naturalizations$Year >= 2000)
naturalizations00 %>%
  group_by(Year) %>%
  ggplot(aes(x = Year, y = Applications, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Applications", labels = comma) +
  scale_x_continuous(breaks = 2000:2015) +
  ggtitle("Naturalizations") +
  scale_fill_manual(values = c("#4bfc51", "#ff6666"))

# SOURCES

# US Department of Homeland Security, Yearbook of Immigration Statistics, 2009 - 2015
# https://www.dhs.gov/immigration-statistics/yearbook/2015
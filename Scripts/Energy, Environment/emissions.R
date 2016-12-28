library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(Cairo)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Energy, Environment data")

emissions <- read_csv("emissions.csv")
emissions <- subset(emissions, emissions$`Energy Source` == "All Sources")

emissions <- emissions %>%
  group_by(Year) %>%
  summarise(total_co2 = sum(CO2),
            total_so2 = sum(SO2),
            total_nox = sum(Nox))

coal <- read_csv("MER_T06_02.csv")
coal <- subset(coal, coal$YYYYMM >= 197013 & coal$Description == "Coal Consumption, Total" & coal$YYYYMM <= 201513)
coal$YYYYMM <- as.integer(coal$YYYYMM / 100)
coal$Value <- as.double(coal$Value)

coal <- coal %>%
  group_by(YYYYMM) %>%
  summarise(total = sum(Value))

ggplot(emissions, aes(x = Year, y = total_co2)) +
  geom_line() +
  geom_vline(xintercept = 2009) +
  xlab("Year") +
  ylab(bquote("Total "~CO[2]~" Emissions (in metric tons)")) +
  ggtitle("CO2") +
  scale_y_continuous(labels = comma)

ggplot(emissions, aes(x = Year, y = total_so2)) +
  geom_line() +
  geom_vline(xintercept = 2009) +
  xlab("Year") +
  ylab(bquote("Total "~SO[2]~" Emissions (in metric tons)")) +
  ggtitle("SO2") +
  scale_y_continuous(labels = comma)

ggplot(emissions, aes(x = Year, y = total_nox)) +
  geom_line() +
  geom_vline(xintercept = 2009) +
  xlab("Year") +
  ylab(bquote("Total "~NO[x]~" Emissions (in metric tons)")) +
  ggtitle("NOx") +
  scale_y_continuous(labels = comma)

ggplot(coal, aes(x = YYYYMM, y = total)) +
  geom_line() +
  geom_vline(xintercept = 2009) + 
  xlab("Year") +
  ylab("Total Coal Consumption (in thousands of short tons)")

# SOURCES

# Emissions data: US Department of Energy, via Data.gov: https://catalog.data.gov/dataset/annual-1990-2011-u-s-electric-power-industry-estimated-emissions-by-state
# Coal consumption data: US Energy Information Administration: http://www.eia.gov/coal/data.php#consumption


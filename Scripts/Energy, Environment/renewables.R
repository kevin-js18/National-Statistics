library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Energy, Environment data")

renewables <- read_csv("Renewables by source.csv")

renewables.prod <- gather(renewables, Source, `Production (in trillions of BTUs)`, -(Year:`Total Biomass Energy Production`), -(`Hydroelectric Power Consumption`:`Total Renewable Energy Consumption`))

renewables.consum <- gather(renewables, Source, `Consumption (in trillions of BTUs)`, `Hydroelectric Power Consumption`:`Wind Energy Consumption`, `Total Biomass Energy Consumption`, -(`Biofuels Production`:`Total Renewable Energy Production`), -(`Wood Energy Consumption`:`Biofuels Consumption`), -(`Total Renewable Energy Consumption`))

renewables.consum$Source <- factor(renewables.consum$Source, levels = c("Total Biomass Energy Consumption", "Hydroelectric Power Consumption",
                                                                        "Wind Energy Consumption", "Solar Energy Consumption", "Geothermal Energy Consumption"))

renewables.consum <- renewables.consum[order(renewables.consum$Source), ]

ggplot(renewables.consum, aes(x = Year, y = `Consumption (in trillions of BTUs)`, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = 2000:2015) +
  scale_fill_manual(name = "Source",
                    values = c("#074F57", "#0EB1D2", "#52B788", "#F5A658", "#C1CC99"),
                    labels = c("Biomass", "Hydroelectric", "Wind", "Solar", "Geothermal")) +
  ggtitle("Consumption") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "grey"))
  
ggplot(renewables.prod, aes(x = Year, y = `Production (in trillions of BTUs)`)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 2000:2015) +
  ggtitle("Production") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "grey"))

# SOURCES

# US Energy Information Administration
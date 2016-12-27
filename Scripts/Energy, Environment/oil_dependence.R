library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/aet/Desktop/School/R projects/Final project/President-Obama-Evaluation/Data files/Energy, Environment data")

imports <- read_csv("Oil imports.csv")
production <- read_csv("Oil production.csv")
exports <- read_csv("Oil exports.csv")

production <- production[, 1:2]
exports <- exports[, c(1, 3)]

imports <- separate(imports, Date, c("Month", "Year"), sep = "-")
production <- separate(production, Date, c("Month", "Year"), sep = "-")
exports <- separate(exports, Date, c("Month", "Year"), sep = "-")

for (i in 1:length(production$`U.S. Field Production of Crude Oil (Thousand Barrels)`)) {
  production$`U.S. Field Production of Crude Oil (Thousand Barrels)`[i] <- production$`U.S. Field Production of Crude Oil (Thousand Barrels)`[i] - exports$`U.S. Exports of Crude Oil (Thousand Barrels)`[i]
}

production <- production %>%
  group_by(Year) %>%
  summarise(total = sum(`U.S. Field Production of Crude Oil (Thousand Barrels)`))

production <- subset(production, production$Year >= 2000 & production$Year < 2016)

names(imports) <- sapply(strsplit(names(imports), " of Crude Oil \\(Thousand Barrels"), "[", 1)
names(imports) <- sapply(strsplit(names(imports), "Imports from "), "[", 2)
names(imports)[c(1, 2, 3)] <- c("Month", "Year", "Total")


ranking <- imports %>%
  gather(Country, Oil, `Persian Gulf Countries`:Yemen) %>%
  group_by(Country)

for (i in 1:length(ranking$Oil)) {
  if (is.na(ranking$Oil[i])) {
    ranking$Oil[i] <- 0
  }
}

ranking$Oil <- as.integer(ranking$Oil)

ranking <- subset(ranking, ranking$Year >= 2000 & ranking$Year < 2016)

by_year <- ranking

ranking <- ranking %>%
  group_by(Country) %>%
  summarise(total_barrels = sum(Oil)) %>%
  arrange(desc(total_barrels))

by_year <- subset(by_year, by_year$Country == "Canada" |
                    by_year$Country == "Saudi Arabia" |
                    by_year$Country == "Mexico" |
                    by_year$Country == "Venezuela" |
                    by_year$Country == "Nigeria")

by_year_spread <- spread(by_year, Country, Oil)
by_year_spread <- by_year_spread %>%
  mutate(`Rest of World` = Total - (Canada + Mexico + Nigeria + `Saudi Arabia` + Venezuela)) 

by_year_final <- by_year_spread %>%
  gather(Country, Total_barrels, Canada:`Rest of World`) %>%
  group_by(Country, Year) %>%
  summarise(Barrels = sum(Total_barrels))

by_year_final$Country <- factor(by_year_final$Country, levels = c("Canada", "Saudi Arabia", "Mexico", "Venezuela", "Nigeria", "Rest of World"))
by_year_final <- by_year_final[order(by_year_final$Country), ]

ggplot(by_year_final, aes(x = Year, y = Barrels, fill = Country, group = Country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Countries", 
                      values = c("#293132", "#23B5D3", "#6E7E85", "#B7CECE", "#9C92A3", "#DFD6A7"),
                      breaks = c("Rest of World", "Nigeria", "Venezuela", "Mexico", "Saudi Arabia", "Canada"), 
                      labels = c("Rest of the World", "Nigeria", "Venezuela", "Mexico", "Saudi Arabia", "Canada")) +
  scale_y_continuous(name="Crude oil (in thousands of barrels)", labels = comma) 
  

ggplot(production, aes(x = Year, y = total)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name="Crude oil (in thousands of barrels)", labels = comma) 

# SOURCES

# US Energy Information Administration

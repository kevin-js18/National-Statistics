library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)
library(shiny)

setwd("~/President-Obama-Evaluation/Data files/Immigration data/Permanent residents")

data09 <- read_csv("2009.csv")
data10 <- read_csv("2010.csv")
data11 <- read_csv("2011.csv")
data12 <- read_csv("2012.csv")
data13 <- read_csv("2013.csv")
data14 <- read_csv("2014.csv")
data15 <- read_csv("2015.csv")

df_list <- list(d15 = data15, d14 = data14, d13 = data13, 
                d12 = data12, d11 = data11, d10 = data10, 
                d9 = data09)

for (i in 1:length(df_list)) {
  names(df_list[[i]])[1] <- "Country"
  names(df_list[[i]])[3] <- "Family"
  names(df_list[[i]])[4] <- "Employment"
  names(df_list[[i]])[5] <- "Immediate Relative"
  names(df_list[[i]])[7] <- "Asylum or Refugee"
}

for (i in 1:length(df_list)) {
  for (j in 2:length(df_list[[i]])) {
    for (k in 1:(length(df_list[[i]][[j]]) - 1)) {
      if (df_list[[i]][k, j] == "-" | df_list[[i]][k, j] == "D" | is.na(df_list[[i]][k, j])) {
        df_list[[i]][k, j] <- "0"
      }
    }
    
    df_list[[i]][j] <- as.numeric(gsub(",", "", df_list[[i]][[j]]))
  }
}

for (i in 2:length(df_list)) {
  for (j in 1:length(df_list$d15$Total)) {
    if (length(df_list[[i]]$Country[grep(df_list$d15$Country[j], df_list[[i]]$Country)]) != 0) {
      df_list$d15$Total[j] <- df_list$d15$Total[j] + df_list[[i]]$Total[grep(df_list$d15$Country[j], df_list[[i]]$Country)] 
      df_list$d15$Family[j] <- df_list$d15$Family[j] + df_list[[i]]$Family[grep(df_list$d15$Country[j], df_list[[i]]$Country)]
      df_list$d15$Employment[j] <- df_list$d15$Employment[j] + df_list[[i]]$Employment[grep(df_list$d15$Country[j], df_list[[i]]$Country)]
      df_list$d15$`Immediate Relative`[j] <- df_list$d15$`Immediate Relative`[j] + df_list[[i]]$`Immediate Relative`[grep(df_list$d15$Country[j], df_list[[i]]$Country)]
      df_list$d15$Diversity[j] <- df_list$d15$Diversity[j] + df_list[[i]]$Diversity[grep(df_list$d15$Country[j], df_list[[i]]$Country)]
      df_list$d15$`Asylum or Refugee`[j] <- df_list$d15$`Asylum or Refugee`[j] + df_list[[i]]$`Asylum or Refugee`[grep(df_list$d15$Country[j], df_list[[i]]$Country)]
      df_list$d15$Other[j] <- df_list$d15$Other[j] + df_list[[i]]$Other[grep(df_list$d15$Country[j], df_list[[i]]$Country)]
    }
  }
}

all_lpr <- df_list$d15

top_10 <- arrange(all_lpr, desc(Total))[1:11, ]

for (i in 2:length(top_10)) {
  top_10[1, i] <- top_10[1, i] - sum(top_10[2:11, i])
}

top_10 <- top_10[, c(1, 2, 5, 3, 4, 7, 6, 8)]
top_10 <- top_10[c(2:11, 1), ]
top_10 <- top_10[, -2]

top_10.g <- top_10 %>%
  gather(`Admission Class`, Total, `Immediate Relative`:Other) %>%
  group_by(Country)

just.top_10 <- subset(top_10.g, top_10.g$Country != "Total")

lpr.plot <- plot(
  gvisSankey(just.top_10, from = "Country", to = "Admission Class", weight = "Total",
             options = list(width = 1200, height = 1000,
                            sankey = "{ iterations:0 }"))
)



# SOURCES

# US Department of Homeland Security, Yearbook of Immigration Statistics, 2009 - 2015
# https://www.dhs.gov/immigration-statistics/yearbook/2015



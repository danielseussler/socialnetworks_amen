# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# GDP pc Data from World Development Indicators

library(countrycode)
library(tidyverse)
library(WDI)
library(xergm.common)


# Data Setup -------------------------------------------------------------------
data("alliances")

countrycowc <- get.vertex.attribute(allyNet[["2000"]], "vertex.names")

countryname <- countrycode(countrycowc,
  origin = "cowc", destination = "country.name",
  custom_match = c(
    "GFR" = "German Federal Republic",
    "CON" = "Congo",
    "DRC" = "Democratic Republic of the Congo"
  )
)

countryiso2c <- countrycode(countrycowc,
  origin = "cowc", destination = "iso2c",
  custom_match = c("CZE", "GDR", "GFR", "YAR", "YPR", "YUG" = "YU")
)

country <- data.frame("index" = 1:164,
                      "iso2c" = countryiso2c, 
                      "cowc" = countrycowc)

former <- c("YAR", "YPR", "GFR", "GDR", "CZE")
formerIndex <- match(former, countrycowc)
current <- !(countrycowc %in% former)


# World Development Indicators - World Bank ------------------------------------
WDIsearch('gdp.*capita.*constant')
data = WDI(indicator='NY.GDP.PCAP.KD', country = "all", start = 1981, end = 2000)

data <- data %>%
  filter(year == 2000) %>%
  select(iso2c, NY.GDP.PCAP.KD)  

GDP <- merge(country[current, ], data, by = "iso2c", all.x = TRUE)
GDP[is.na(GDP$NY.GDP.PCAP.KD), "NY.GDP.PCAP.KD"] <- 1
GDP[, "NY.GDP.PCAP.KD"] <- log(GDP[, "NY.GDP.PCAP.KD"])
GDP <- GDP[order(GDP$index), c("cowc", "NY.GDP.PCAP.KD")]
head(GDP)
rownames(GDP) <- NULL
GDP <- column_to_rownames(GDP, var = "cowc")

saveRDS(GDP, file = "data/logGDP.rds")

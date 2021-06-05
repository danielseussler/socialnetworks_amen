# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model

# Helper Function to determine GeoDistance between two countries by capital.
# Tutorial: https://www.r-bloggers.com/2019/10/geographic-distance/

library(countrycode)
library(maps)
library(sf)
library(tidyverse)
library(units)
library(xergm.common)

data("alliances")


# Determine Capital Cities -----------------------------------------------------
countrycowc <- get.vertex.attribute(allyNet[["2000"]], "vertex.names")
countryname <- countrycode(countrycowc,
  origin = "cowc", destination = "country.name",
  custom_match = c(
    "GFR" = "German Federal Republic",
    "CON" = "Congo",
    "DRC" = "Democratic Republic of the Congo"
  )
)

former <- c("YAR", "YPR", "GFR", "GDR", "CZE")
formerIndex <- match(former, countrycowc)
current <- !(countrycowc %in% former)

country <- data.frame( "index" = 1:sum(current), "cowc" = countrycowc[current], "name" = countryname[current])
rm(countrycowc, countryname)


head(world.cities)
capital <- filter(world.cities, capital == 1)

countrycapital <- merge(country, capital,
  by.x = "name", by.y = "country.etc",
  all.x = TRUE, all.y = FALSE
)


# Delete Duplicates Manual Match
countrycapital <- countrycapital[!duplicated(countrycapital$name), ]

missingIndex <- is.na(countrycapital$name.y)
countrycapital[missingIndex, ]

missingdata <- data.frame(
  "countryname" = countrycapital[missingIndex, ]$name,
  "capital" = c(
    "Sarajevo", "Yamoussoukro", "Prague", "Kinshasa", "Mbabane", "Suva", "Rangoon",
    "Pyongyang", "Skopje", "Soul", "Port of Spain", "London", "Washington", "Belgrade"
  )
)

missingcities <- world.cities %>%
  filter(name %in% missingdata$capital)

missingcities <- as.data.frame(missingcities)[-c(3, 4, 15, 16), -2]
missing <- merge(missingdata, missingcities, by.x = "capital", by.y = "name")

countrycapital[missingIndex, 4:8] <-  missing[order(missing$countryname), c(1,3,4,5,6)]
any(is.na(countrycapital$name.y))


# Compute Distance -------------------------------------------------------------
countrycapital <- countrycapital %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  arrange(index)

GeoDistance <- st_distance(countrycapital)
any(is.na(GeoDistance))

GeoDistance <- set_units(GeoDistance, "km")
colnames(GeoDistance) <- rownames(GeoDistance) <- country$cowc


# Case: Shared Borders ---------------------------------------------------------
GeoDistance[contigMat[current, current] == 1] <- 0

saveRDS(GeoDistance, file = "data/GeoDistance.rds")

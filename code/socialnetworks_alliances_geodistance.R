# Seminar of Statistical Modeling of Social Networks
# Helper Function to determine GeoDistance between two countries, by capital.
# Tutorial: https://www.r-bloggers.com/2019/10/geographic-distance/

library(xergm.common)
library(countrycode)
library(sf)
library(maps)
library(units)
library(tidyverse)
data("alliances")


# Determine Capital Cities -----------------------------------------------------
countries <- colnames(contigMat)
country_name <- countrycode(countries,
  origin = "cowc", destination = "country.name",
  custom_match = c("GFR" = "German Federal Republic")
)
country_name <- data.frame( "Index" = 1:164, "Name" = country_name)

head(world.cities)
capitals <- world.cities %>% filter(capital == 1)

country_capital <- merge(country_name, capitals,
  by.x = "Name", by.y = "country.etc",
  all.x = TRUE, all.y = FALSE
)


# Delete Duplicates
# Manual Match
country_capital <- country_capital[!duplicated(country_capital$Name), ]

missing_ind <- is.na(country_capital$name)
country_capital[missing_ind, ]

missing_data <- data.frame(
  "Name" = country_capital[missing_ind, ]$Name,
  "name" = c(
    "Sarajevo", "Brazzaville", "Kinshasa", "Yamoussoukro",
    "Prague", "Prague", "Mbabane", "Suva", "Berlin", "Bonn", "Rangoon",
    "Pyongyang", "Skopje", "Soul", "Port of Spain", "London",
    "Washington", "San'a", "San'a", "Belgrade"
  )
)

missing_cities <- world.cities %>%
  filter(name %in% missing_data$name)

(missing_cities <- as.data.frame(missing_cities)[-c(2, 3, 8, 9, 21, 22), -2])

country_capital[missing_ind, ]$name <- missing_data$name


country_capital[missing_ind, ] <- left_join(country_capital[missing_ind, 1:3], missing_cities,
  by = "name")
any(is.na(country_capital))

# Compute Distance -------------------------------------------------------------
country_capital <- country_capital %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  arrange(Index)

GeoDistance <- st_distance(country_capital)
any(is.na(GeoDistance))

GeoDistance <- set_units(GeoDistance, "km")
colnames(GeoDistance) <- countries
rownames(GeoDistance) <- countries

saveRDS(GeoDistance, file = "data/GeoDistance.rds")

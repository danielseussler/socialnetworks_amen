# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model

# Trade Flows
# Data: https://correlatesofwar.org/data-sets/bilateral-trade

# GDP 
# Data: World Development Indicators

library(countrycode)
library(igraph)
library(tidyverse)
library(scales)
library(WDI)
library(xergm.common)


# Determine COW Codes ----------------------------------------------------------
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

countrycown <- countrycode(countrycowc,
  origin = "cowc", destination = "cown",
  custom_match = c("GFR" = 260)
)

country <- data.frame("index" = 1:164,
                      "iso2c" = countryiso2c, 
                      "cowc" = countrycowc)

former <- c("YAR", "YPR", "GFR", "GDR", "CZE")
formerIndex <- match(former, countrycowc)
current <- !(countrycowc %in% former)


# Import Data Set --------------------------------------------------------------
Dyadic_COW_4.0 <- read.csv("data/raw/COW_Trade_4.0/Dyadic_COW_4.0.csv", row.names = NULL)

tradedyad <- Dyadic_COW_4.0 %>%
  select(ccode1, ccode2, year, flow1, flow2) %>%
  filter(year == 2000) %>%
  filter(ccode1 %in% c(countrycown[current]), ccode2 %in% countrycown[current]) %>%
  mutate(trade = flow1 + flow2) %>%
  select(ccode1, ccode2, trade)

tradedyad <- data.matrix(tradedyad)
network <- graph.data.frame(tradedyad)
network <- get.adjacency(network, sparse = FALSE, attr = "trade")
network <- network + t(network)


# Subset relevant data ---------------------------------------------------------
all(colnames(network) %in% c(countrycown))
all(c(countrycown) %in% colnames(network))

tradematrix <- matrix(data = 0, nrow = sum(current), ncol = sum(current), dimnames = list(countrycown[current], countrycown[current]))
index <- match(countrycown[current], colnames(network))
tradematrix <- network[index, index]

any(tradematrix < 0) 
any(is.na(tradematrix))

# Impute missing and negative values 
tradematrix[is.na(tradematrix)] <- 0
tradematrix[tradematrix < 0] <- 0

colnames(tradematrix) <- rownames(tradematrix) <- countrycowc[current]



# World Development Indicators - World Bank ------------------------------------
# WDIsearch(string = "gdp")
WDIsearch('gdp.*constant')

# use NY.GDP.MKTP.CD for GDP current US$
# use NY.GDP.PCAP.CD for GDP per capita current US$
data = WDI(indicator = 'NY.GDP.MKTP.CD', country = "all", start = 1981, end = 2000)

data <- data %>%
  filter(year == 2000) %>%
  select(iso2c, NY.GDP.MKTP.CD)

GDP <- merge(country[current, ], data, by = "iso2c", all.x = TRUE)

# Impute NA Values with 1
any(is.na(GDP))
GDP[is.na(GDP)] <- 0

GDP <- GDP[order(GDP$index), c("cowc", "NY.GDP.MKTP.CD")]
head(GDP)
rownames(GDP) <- NULL
GDP <- column_to_rownames(GDP, var = "cowc")



# Calculate economic dependence
# COW Trade Data is in US millions of current dollars
EconomicDep <- matrix(0, nrow = sum(current), ncol = sum(current))
  
for (i in 1:sum(current)) {
  for (j in 1:sum(current)) {
    if (GDP[i, ] == 0 || GDP[j, ] == 0) {
      EconomicDep[i, j] <- 0
    } else {
      EconomicDep[i, j] <- min(tradematrix[i, j] * 1000000 / GDP[i, ], tradematrix[i, j] * 1000000 / GDP[j, ])
    }
  }
}

any(is.na(EconomicDep))
any(is.infinite(EconomicDep))
EconomicDep[is.na(EconomicDep)] <- 0

colnames(EconomicDep) <- rownames(EconomicDep) <- countrycowc[current]

min(EconomicDep)
max(EconomicDep)
EconomicDep <- EconomicDep*100

# Economic Trade Dependency for US Partners
plot(EconomicDep[1, ])

saveRDS(EconomicDep, file = "data/EconomicDep.rds")

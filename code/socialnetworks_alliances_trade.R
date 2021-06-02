# Seminar of Statistical Modeling of Social Networks
# Helper Function to determine sum of trade flows
# Data: https://correlatesofwar.org/data-sets/bilateral-trade

library(tidyverse)
library(xergm.common)
library(countrycode)
library(igraph)
data("alliances")


# Determine COW Codes ----------------------------------------------------------
country <- data.frame("Index" = 1:164, "cowc" = colnames(contigMat))

country$name <- countrycode(country$cowc,
  origin = "cowc", destination = "country.name",
  custom_match = c("GFR" = "German Federal Republic")
)

country$ccode <- countrycode(country$cowc,
  origin = "cowc", destination = "cown",
  custom_match = c("GFR" = 260)
)

# Import Data Set --------------------------------------------------------------
Dyadic_COW_4.0 <- read.csv("data/raw/COW_Trade_4.0/Dyadic_COW_4.0.csv", row.names = NULL)

tradedyad <- Dyadic_COW_4.0 %>%
  select(ccode1, ccode2, year, flow1, flow2) %>%
  filter(year == 2000) %>%
  filter(ccode1 %in% c(country$ccode), ccode2 %in% c(country$ccode)) %>%
  mutate(trade = flow1 + flow2) %>%
  select(ccode1, ccode2, trade) 


tradedyad <- data.matrix(tradedyad)
network <- graph.data.frame(tradedyad)
network <- get.adjacency(network, sparse = FALSE, attr='trade')
network <- network + t(network)


# Subset relevant data ---------------------------------------------------------
all(colnames(network) %in% c(country$ccode))
all(c(country$ccode) %in% colnames(network))

TradeFlowsY2000 <- matrix(data = 0, nrow = 164, ncol = 164, dimnames = list(country$ccode, country$ccode))
TradeFlowsY2000[rownames(network), colnames(network)] <- network

colnames(TradeFlowsY2000) <- country$cowc
rownames(TradeFlowsY2000) <- country$cowc

saveRDS(TradeFlowsY2000, file = "data/TradeFlowsY2000.rds")

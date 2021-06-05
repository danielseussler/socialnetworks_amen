# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model

# Trade Flows
# Data: https://correlatesofwar.org/data-sets/bilateral-trade

library(countrycode)
library(igraph)
library(tidyverse)
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

countrycown <- countrycode(countrycowc,
  origin = "cowc", destination = "cown",
  custom_match = c("GFR" = 260)
)

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

any(tradematrix < 1) 
any(is.na(tradematrix))

tradematrix <- ifelse(tradematrix > 1, log(tradematrix), 0)
any(is.na(tradematrix))

colnames(tradematrix) <- rownames(tradematrix) <- countrycowc[current]

saveRDS(tradematrix, file = "data/TradeFlows.rds")

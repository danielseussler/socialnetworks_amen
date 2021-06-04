# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# Cultural Similarity: Joint Language Indicator
# Data from the replication files of Warren 2010

# Data from 2000 doesnt seem to be coherent. Identity Matrix?
# I take data from 1985.

library(countrycode)
library(network)
library(xergm.common)

# Load Data --------------------------------------------------------------------
jointlanguage <- read.delim("data/raw/networks/joint language 1985.txt", header = FALSE, row.names = NULL)
jointlanguage <- as.matrix(jointlanguage)
jointlanguageCountries <- read.delim("data/raw/Country List (1950 - 2000).txt")

rownames(jointlanguage) <- colnames(jointlanguage) <- jointlanguageCountries$cowcode


# Selection
data("alliances")

countrycowc <- get.vertex.attribute(allyNet[["2000"]], "vertex.names")
countrycown <- countrycode(countrycowc,
  origin = "cowc", destination = "cown",
  custom_match = c("GFR" = 260)
)

former <- c("YAR", "YPR", "GFR", "GDR", "CZE")
formerIndex <- match(former, countrycowc)
current <- !(countrycowc %in% former)



# Country Selection ------------------------------------------------------------
selection <- match(jointlanguageCountries$cowcode, countrycown[current], nomatch = 0)

CulturalSim <- matrix(data = 0, nrow = sum(current), ncol = sum(current), 
                      dimnames = list(countrycowc[current], countrycowc[current]))
CulturalSim <- jointlanguage[selection, selection]

det(data.matrix(CulturalSim))

colnames(CulturalSim) <- rownames(CulturalSim) <- countrycowc[current]

saveRDS(CulturalSim, file = "data/culturalsim.rds")

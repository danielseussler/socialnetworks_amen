# Seminar of Statistical Modeling of Social Networks
# Helper Function to determine GeoDistance between two countries, by capital.
# Data from the replication files of Warren 2010

# Issue: Data from 2000 seems to be an identiy matrix, det = 1. 
# I take data from 1985.

library(xergm.common)
library(countrycode)

# Load Data --------------------------------------------------------------------
data("alliances")

jointlanguage <- read.delim("data/raw/networks/joint language 1985.txt", header = FALSE, row.names = NULL)
country <- read.delim("data/raw/Country List (1950 - 2000).txt")

rownames(jointlanguage) <- country$cowcode
colnames(jointlanguage) <- country$cowcode

alliances <- data.frame("Index" = 1:164, "cowc" = colnames(contigMat))
alliances$ccode <- countrycode(alliances$cowc,
  origin = "cowc", destination = "cown",
  custom_match = c("GFR" = 260)
)

# Country Selection ------------------------------------------------------------
ind <- c(country$cowcode) %in% c(alliances$ccode)
jointlanguage <- jointlanguage[ind, ind]
CulturalSim <- matrix(data = 0, nrow = 164, ncol = 164, dimnames = list(c(alliances$ccode), c(alliances$ccode)))
CulturalSim <- jointlanguage

all(colnames(jointlanguage) == colnames(CulturalSim))
det(data.matrix(CulturalSim))

colnames(CulturalSim) <- colnames(contigMat)
rownames(CulturalSim) <- rownames(contigMat)
saveRDS(CulturalSim, file = "data/CulturalSim.rds")

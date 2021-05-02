# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Model
# Daniel A. Seussler Becerra

library(tidyverse)
library(amen)
library(statnet)
library(xergm.common)
library(ggnetwork)

# Data Description -------------------------------------------------------------
# allyNet Network objects, undirected, three vertex attributes
#         cinc composite natonal capability, polity score, year
# contigMat indicates shared border
# lnet
# LSP Matrix recording the number of shared partners
# warNet whether two states were in a militarized interstate dispute in this year.


# Data Exploration -------------------------------------------------------------
data("alliances")

# We first analyse the year 2000 as a static network
allyNet2000 <- allyNet[["2000"]]
lNet2000 <- lNet[["2000"]]
LSP2000 <- LSP[["2000"]]
warNet2000 <- warNet[["2000"]]
countries <- colnames(contigMat)
rm("allyNet", "lNet", "LSP", "warNet")
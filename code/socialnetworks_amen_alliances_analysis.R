# Seminar on Statistical Modelling of Social Networks
# Topic: Additive and Multiplicative Effects Model
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
rm("allyNet", "lNet", "LSP", "warNet")


# Network Statistics -----------------------------------------------------------
is.network(allyNet2000)

summary(allyNet2000)
InDegree <- degree(allyNet2000, cmode = "indegree")
network.size(allyNet2000)
network.edgecount(allyNet2000)
network.dyadcount(allyNet2000)

gden(allyNet2000, mode = "digraph")
components(allyNet2000, connected = "strong")

# Visualization ----------------------------------------------------------------

plot(allyNet2000)


ggnetwork(allyNet2000) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey") +
  geom_nodes(color = "red", size = InDegree / 10) +
  theme_blank() +
  ggtitle("The Alliances Year 2000")
# geom_nodelabel_repel


hist(InDegree, xlab = "Indegree", main = "In-Degree Distribution", prob = FALSE, breaks = 50)



# AMEN Analysis ----------------------------------------------------------------

# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# Data Statistics Figures

library(amen)
library(countrycode)
library(network)
library(RColorBrewer)
library(statnet)
library(tictoc)
library(xergm.common)

set.seed(42)

# Data Year 2000 ---------------------------------------------------------------
data("alliances")

allyNet <- allyNet[["2000"]]
lNet <- lNet[["2000"]]
LSP <- LSP[["2000"]]
warNet <- warNet[["2000"]]

allyNetMat <- as.matrix.network(allyNet)

countrycowc <- get.vertex.attribute(allyNet, "vertex.names")

countryname <- countrycode(countrycowc,
  origin = "cowc", destination = "country.name",
  custom_match = c(
    "GFR" = "German Federal Republic",
    "CON" = "Congo",
    "DRC" = "Democratic Republic of the Congo"
  )
)

countryregion <- countrycode(countrycowc,
  origin = "cowc", destination = "continent",
  custom_match = c(
    "CZE" = "Europe", "GDR" = "Europe", "GFR" = "Europe",
    "YAR" = "Asia", "YPR" = "Asia", "YUG" = "Europe"
  )
)

set.vertex.attribute(allyNet, "region", countryregion)

head(cbind(countrycowc, countryname, countryregion))


# Drop former countries 
former <- c("YAR", "YPR", "GFR", "GDR", "CZE")
any(allyNetMat[former, former] == 1)

formerIndex <- match(former, countrycowc)
current <- !(countrycowc %in% former)

allyNet <- delete.vertices(allyNet, formerIndex)
allyNetMat <- allyNetMat[current, current]
lNet <- lNet[current, current]
LSP <- LSP[current, current]
warNet <- warNet[current, current]
countrycowc <- countrycowc[current]
countryname <- countryname[current]
countryregion <- countryregion[current]

# Network Statistics -----------------------------------------------------------
is.network(allyNet)

summary(allyNet, print.adj = FALSE)
degreeNet <- degree(allyNet, cmode = "indegree")   #Indegree = Outdegree

network.size(allyNet)
network.edgecount(allyNet)
network.dyadcount(allyNet) # which is n*(n-1)/2

countrycowc[degreeNet > 0]
table(degreeNet)

countryname[countryregion == "Asia"]
countryname[countryregion == "Oceania"]

gden(allyNet, mode = "digraph")
components(allyNet, connected = "strong")

hist(degreeNet, xlab = "Indegree", main = "In-Degree Distribution", prob = FALSE, breaks = 50)
hist(degreeNet, xlab = "", ylab = "", main = "", prob = FALSE, breaks = 50) #saved as pdf
countryname[degreeNet == 23]


## Set Colors ----------------------------------------------------------
col <- character(length = length(countryregion))
col[countryregion == "Europe"] <- brewer.pal(5, "Blues")[1]
col[countryregion == "Asia"] <- brewer.pal(5, "Blues")[2]
col[countryregion == "Oceania"] <- brewer.pal(5, "Blues")[3]
col[countryregion == "Africa"] <- brewer.pal(5, "Blues")[4]
col[countryregion == "Americas"] <- brewer.pal(5, "Blues")[5]


# Figure Interstate Alliance Network -------------------------------------------
colnames(allyNetMat) <- rownames(allyNetMat) <- countryname

netplot(allyNetMat, seed = 42, directed = FALSE, plotnames = FALSE, plot.iso = FALSE)
netplot(allyNetMat, seed = 42, directed = FALSE, plotnames = TRUE)
netplot(allyNetMat, seed = 1, directed = FALSE, plotnames = TRUE, plot.iso = FALSE) #Figure PDF

netplot(warNet, seed = 42)
netplot(contigMat, seed = 42)

circplot(allyNetMat, jitter = 10) # later for latent variable analysis


# Figure The Alliances Data Set - Regional Color Coding ------------------------
netplot(allyNetMat, seed = 42, directed = FALSE, plotnames = FALSE, plot.iso = FALSE, ncol = col) # saved as Rplot2.pdf



# Figure Circplot --------------------------------------------------------------
ame_geom_evol_R2 <- readRDS(file = "analysis/models/ame_geom_evol_R2.rds")
Xevol <- readRDS(file = "analysis/models/Xevol.rds")
plot(ame_geom_evol_R2)

mu <- mean(ame_geom_evol_R2$BETA[, 1])
bd <- apply(ame_geom_evol_R2$BETA[, 1 + 1:5], 2, mean)

EY2 <- mu + Xbeta(Xevol, bd)


circplot(Y, ame_geom_evol_R2$U, ame_geom_evol_R2$V, pscale = 1.1, row.names = countries, col.names = countries)

circplot(Y[InDegree > 0, InDegree > 0],
         ame_geom_evol_R2$U[InDegree > 0, ], ame_geom_evol_R2$V[InDegree > 0, ],
         pscale = 1.3, plotnames = FALSE
)

circplot(Y[InDegree >= 9, InDegree >= 9],
         ame_geom_evol_R2$U[InDegree >= 9, ], ame_geom_evol_R2$V[InDegree >= 9, ],
         pscale = 1.3, plotnames = FALSE
)

circplot(1*(Y[InDegree > 0, InDegree > 0]),
         ame_geom_evol_R2$U[InDegree > 0, ], ame_geom_evol_R2$V[InDegree > 0, ],
         pscale = 1.3, plotnames = FALSE
)

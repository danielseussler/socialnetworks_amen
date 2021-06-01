# Seminar on Statistical Modeling of Social Networks
# Daniel A. Seussler Becerra
# Code for the Figures

library(amen)
library(countrycode)
library(RColorBrewer)
library(xergm.common)
library(statnet)

set.seed(42)

# Network Data of Year 2000 ----------------------------------------------------
data("alliances")

allyNet1981 <- allyNet[["1981"]]
allyNet2000 <- allyNet[["2000"]]
lNet2000 <- lNet[["2000"]]
LSP2000 <- LSP[["2000"]]
warNet2000 <- warNet[["2000"]]

rm("allyNet", "lNet", "LSP", "warNet")

countries <- colnames(contigMat)

names <- countrycode(countries,
                     origin = "cowc", destination = "country.name",
                     custom_match = c("GFR" = "German Federal Republic")
)

regions <- countrycode(countries,
                       origin = "cowc", destination = "continent",
                       custom_match = c(
                         "CZE" = "Europe", "GDR" = "Europe", "GFR" = "Europe",
                         "YAR" = "Asia", "YPR" = "Asia", "YUG" = "Europe"
                       )
)

set.vertex.attribute(allyNet2000, "region", regions)



# Introduction: Network Statistics ---------------------------------------------
is.network(allyNet2000)

summary(allyNet2000, print.adj = FALSE)
InDegree <- degree(allyNet2000, cmode = "indegree")
OutDegree <- degree(allyNet2000, cmode = "outdegree") # same as InDegree as sym
network.size(allyNet2000)
network.edgecount(allyNet2000)
network.dyadcount(allyNet2000) # which is n*(n-1)/2
countries[InDegree > 0]
table(InDegree)

names[regions == "Asia"] 
names[regions == "Oceania"] 

gden(allyNet2000, mode = "digraph")
components(allyNet2000, connected = "strong")

network_statistics <- c(network.size(allyNet2000), network.edgecount(allyNet2000), 
                        network.dyadcount(allyNet2000), gden(allyNet2000, mode = "digraph"))
names(network_statistics) <- c("Size", "Edgecount", "Dyadcount", "Density")

hist(InDegree, xlab = "Indegree", main = "In-Degree Distribution", prob = FALSE, breaks = 50)
hist(InDegree, xlab = "", ylab = "", main = "", prob = FALSE, breaks = 50) #save as degree_histogram.pdf
countries[InDegree == 23]

saveRDS(network_statistics, file = "analysis/data/socialnetworks_statistics.rds")



## Set Colors for all Plots ----------------------------------------------------
col <- character(length = 164)
col[regions == "Europe"] <- brewer.pal(5, "Blues")[1]
col[regions == "Asia"] <- brewer.pal(5, "Blues")[2]
col[regions == "Oceania"] <- brewer.pal(5, "Blues")[3]
col[regions == "Africa"] <- brewer.pal(5, "Blues")[4]
col[regions == "Americas"] <- brewer.pal(5, "Blues")[5]



# Figure 1: The Alliances Data Set ---------------------------------------------
Y <- as.matrix.network(allyNet2000)

netplot(Y, xlab = "The Alliances Network Year 2000", seed = 42, directed = FALSE, plotnames = FALSE, plot.iso = FALSE)
netplot(Y, xlab = "The Alliances Network Year 2000", seed = 42, directed = FALSE, plotnames = TRUE)
netplot(Y, seed = 42, directed = FALSE, plotnames = TRUE, plot.iso = FALSE) 
netplot(warNet2000, seed = 42)
netplot(contigMat, seed = 42)

circplot(Y, jitter = 10)    # later for latent variable analysis

Y2 <- Y
colnames(Y2) <- names; rownames(Y2) <- names
netplot(Y2, seed = 42, directed = FALSE, plotnames = TRUE, plot.iso = FALSE) #saved as Rplot1.pdf



# Figure 2: The Alliances Data Set - Regional Color Coding ---------------------
netplot(Y2, seed = 42, directed = FALSE, plotnames = FALSE, plot.iso = FALSE, ncol = regions ) #saved as Rplot2.pdf



# Figure 3: Circplot -----------------------------------------------------------
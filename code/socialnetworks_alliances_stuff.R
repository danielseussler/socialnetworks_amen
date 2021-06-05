# Code for Plots not used

library(tidyverse)
library(amen)
library(statnet)
library(xergm.common)
library(ggnetwork)
library(arcdiagram) # only available on github
library(reshape)
library(countrycode)
library(RColorBrewer)
library(GGally)

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



# Network Statistics -----------------------------------------------------------
is.network(allyNet2000)

summary(allyNet2000, print.adj = FALSE)
InDegree <- degree(allyNet2000, cmode = "indegree")
OutDegree <- degree(allyNet2000, cmode = "outdegree") # same as InDegree as sym
network.size(allyNet2000)
network.edgecount(allyNet2000)
network.dyadcount(allyNet2000) # which is n*(n-1)/2
countries[InDegree > 0]
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



# Visualization with Plot Network ----------------------------------------------
label_topInDegree <- ifelse(InDegree > 10, network.vertex.names(allyNet2000), "")

plot(allyNet2000,
     displaylabels = TRUE, label = label_topInDegree, vertex.col = col, pad = 2,
     label.cex = 1, boxed.labels = FALSE,
     xlab = "The Alliances Network Year 2000"
)

plot(allyNet2000,
     displaylabels = TRUE, vertex.col = col, pad = 2, edge.len = 10,
     label.cex = 1, boxed.labels = FALSE,
     displayisolates = FALSE,
     xlab = "The Alliances Network Year 2000"
)




# Visualization with netplot (amen) --------------------------------------------
Y <- as.matrix.network(allyNet2000)

netplot(Y, xlab = "The Alliances Network Year 2000", seed = 42, directed = FALSE, plotnames = FALSE, plot.iso = FALSE)
netplot(Y, xlab = "The Alliances Network Year 2000", seed = 42, directed = FALSE, plotnames = TRUE)
netplot(Y, seed = 42, directed = FALSE, plotnames = TRUE, plot.iso = FALSE)
netplot(warNet2000, seed = 42)
netplot(contigMat, seed = 42)

circplot(Y, jitter = 10) # later for latent variable analysis

Y2 <- Y
colnames(Y2) <- names; rownames(Y2) <- names
netplot(Y2, seed = 42, directed = FALSE, plotnames = TRUE, plot.iso = FALSE)

# Visualization with Plot Network ----------------------------------------------
ggnetwork(allyNet2000) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey") +
  geom_nodes(color = col, size = log(InDegree + 1)) +
  theme_blank() +
  ggtitle("The Alliances Network Year 2000") +
  geom_nodelabel_repel(label = label_topInDegree)



# Arcdiagram with library arcdiagram  ------------------------------------------
E <- as.edgelist(allyNet2000, directed = FALSE, n = 164)
V <- c(1:164)
vlabels <- get.vertex.attribute(allyNet2000, "vertex.names")

arcplot(E, V, labels = vlabels)
arcplot(E, V, labels = vlabels, horizontal = FALSE)

x <- data.frame(regions = regions, degrees = InDegree, ind = c(1:164), stringsAsFactors = TRUE)
x$regions <- factor(regions, level = c("Americas", "Europe", "Oceania", "Asia", "Africa"))
x <- arrange(x, sort(regions), desc(InDegree))

arcplot(E, V,
        ordering = x$ind, labels = vlabels, cex.labels = 0.8,
        cex.nodes = log(InDegree + 1) + 0.5, horizontal = FALSE,
        show.nodes = TRUE, pch.nodes = 21, col.nodes = colors,
        bg.nodes = col, col.arcs = hsv(0, 0, 0.2, 0.25),
        line = -0.5
)
col

# ToDo
# Order is still wrong. Not really leglible



# Using ggnet2 from the GGally Package -----------------------------------------
ggnet2(allyNet1981, size = 2)

set.vertex.attribute(allyNet2000, "color", col)
ggnet2(allyNet2000, size = 2, color = "color")

ggnet2(allyNet2000, mode = "circle", size = 2)

ggnet2(allyNet2000, size = 2, label = TRUE, layout.par = list(cell.jitter = 0.75, niter = 1000))

# not possible to set seed?
# does not work as easy with colors as advertised




################################################################################

# Longitudinal Analysis --------------------------------------------------------
year <- c("1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1990", 
          "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000")
Y_time <- array(data = 0, dim = c(164, 164, 19), dimnames = list(countries, countries, year))
XNode_time <- array(data = 0, dim = c(164, 2, 19), dimnames = list(countries, c("cinc", "polity"), year))
XDyad_time <- array(data = 0, dim = c(164, 164, 4, 19), dimnames = list(countries, countries, c("contigMat", "lNet", "LSP", "warNet"), year))

for (yr in seq_along(year)) {
  # Sociomatrix
  year_char <- year[yr]
  Y_time[, , yr] <- as.matrix.network(allyNet[[year_char]])
  
  # Nodal Covariates
  XNode_time[, , yr] <- cbind(
    get.vertex.attribute(allyNet[[year_char]], "cinc"),
    get.vertex.attribute(allyNet[[year_char]], "polity")
  )
  
  # Dyadic Covariates
  XDyad_time[, , , yr] <- array(data = c(contigMat, lNet[[year_char]], LSP[[year_char]], warNet[[year_char]]),
                                dim = c(164, 164, 4))
}



# AME model replicated relational data -----------------------------------------
tic("Largest Model so far:")
fit_AME_Rep_R3 <- ame_rep(Y_time, Xdyad = XDyad_time, Xrow = XNode_time, Xcol = XNode_time,
                          R= 2, family = "bin", symmetric = TRUE)
toc()
saveRDS(fit_AME_Rep_R3, file = "analysis/models/fit_AME_Rep_R3.rds")
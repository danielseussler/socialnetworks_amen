# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# Daniel A. Seussler Becerra
library(tidyverse)
library(amen)
library(statnet)
library(xergm.common)
library(ggnetwork)
library(arcdiagram)   #only available on github
library(reshape)
library(countrycode)
library(wesanderson)
set.seed(42)

# Network Data of Year 2000 ----------------------------------------------------
data("alliances")

allyNet2000 <- allyNet[["2000"]]
lNet2000 <- lNet[["2000"]]
LSP2000 <- LSP[["2000"]]
warNet2000 <- warNet[["2000"]]
rm("allyNet", "lNet", "LSP", "warNet")

countries <- colnames(contigMat)
names <- countrycode(countries, origin = "cowc", destination = "country.name", 
                     custom_match = c("GFR" = "German Federal Republic"))
regions <- countrycode(countries, origin = "cowc", destination = "continent", 
                       custom_match = c("CZE" = "Europe", "GDR" = "Europe", "GFR" = "Europe",
                                        "YAR" = "Asia", "YPR" = "Asia", "YUG" = "Europe"))



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


# Visualization Options------- -------------------------------------------------
label_topInDegree <- ifelse(InDegree > 10, network.vertex.names(allyNet2000), "")
plot(allyNet2000, displaylabels = TRUE, label = label_topInDegree,
     label.cex = 0.7, boxed.labels = FALSE, xlab = "The Alliances Network Year 2000")

ggnetwork(allyNet2000) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey") +
  geom_nodes(color = "red", size = log(InDegree + 1)) +
  theme_blank() +
  ggtitle("The Alliances Network Year 2000")+
  geom_nodelabel_repel(label = label_topInDegree)

hist(InDegree, xlab = "Indegree", main = "In-Degree Distribution", prob = FALSE, breaks = 50)
countries[InDegree == 23]


# Visualization with netplot (amen) --------------------------------------------
Y <- as.matrix.network(allyNet2000)

netplot(Y, xlab ="The Alliances Network Year 2000", seed = 42, directed = FALSE, plotnames = FALSE, plot.iso = FALSE)
netplot(Y, xlab="The Alliances Network Year 2000", seed = 42, directed = FALSE, plotnames = TRUE)
netplot(Y, xlab="The Alliances Network Year 2000", seed = 42, directed = FALSE, plotnames = TRUE , plot.iso = FALSE)
netplot(warNet2000, seed = 42)
netplot(contigMat, seed = 42)


circplot(Y, jitter = 10) # later for latent variable analysis


# Arcdiagram with library arcdiagram  ------------------------------------------
E <- as.edgelist(allyNet2000, directed = FALSE, n = 164)
V <- c(1:164)
vlabels <- get.vertex.attribute(allyNet2000, "vertex.names")

arcplot(E, V, labels = vlabels)
arcplot(E, V, labels = vlabels, horizontal = FALSE)

x <- data.frame(regions = regions, degrees = InDegree, ind = c(1:164), stringsAsFactors = TRUE)
x$regions <- factor(regions, level = c("Americas", "Europe", "Oceania", "Asia", "Africa"))
x <- arrange(x, sort(regions), desc(InDegree))

colors <- character(length = 164)
colors[regions == "Europe"] <-wes_palette("Rushmore1", 5)[1]
colors[regions == "Asia"] <-wes_palette("Rushmore1", 5)[2]
colors[regions == "Oceania"] <-wes_palette("Rushmore1", 5)[3]
colors[regions == "Africa"] <-wes_palette("Rushmore1", 5)[4]
colors[regions == "Americas"] <- wes_palette("Rushmore1", 5)[5]

arcplot(E, V, ordering = x$ind, labels = vlabels, cex.labels = 0.8,
        cex.nodes = log(InDegree + 1) + 0.5, horizontal = FALSE, 
        show.nodes = TRUE, pch.nodes=21, col.nodes = colors, bg.nodes=colors,
        col.arcs = hsv(0, 0, 0.2, 0.25),
        line = -0.5)
  
# ToDo
  # Order is still wrong. Not really eliglble


# Using ggnet2 from the GGally Package -----------------------------------------

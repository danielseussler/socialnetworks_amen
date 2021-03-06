# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model

# Analysis Data Statistics Figures

library(amen)
library(bayesplot)
library(countrycode)
library(ggplot2)
library(network)
library(RColorBrewer)
library(statnet)
library(xergm.common)

set.seed(42)

# Data Year 2000 ---------------------------------------------------------------
data("alliances")

# range(gden(allyNet, mode = "digraph"))  #compute range of network density

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

countrycown <- countrycode(countrycowc,
  origin = "cowc", destination = "cown",
  custom_match = c("GFR" = 260)
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
former <- c("YAR", "YPR", "GFR", "GDR", "CZE", "YUG")
any(allyNetMat[former, former] == 1)

formerIndex <- match(former, countrycowc)
current <- !(countrycowc %in% former)

allyNet <- delete.vertices(allyNet, formerIndex)
allyNetMat <- allyNetMat[current, current]
lNet <- lNet[current, current]
LSP <- LSP[current, current]
warNet <- warNet[current, current]
contigMat <- contigMat[current, current]
countrycown <- countrycown[current]
countrycowc <- countrycowc[current]
countryname <- countryname[current]
countryregion <- countryregion[current]



# Network Statistics -----------------------------------------------------------
is.network(allyNet)

summary(allyNet, print.adj = FALSE)
degreeNet <- degree(allyNet, cmode = "indegree") # Indegree = Outdegree

network.size(allyNet)
network.edgecount(allyNet)
network.dyadcount(allyNet) # which is n*(n-1)/2

countrycowc[degreeNet > 0]
table(degreeNet)

median(degreeNet)

countryname[countryregion == "Asia"]
countryname[countryregion == "Oceania"]

gden(allyNet, mode = "digraph")
components(allyNet, connected = "strong")

hist(degreeNet, xlab = "Indegree", main = "In-Degree Distribution", prob = FALSE, breaks = 50)
hist(degreeNet, xlab = "", ylab = "", main = "", prob = FALSE, breaks = 50) # saved as pdf
countryname[degreeNet == 23]



# GOF Statistics ---------------------------------------------------------------
gofstats(allyNetMat)

diag(allyNetMat) <- NA
sd.colmean <- sd.rowmean <- sd(rowMeans(allyNetMat, na.rm = TRUE), na.rm = TRUE)
sd.colmean

dyad.dep <- suppressWarnings(cor(c(allyNetMat), c(t(allyNetMat)), use = "complete.obs"))

E <- allyNetMat - mean(allyNetMat, na.rm = TRUE)
D <- 1 * (!is.na(E))
E[is.na(E)] <- 0

cycle.dep <- sum(diag(E %*% E %*% E)) / (sum(diag(D %*% D %*% D)) * sd(c(allyNetMat), na.rm = TRUE)^3)
trans.dep <- sum(diag(E %*% t(E) %*% E)) / (sum(diag(D %*% t(D) %*% D)) * sd(c(allyNetMat), na.rm = TRUE)^3)

# F-Test Anova for row (or column) heterogeneityd
Rowcountry <- matrix(rownames(allyNetMat), nrow(allyNetMat), ncol(allyNetMat))
Colcountry <- t(Rowcountry)
anova(lm(c(allyNetMat) ~ c(Rowcountry) + c(Colcountry)))



## Set Colors ----------------------------------------------------------
col <- character(length = length(countryregion))
# display.brewer.pal(n = 5, name = 'Blues')
# col[countryregion == "Europe"] <- brewer.pal(5, "Blues")[1]
# col[countryregion == "Asia"] <- brewer.pal(5, "Blues")[2]
# col[countryregion == "Oceania"] <- brewer.pal(5, "Blues")[3]
# col[countryregion == "Africa"] <- brewer.pal(5, "Blues")[4]
# col[countryregion == "Americas"] <- brewer.pal(5, "Blues")[5]

col[countryregion == "Europe"] <- "#4DB3B3"
col[countryregion == "Asia"] <- "#3D4C53"
col[countryregion == "Oceania"] <- "#E6772E"
col[countryregion == "Africa"] <- "#E64A45"
col[countryregion == "Americas"] <- "#F2C249"



# Figure Interstate Alliance Network -------------------------------------------
colnames(allyNetMat) <- rownames(allyNetMat) <- countryname

netplot(allyNetMat, seed = 42, directed = FALSE, plotnames = FALSE, plot.iso = FALSE)
netplot(allyNetMat, seed = 42, directed = FALSE, plotnames = TRUE)
netplot(allyNetMat, seed = 1, directed = FALSE, plotnames = TRUE, plot.iso = FALSE) # Figure PDF


countrydeselect <- c(
  "UZB", "LBR", "BEN", "POL", "CON", "BUI", "IRQ", "LIB",
  "GAM", "ALG", "JAM", "GAB", "JPN", "AUL", "CAO", "GUA", "CDI",
  "UAE", "OMA", "DOM", "JAM", "NIR", "GUI", "SIE", "DJI", "COM",
  "HUN", "POR", "DRC", "SAL", "VEN", "PAR", "ARG", "NTH", "SOM",
  "TRI", "GUY", "BOL", "URU", "GNB", "GMY", "NIC", "PAN"
)
countrydeselect <- (countrycowc %in% countrydeselect)
countrynameselect <- countryname
countrynameselect[countrydeselect] <- " "
colnames(allyNetMat) <- rownames(allyNetMat) <- countrynameselect

netplot(allyNetMat, seed = 1, directed = FALSE, plotnames = TRUE, plot.iso = FALSE) # Figure PDF less names

netplot(warNet, seed = 42)
netplot(contigMat, seed = 42)

# Figure The Alliances Data Set - Regional Color Coding ------------------------
netplot(allyNetMat, seed = 42, directed = FALSE, plotnames = FALSE, plot.iso = FALSE, ncol = col)



# Covariate Correlations -------------------------------------------------------
cinc <- get.vertex.attribute(allyNet, "cinc")[current] * 100
polity <- get.vertex.attribute(allyNet, "polity")[current]

cor(cinc, degreeNet, use = "complete.obs")
cor(cinc, logGDP, use = "complete.obs")



# extended summary function of amen package to output a table ------------------
table_ame <- function(object, ...) {
  fit <- object
  tmp <- cbind(
    apply(fit$BETA, 2, mean), apply(fit$BETA, 2, sd),
    apply(fit$BETA, 2, mean) / apply(fit$BETA, 2, sd),
    2 * (1 - pnorm(abs(apply(fit$BETA, 2, mean) / apply(fit$BETA, 2, sd)))),
    round(coda::effectiveSize(fit$BETA))
  )
  colnames(tmp) <- c("pmean", "psd", "z-stat", "p-val", "n-eff")
  out <- round(tmp, 4)


  tmp <- cbind(apply(fit$VC, 2, mean), apply(fit$VC, 2, sd))
  tmp <- cbind(round(tmp, 4), array("-", dim = c(nrow(tmp), 3)))

  out <- rbind(out, tmp)
  return(out)
}



# Load Models ------------------------------------------------------------------
fitZNII0H <- readRDS(file = "analysis/models/fitZNII0H.rds") # AME R=2
fitCPLTUK <- readRDS(file = "analysis/models/fitCPLTUK.rds") # Additive Effects
fitX7XDFO <- readRDS(file = "analysis/models/fitX7XDFO.rds") # Multiplicative Effects R = 2
fitDASD8R <- readRDS(file = "analysis/models/fitDASD8R.rds") # No Random Effects


fitIQD1Q2 <- readRDS(file = "analysis/models/fitIQD1Q2.rds") # AME R=5
fitWIBWVH <- readRDS(file = "analysis/models/fitWIBWVH.rds") # ME R=5
fitFVAEMT <- readRDS(file = "analysis/models/fitFVAEMT.rds") # AME R=2 Drop Intercept


fitSIXQ7Y <- readRDS(file = "analysis/models/fitSIXQ7Y.rds") # AME drop all fixed effects
fitEQNO0V <- readRDS(file = "analysis/models/fitEQNO0V.rds") # AME drop SharedA Conflict
fitGIPKVP <- readRDS(file = "analysis/models/fitGIPKVP.rds") # AME Drop all non significant except GDP DIST ECON SharedAllies POLITY


fitRU7J8K <- readRDS(file = "analysis/models/fitRU7J8K.rds") # AME quadruble iterations

# Plots + Summaries ------------------------------------------------------------
plot(fitZNII0H)
plot(fitCPLTUK)
plot(fitX7XDFO)
plot(fitDASD8R)

plot(fitIQD1Q2)
plot(fitWIBWVH)
plot(fitFVAEMT)

plot(fitSIXQ7Y)
plot(fitEQNO0V)
plot(fitGIPKVP)

plot(fitRU7J8K)


# R=2 vs R=5 -------------------------------------------------------------------

ht <- c(1000, 1000, 50, 100, 100)

for (k in c(1, 4)) {
  # AME R = 5
  xlim <- range(c(fitIQD1Q2$GOF[, k], fitZNII0H$GOF[, k], fitIQD1Q2$GOF[1, k])) * c(.9, 1.1)
  hist(fitIQD1Q2$GOF[-1, k],
    prob = TRUE, col = "yellowgreen", xlim = xlim, main = "", ylab = "",
    xlab = colnames(fitIQD1Q2$GOF)[k], ylim = c(0, ht[k])
  )
  # AME R = 2
  clr <- c(col2rgb("slategrey") / 255, .75)
  hist(fitZNII0H$GOF[-1, k],
    prob = TRUE, add = TRUE,
    col = rgb(clr[1], clr[2], clr[3], clr[4])
  )

  abline(v = fitZNII0H$GOF[1, k], col = "red")
}


median(fitZNII0H$GOF[-1, k]) # R=2
median(fitIQD1Q2$GOF[-1, k]) # R=5  #does not yield improvement


# Model Comparison using GOF Statistics ----------------------------------------
# Saved as figure
par(mfrow = c(3, 2))

# Ordinary Regression vs Additive Effects (SRRM)
ht <- c(300, 300, 50, 100, 100)

for (k in c(1, 4)) {
  # Ordinary Regression
  xlim <- range(c(fitDASD8R$GOF[, k], fitCPLTUK$GOF[, k], fitDASD8R$GOF[1, k])) * c(.9, 1.1)
  hist(fitDASD8R$GOF[-1, k],
    prob = TRUE, col = "pink", xlim = xlim, main = "", ylab = "",
    xlab = colnames(fitDASD8R$GOF)[k], ylim = c(0, ht[k])
  )
  # Additive Effects
  clr <- c(col2rgb("lightblue") / 255, .75)
  hist(fitCPLTUK$GOF[-1, k],
    prob = TRUE, add = TRUE,
    col = rgb(clr[1], clr[2], clr[3], clr[4])
  )

  abline(v = fitDASD8R$GOF[1, k], col = "red")
}



# Multiplicative Effects vs Additive Effects (SRRM)
ht <- c(500, 500, 50, 50, 50)

for (k in c(1, 4)) {
  # Multiplicative Effects
  xlim <- range(c(fitX7XDFO$GOF[, k], fitCPLTUK$GOF[, k], fitX7XDFO$GOF[1, k])) * c(.9, 1.1)
  hist(fitX7XDFO$GOF[-1, k],
    prob = TRUE, col = "yellowgreen", xlim = xlim, main = "", ylab = "",
    xlab = colnames(fitX7XDFO$GOF)[k], ylim = c(0, ht[k])
  )
  # Additive Effects
  clr <- c(col2rgb("lightblue") / 255, .75)
  hist(fitCPLTUK$GOF[-1, k],
    prob = TRUE, add = TRUE,
    col = rgb(clr[1], clr[2], clr[3], clr[4])
  )

  abline(v = fitX7XDFO$GOF[1, k], col = "red")
}


# Multiplicative Effects vs AME
ht <- c(1000, 1000, 50, 100, 100)

for (k in c(1, 4)) {
  # Multiplicative Effects
  xlim <- range(c(fitX7XDFO$GOF[, k], fitZNII0H$GOF[, k], fitX7XDFO$GOF[1, k])) * c(.9, 1.1)
  hist(fitX7XDFO$GOF[-1, k],
    prob = TRUE, col = "yellowgreen", xlim = xlim, main = "", ylab = "",
    xlab = colnames(fitX7XDFO$GOF)[k], ylim = c(0, ht[k])
  )
  # AME
  clr <- c(col2rgb("slategrey") / 255, .75)
  hist(fitZNII0H$GOF[-1, k],
    prob = TRUE, add = TRUE,
    col = rgb(clr[1], clr[2], clr[3], clr[4])
  )

  abline(v = fitZNII0H$GOF[1, k], col = "red")
}




# Load Theme ------------------------------------------------------------------
my_theme <- theme_minimal() + theme(text = element_text(size = 20, family = "serif"))
ggplot2::theme_set(my_theme)
color_scheme_set("blue")



# MCMC Analysis with bayesplot -------------------------------------------------
pars <- c(
  "GDP (log p.c.).node", "GeoDistance.dyad", "CulturalSim.dyad", "EconomicDep.dyad",
  "SharedAllies.dyad", "ConflictInd.dyad", "PoliticalSim.dyad", "CapabilityRat.dyad"
)

coda::effectiveSize(fitZNII0H$BETA)
summary(fitZNII0H)
plot(fitZNII0H)
mcmc_areas(fitZNII0H$BETA)
mcmc_hist(fitZNII0H$BETA, pars = pars)
mcmc_intervals(fitZNII0H$BETA, pars = pars)



# Figure Trace Plot ------------------------------------------------------------
fitZNII0Hparam <- cbind(fitZNII0H$VC[, "va"], fitZNII0H$BETA)
colnames(fitZNII0Hparam) <- c("VA", "Intercept", pars)
mcmc_trace(fitZNII0Hparam, facet_args = list(ncol = 2))



# AME Rank 2 Random Effects Analysis -------------------------------------------

# Multiplicative Effects Figure
head(fitZNII0H$APM)
plot(fitZNII0H$APM)


head(fitZNII0H$U)

countrycowc[countryname == "Belarus"]

LatentSelect <- c(
  "CUB", "USA", "COS", "DRC", "COL", "SLO", "SAU", "AZE", "SEN", "LIB",
  "GHA", "CHN", "MAL", "KGY", "GMY", "FRN", "NIR", "RUS", "NAM", "UZB",
  "NAM", "AUL", "SWZ", "BUI", "BLR"
)
LatentSelectIndex <- match(LatentSelect, countrycowc)

LatentSpace <- fitZNII0H$U %*% diag(sqrt(fitZNII0H$L))

par(mfrow = c(1, 1), mai = c(1, 1, 0.2, 0.2))
plot(LatentSpace, col = col, pch = 16, cex = 1.2, xlab = expression(u[i1]), ylab = expression(u[~~i2]))
legend("topleft",
  legend = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
  bty = "n", col = c("#E64A45", "#F2C249", "#3D4C53", "#4DB3B3", "#E6772E"),
  pch = 16, pt.cex = 1.5
)
text(LatentSpace[LatentSelectIndex, ], labels = countryname[LatentSelectIndex], cex = 1, pos = 4, col = "black")

# text(LatentSpace, labels = countrycowc, cex = 0.7, pos = 4, col = "black")



# Additive Effects Figure
median(fitZNII0H$APM)
var(fitZNII0H$APM)

plot(density(fitZNII0H$APM))

head(countryname[order(fitZNII0H$APM)])
head(countryname[order(-fitZNII0H$APM)])

AdditiveSelect <- c(
  "USA", "COL", "SWZ", "ETH", "GMY", "COS", "CHN",
  "KZK", "SAF", "KEN"
)

AdditiveEff <- data.frame(
  "Effect" = fitZNII0H$APM, "Countryname" = countryname,
  "Countrycowc" = countrycowc
)[order(fitZNII0H$APM), ]
AdditiveSelectIndex <- match(AdditiveSelect, AdditiveEff$Countrycowc)


par(mai = c(1, 1.5, 0.5, 0.2))
plot(fitZNII0H$APM[order(fitZNII0H$APM)], 1:sum(current), col = c("#3D4C53"), pch = 16, xlab = expression(a[i]), ylab = "", yaxt = "n", cex.axis = 0.8)
axis(2, at = AdditiveSelectIndex, labels = AdditiveEff$Countryname[AdditiveSelectIndex], las = 1, cex.axis = 0.8, family = "serif")
abline(v = 0, col = "grey")





# Changes in model specifications ----------------------------------------------

plot(fitIQD1Q2)
plot(fitWIBWVH)


plot(fitSIXQ7Y)
plot(fitEQNO0V)
plot(fitGIPKVP)



# Extension AME R=2 No Intercept -----------------------------------------------
plot(fitFVAEMT)

fitFVAEMTparam <- cbind(fitFVAEMT$VC[, "va"], fitFVAEMT$BETA)
colnames(fitFVAEMTparam) <- c("VA", pars)
mcmc_trace(fitFVAEMTparam, facet_args = list(ncol = 2))

coda::effectiveSize(fitFVAEMT$BETA)
summary(fitFVAEMT)



# Extension AME drop SharedA Conflict-------------------------------------------
plot(fitEQNO0V)
coda::effectiveSize(fitEQNO0V$BETA)
summary(fitEQNO0V)


# Extension Drop all non significant except GDP DIST ECON SharedAllies POLITY---



# Extension Drop all covariates ------------------------------------------------
plot(fitSIXQ7Y)
summary(fitSIXQ7Y)
coda::effectiveSize(fitSIXQ7Y$BETA)

AdditiveSelect <- c(
  "USA", "COL", "SWZ", "ETH", "COS", "CHN",
  "KZK", "SAF", "KEN"
)
AdditiveEff <- data.frame(
  "Effect" = fitSIXQ7Y$APM, "Countryname" = countryname,
  "Countrycowc" = countrycowc
)[order(fitSIXQ7Y$APM), ]

AdditiveSelectIndex <- match(AdditiveSelect, AdditiveEff$Countrycowc)

par(mfrow = c(1, 1), mai = c(0.5, 1.5, 0.5, 0.2))

plot(fitSIXQ7Y$APM[order(fitSIXQ7Y$APM)], 1:sum(current), col = c("#3D4C53"), pch = 16, xlab = "", ylab = "", yaxt = "n", cex.axis = 0.8)
axis(2, at = AdditiveSelectIndex, labels = AdditiveEff$Countryname[AdditiveSelectIndex], las = 1, cex.axis = 0.8, family = "serif")
abline(v = 0, col = "grey")


countrycowc[countryname == "Belarus"]
LatentSelect <- c(
  "CUB", "USA", "DRC", "COL", "SAU", "AZE", "SEN", "LIB",
  "GHA", "KGY", "GMY", "FRN", "RUS", "NAM",
  "NAM", "AUL", "SWZ", "BUI"
)
LatentSelectIndex <- match(LatentSelect, countrycowc)

LatentSpace <- fitSIXQ7Y$U %*% diag(sqrt(fitSIXQ7Y$L))

par(mfrow = c(1, 1))
plot(LatentSpace, col = col, pch = 16, cex = 1.2, xlab = "", ylab = "")
legend("topleft",
  legend = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
  bty = "n", col = c("#E64A45", "#F2C249", "#3D4C53", "#4DB3B3", "#E6772E"),
  pch = 16, pt.cex = 1.5
)
text(LatentSpace[LatentSelectIndex, ], labels = countryname[LatentSelectIndex], cex = 1, pos = 4, col = "black")

# latent space characteristics are even stronger



# Extended Drop all non-significant --------------------------------------------
pars <- c(
  "GDP (log p.c.).node", "GeoDistance.dyad", "EconomicDep.dyad",
  "SharedAllies.dyad", "PoliticalSim.dyad"
)

coda::effectiveSize(fitGIPKVP$BETA)
summary(fitGIPKVP)
plot(fitGIPKVP)

mcmc_areas(fitGIPKVP$BETA)
mcmc_hist(fitGIPKVP$BETA, pars = pars)
mcmc_intervals(fitGIPKVP$BETA, pars = pars)



# Extended Quadruble Iterations  -----------------------------------------------
coda::effectiveSize(fitRU7J8K$BETA)
summary(fitRU7J8K)
plot(fitRU7J8K)

pars <- c(
  "intercept", "GDP (log p.c.).node", "GeoDistance.dyad", "CulturalSim.dyad", "EconomicDep.dyad",
  "SharedAllies.dyad", "ConflictInd.dyad", "PoliticalSim.dyad", "CapabilityRat.dyad"
)

mcmc_areas(fitRU7J8K$BETA)
mcmc_hist(fitRU7J8K$BETA, pars = pars)
mcmc_intervals(fitRU7J8K$BETA, pars = pars)

# Picture Trace Plots AME 100,000 vs 400,000 Goodness of Fit
par(mfrow = c(2, 2), mai = c(0.5, 0.5, 0.5, 0.5))
fitRU7J8Kparam <- cbind(fitRU7J8K$VC[, "va"], fitRU7J8K$BETA)
colnames(fitRU7J8Kparam) <- c("VA", "Intercept", pars[-1])
mcmc_trace(fitRU7J8Kparam, facet_args = list(ncol = 2), pars = c("VA", "Intercept"))

# AME 100,000 vs 400,000
par(mfrow = c(1, 2), mai = c(1, 0.5, 0.5, 0.5))
ht <- c(1200, 1200, 50, 100, 100)
for (k in c(1, 4)) {
  # Multiplicative Effects
  xlim <- range(c(fitRU7J8K$GOF[, k], fitZNII0H$GOF[, k], fitRU7J8K$GOF[1, k])) * c(.9, 1.1)
  hist(fitRU7J8K$GOF[-1, k],
    prob = TRUE, col = "yellowgreen", xlim = xlim, main = "", ylab = "",
    xlab = colnames(fitRU7J8K$GOF)[k], ylim = c(0, ht[k])
  )
  # AME
  clr <- c(col2rgb("slategrey") / 255, .75)
  hist(fitZNII0H$GOF[-1, k],
    prob = TRUE, add = TRUE,
    col = rgb(clr[1], clr[2], clr[3], clr[4])
  )

  abline(v = fitZNII0H$GOF[1, k], col = "red")
}


# Analysis Additive Effects
AdditiveSelect <- c(
  "USA", "COL", "SWZ", "ETH", "COS", "CHN",
  "KZK", "SAF", "KEN"
)
AdditiveEff <- data.frame(
  "Effect" = fitRU7J8K$APM, "Countryname" = countryname,
  "Countrycowc" = countrycowc
)[order(fitRU7J8K$APM), ]

AdditiveSelectIndex <- match(AdditiveSelect, AdditiveEff$Countrycowc)

par(mfrow = c(1, 1), mai = c(0.5, 1.5, 0.5, 0.2))

plot(fitRU7J8K$APM[order(fitRU7J8K$APM)], 1:sum(current), col = c("#3D4C53"), pch = 16, xlab = "", ylab = "", yaxt = "n", cex.axis = 0.8)
axis(2, at = AdditiveSelectIndex, labels = AdditiveEff$Countryname[AdditiveSelectIndex], las = 1, cex.axis = 0.8, family = "serif")
abline(v = 0, col = "grey")

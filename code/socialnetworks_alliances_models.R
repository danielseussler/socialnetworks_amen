# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# Models

library(amen)
library(bayesplot)
library(network)
library(scales)
library(tictoc)
library(xergm.common)


# Data Year 2000 ---------------------------------------------------------------
data("alliances")

allyNet <- allyNet[["2000"]]
lNet <- lNet[["2000"]]
LSP <- LSP[["2000"]]
warNet <- warNet[["2000"]]

allyNetMat <- as.matrix.network(allyNet)
countrycowc <- get.vertex.attribute(allyNet, "vertex.names")


# Drop former countries
former <- c("YAR", "YPR", "GFR", "GDR", "CZE", "YUG")
formerIndex <- match(former, countrycowc)
current <- !(countrycowc %in% former)

allyNet <- delete.vertices(allyNet, formerIndex)
allyNetMat <- allyNetMat[current, current]
lNet <- lNet[current, current]
LSP <- LSP[current, current]
warNet <- warNet[current, current]
contigMat <- contigMat[current, current]
countrycowc <- countrycowc[current]

cinc <- get.vertex.attribute(allyNet, "cinc") * 100
polity <- get.vertex.attribute(allyNet, "polity")

any(is.na(cinc))
any(is.na(polity))

# Load GDP Distance Trade Culture Conflict -------------------------------------
logGDP <- readRDS(file = "data/logGDP.rds")
GeoDistance <- readRDS(file = "data/GeoDistance.rds")
EconomicDep <- readRDS(file = "data/EconomicDep.rds")
CulturalSim <- readRDS(file = "data/CulturalSim.rds")
ConflictInd <- readRDS(file = "data/ConflictInd.rds")



# Nodal Covariates -------------------------------------------------------------
list.vertex.attributes(allyNet)

Xnode <- array(
  data = c(as.matrix(logGDP), cinc, polity),
  dim = c(sum(current), 3),
  dimnames = list(
    countrycowc,
    c("logGDP", "cinc", "polity")
  )
)

any(is.na(Xnode))



# Additional Specification -----------------------------------------------------
PoliticalSim <- array(data = 0, dim = c(sum(current), sum(current)))
CapabilityRat <- array(data = 0, dim = c(sum(current), sum(current)))

sum(get.vertex.attribute(allyNet, "cinc"))

# catch cinc = 0 issue with imputing the average of 0 and the second lowest
table(cinc)[2]
cinc[cinc == 0] <- 0.0013 / 2

for (i in 1:sum(current)) {
  for (j in 1:sum(current)) {
    # The Polity Index ranges from -10 to 10.
    # PoliticalSim[i, j] <- (20 - abs(polity[i] - polity[j])) / 20
    PoliticalSim[i, j] <- abs(polity[i] - polity[j])

    CapabilityRat[i, j] <- ifelse(cinc[i] / cinc[j] >= 1,
      log(cinc[i] / cinc[j]),
      log(cinc[j] / cinc[i])
    )
  }
}

any(is.infinite(CapabilityRat))
any(is.nan(CapabilityRat))

any(is.infinite(PoliticalSim))
any(is.nan(PoliticalSim))


# Scale Covariates -------------------------------------------------------------
# GeoDistance <- apply(GeoDistance, 2, rescale, to = c(0, 10), from = c(0, max(GeoDistance)))
GeoDistance <- log(GeoDistance + 1)


# Dyadic Covariates ------------------------------------------------------------
Xdyad <- array(
  data = c(
    GeoDistance, CulturalSim, EconomicDep, LSP,
    lNet, ConflictInd, PoliticalSim, CapabilityRat
  ),
  dim = c(sum(current), sum(current), 8),
  dimnames = list(
    countrycowc, countrycowc,
    c(
      "GeoDistance", "CulturalSim", "EconomicDep", "SharedAllies",
      "LatentNet", "ConflictInd", "PoliticalSim", "CapabilityRat"
    )
  )
)


# Preliminary Modeling ---------------------------------------------------------
# ANOVA Decomposition
Rowcountry <- matrix(rownames(allyNetMat), nrow(allyNetMat), ncol(allyNetMat))
Colcountry <- t(Rowcountry)

anova(lm(c(allyNetMat) ~ c(Rowcountry) + c(Colcountry)))

# Check Model Assumptions Indicates a large degree of heterogeneity, more as if a_i or b_i were all zero.

rmean <- rowMeans(allyNetMat, na.rm = TRUE)
cmean <- colMeans(allyNetMat, na.rm = TRUE)
muhat <- mean(allyNetMat, na.rm = TRUE)
ahat <- rmean - muhat
bhat <- cmean - muhat

# additive effects and importer effects
head(sort(ahat, decreasing = TRUE))
head(sort(bhat, decreasing = TRUE)) 

# covariance and correlation between row and column effects - same as we have dyadic data
cov(cbind(ahat, bhat))
cor(ahat, bhat)

# estimate of dyadic covariance and correlation, does it make sense here?
R <- allyNetMat - (muhat + outer(ahat, bhat, "+"))
cov(cbind(c(R), c(t(R))), use = "complete")
cor(c(R), c(t(R)), use = "complete")



# Model Comparison -------------------------------------------------------------

# AME
fitZNII0H <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , -5], R = 2, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = TRUE,
  nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitZNII0H, file = "analysis/models/fitZNII0H.rds")
fitZNII0H <- readRDS(file = "analysis/models/fitZNII0H.rds")


# AME without multiplicative effects (SRRM Model)
fitCPLTUK <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , -5], R = 0, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = TRUE,
  nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitCPLTUK, file = "analysis/models/fitCPLTUK.rds")
fitCPLTUK <- readRDS(file = "analysis/models/fitCPLTUK.rds")


# AME without additive effects
fitX7XDFO <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , -5], R = 2, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = FALSE,
  nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitX7XDFO, file = "analysis/models/fitX7XDFO.rds")
fitX7XDFO <- readRDS(file = "analysis/models/fitX7XDFO.rds")


# AME without multiplicative and additive effects (Classical Regression)
fitDASD8R <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , -5], R = 0, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = FALSE,
  nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitDASD8R, file = "analysis/models/fitDASD8R.rds")
fitDASD8R <- readRDS(file = "analysis/models/fitDASD8R.rds")




# Model Analysis Extended-------------------------------------------------------

# Extended: R5 Improvement?
fitIQD1Q2 <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , -5], R = 5, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = TRUE,
  nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitIQD1Q2, file = "analysis/models/fitIQD1Q2.rds")
fitIQD1Q2 <- readRDS(file = "analysis/models/fitIQD1Q2.rds")


# Extended: R5 Improvement? Only ME
fitWIBWVH <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , -5], R = 5, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = FALSE,
  nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitWIBWVH, file = "analysis/models/fitWIBWVH.rds")
fitWIBWVH <- readRDS(file = "analysis/models/fitWIBWVH.rds")


# Extended: Does dropping the intercept improve mcmc estiamtion?
fitFVAEMT <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , -5], R = 2, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = TRUE, intercept = FALSE,
  nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitFVAEMT, file = "analysis/models/fitFVAEMT.rds")
fitFVAEMT <- readRDS(file = "analysis/models/fitFVAEMT.rds")


# Extended: Drop all covariates
fitSIXQ7Y <- ame(allyNetMat,
  R = 2, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = TRUE,
  nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitSIXQ7Y, file = "analysis/models/fitSIXQ7Y.rds")
fitSIXQ7Y <- readRDS(file = "analysis/models/fitSIXQ7Y.rds")


# Extended: Does dropping covariates yield different estimates? Drop SharedAllies ConflictInd
fitEQNO0V <- ame(allyNetMat,
  Xrow = Xnode[, c("logGDP")], Xcol = Xnode[, c("logGDP")],
  Xdyad = Xdyad[, , c(1, 2, 3, 7, 8)], R = 2, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = TRUE,
  intercept = TRUE, nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitEQNO0V, file = "analysis/models/fitEQNO0V.rds")
fitEQNO0V <- readRDS(file = "analysis/models/fitEQNO0V.rds")


# Extended: Does dropping covariates yield different estimates? Drop all non significant except GDP DIST ECON SharedAllies POLITY
fitGIPKVP <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , c(1, 3, 4, 7)], R = 2, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = TRUE,
  intercept = TRUE, nscan = 100000, burn = 10000, odens = 100
)
saveRDS(fitGIPKVP, file = "analysis/models/fitGIPKVP.rds")
fitGIPKVP <- readRDS(file = "analysis/models/fitGIPKVP.rds")


# Extended: 4x Estimation time
fitRU7J8K <- ame(allyNetMat,
  Xrow = logGDP, Xcol = logGDP,
  Xdyad = Xdyad[, , -5], R = 2, family = "bin", symmetric = TRUE,
  rvar = FALSE, cvar = FALSE, nvar = TRUE,
  intercept = TRUE, nscan = 400000, burn = 100000, odens = 100
)
saveRDS(fitRU7J8K, file = "analysis/models/fitRU7J8K.rds")
fitRU7J8K <- readRDS(file = "analysis/models/fitRU7J8K.rds")
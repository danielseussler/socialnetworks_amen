# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# Daniel A. Seussler Becerra

library(amen)
library(network)
library(statnet)
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
former <- c("YAR", "YPR", "GFR", "GDR", "CZE")
formerIndex <- match(former, countrycowc)
current <- !(countrycowc %in% former)

allyNet <- delete.vertices(allyNet, formerIndex)
allyNetMat <- allyNetMat[current, current]
lNet <- lNet[current, current]
LSP <- LSP[current, current]
warNet <- warNet[current, current]
contigMat <- contigMat[current, current]
countrycowc <- countrycowc[current]

cinc <- get.vertex.attribute(allyNet, "cinc")[current] * 100
polity <- get.vertex.attribute(allyNet, "polity")[current]


# Load Distance Trade Culture Conflict -----------------------------------------
GeoDistance <- readRDS(file = "data/GeoDistance.rds")
TradeFlows <- readRDS(file = "data/TradeFlows.rds")
CulturalSim <- readRDS(file = "data/CulturalSim.rds")
ConflictInd <- readRDS(file = "data/Conflict.rds")
logGDP <- readRDS(file = "data/logGDP.rds")


# Nodal Covariates -------------------------------------------------------------
list.vertex.attributes(allyNet)
cinc[is.na(cinc)] <- 0 # Missing Data imputation?
polity[is.na(polity)] <- 0

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

for (i in 1:sum(current)) {
  for (j in 1:sum(current)) {
    # The Polity Index ranges from -10 to 10.
    PoliticalSim[i, j] <- (20 - abs(polity[i] - polity[j])) / 20

    CapabilityRat[i, j] <- ifelse(cinc[i] / cinc[j] >= 1,
      log(cinc[i] / cinc[j]),
      log(cinc[j] / cinc[i])
    )
  }
}

CapabilityRat[is.infinite(CapabilityRat)] <- 0 # Correction for CINC = 0 Issue
CapabilityRat[is.nan(CapabilityRat)] <- 0



# Dyadic Covariates ------------------------------------------------------------
Xdyad <- array(
  data = c(GeoDistance, CulturalSim, TradeFlows, LSP, 
           lNet, ConflictInd, PoliticalSim, CapabilityRat),
  dim = c(sum(current), sum(current), 8),
  dimnames = list(countrycowc, countrycowc, 
                  c("GeoDistance", "CulturalSim", "TradeFlows", "SharedAllies", 
                    "LatentNet", "ConflictInd", "PoliticalSim", "CapabilityRat"))
)


# Preliminary Modeling ---------------------------------------------------------
# ANOVA Decomposition 
Rowcountry <- matrix(rownames(Y), nrow(Y), ncol(Y))
Colcountry <- t(Rowcountry)

anova(lm(c(Y) ~ c(Rowcountry) + c(Colcountry)))

  # Check Model Assumptions Indicates a large degree of heterogeneity, more as if a_i or b_i were all zero.


rmean <- rowMeans(Y, na.rm = TRUE)
cmean <- colMeans(Y, na.rm = TRUE)
muhat <- mean(Y, na.rm = TRUE)
ahat <- rmean - muhat
bhat <- cmean - muhat

# additive "exporter" effects and importer effects
head(sort(ahat, decreasing = TRUE))
head(sort(bhat, decreasing = TRUE)) # unsurprisingly the same as before

# covariance and correlation between row and column effects - same as we have dyadic data
cov(cbind(ahat, bhat))
cor(ahat, bhat)

# estimate of dyadic covariance and correlation, does it make sense here?
R <- Y - (muhat + outer(ahat, bhat, "+"))
cov(cbind(c(R), c(t(R))), use = "complete")
cor(c(R), c(t(R)), use = "complete")



# The Social Relations Model ---------------------------------------------------
fit_SRM <- ame(Y, family = "bin", symmetric = TRUE)
saveRDS(fit_SRM, file = "analysis/models/SRMmodel.rds")

fit_SRM <- readRDS(file = "analysis/models/SRMmodel.rds")
summary(fit_SRM)


# Reduced Model that lacks SRM terms of column effects
fit_SRG <- ame(Y, family = "bin", symmetric = TRUE, rvar = FALSE, cvar = FALSE, dcor = FALSE)
saveRDS(fit_SRG, file = "analysis/models/SRGmodel.rds")

fit_SRG <- readRDS(file = "analysis/models/SRGmodel.rds")
summary(fit_SRG)



# Model with added Nodal Covariates Cinc and Polity
fit_SRRM_nodal <- ame(Y, Xrow = Xno[, 2:3], Xcol = Xno[, 2:3], family = "bin", symmetric = TRUE)
saveRDS(fit_SRRM_nodal, file = "analysis/models/fit_SRRM_nodal.rds")

fit_SRRM_nodal <- readRDS(file = "analysis/models/fit_SRRM_nodal.rds")
summary(fit_SRRM_nodal)




# Model with added Nodal Covariates Cinc and Polity + All Dyad
fit_SRRM_nodal_dayd <- ame(Y, Xdyad = Xdyad, Xrow = Xno[, 2:3], Xcol = Xno[, 2:3], family = "bin", symmetric = TRUE)
saveRDS(fit_SRRM_nodal_dayd, file = "analysis/models/fit_SRRM_nodal_dayd.rds")

fit_SRRM_nodal <- readRDS(file = "analysis/models/fit_SRRM_nodal.rds")
summary(fit_SRRM_nodal)



################################################################################

# AME Model: Full Specification with Intercept Rank 1
fitAMER1 <- ame(allyNetMat, Xrow = Xnode[ , c("logGDP", "cinc")], Xcol = Xnode[, c("logGDP", "cinc")], 
                Xdyad = Xdyad[ , , -5], R = 1, family = "bin", symmetric = TRUE, 
                nscan = 100000, burn = 10000, odens = 100)
saveRDS(fitAMER1, file = "analysis/models/fitAMER1.rds")







# AME Model: Full Specification without Intercept Rank 1
fitAMER1noI <- ame(allyNetMat, Xrow = Xnode[ , c("logGDP", "cinc")], Xcol = Xnode[, c("logGDP", "cinc")],
                Xdyad = Xdyad[ , , -5], R = 1, family = "bin", symmetric = TRUE, 
                intercept = FALSE, nscan = 100000, burn = 10000, odens = 100)
saveRDS(fitAMER1noI, file = "analysis/models/fitAMER1noI.rds")










# Modified summary function of amen package to output a table ------------------
table_ame <- function(object, ...){ 
  fit <- object
  tmp <- cbind(
    apply(fit$BETA, 2, mean), apply(fit$BETA, 2, sd),
    apply(fit$BETA, 2, mean) / apply(fit$BETA, 2, sd),
    2 * (1 - pnorm(abs(apply(fit$BETA, 2, mean) / apply(fit$BETA, 2, sd))))
  )
  colnames(tmp) <- c("pmean", "psd", "z-stat", "p-val")
  out <- round(tmp, 4)
  
  
  tmp <- cbind(apply(fit$VC, 2, mean), apply(fit$VC, 2, sd))
  tmp <- cbind(round(tmp, 4), array("-", dim = c(nrow(tmp), 2)))

  out <- rbind(out, tmp)
  return(out)
} 

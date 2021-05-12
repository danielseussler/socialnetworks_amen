# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# Daniel A. Seussler Becerra

library(amen)
library(statnet)
library(xergm.common)
library(tictoc)



# Data Preparation Year 2000 ---------------------------------------------------
data("alliances")

allyNet2000 <- allyNet[["2000"]]
lNet2000 <- lNet[["2000"]]
LSP2000 <- LSP[["2000"]]
warNet2000 <- warNet[["2000"]]
countries <- colnames(contigMat)

Y <- as.matrix.network(allyNet2000)



# ANOVA Decomposition ----------------------------------------------------------
Rowcountry <- matrix(rownames(Y), nrow(Y), ncol(Y))
Colcountry <- t(Rowcountry)

anova(lm(c(Y) ~ c(Rowcountry) + c(Colcountry)))

  # Check Model Assumptions Indicates a large degree of heterogeneity, more as if a_i or b_i were all zero.



# ---- comparison of countries in terms of row and column means (will be the same)
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


# Define Nodal Covariates
list.vertex.attributes(allyNet2000)
Xno <- cbind(get.vertex.attribute(allyNet2000, "year"),
             get.vertex.attribute(allyNet2000, "cinc"),
             get.vertex.attribute(allyNet2000, "polity"),
             get.vertex.attribute(allyNet2000, "na"))
rownames(Xno) <- get.vertex.attribute(allyNet2000, "vertex.names")
colnames(Xno) <- c("Year", "cinc", "polity", "na")


# Model with added Nodal Covariates Cinc and Polity
fit_SRRM_nodal <- ame(Y, Xrow = Xno[, 2:3], Xcol = Xno[, 2:3], family = "bin", symmetric = TRUE)
saveRDS(fit_SRRM_nodal, file = "analysis/models/fit_SRRM_nodal.rds")

fit_SRRM_nodal <- readRDS(file = "analysis/models/fit_SRRM_nodal.rds")
summary(fit_SRRM_nodal)


# Define Dyadic Covariates
Xdyad <- array(data = c(contigMat, lNet2000, LSP2000, warNet2000),
               dim = c(164,164,4),
               dimnames = list(get.vertex.attribute(allyNet2000, "vertex.names"),
                               get.vertex.attribute(allyNet2000, "vertex.names"),
                               c("contigMat", "lNet", "LSP", "warNet")))

# Model with added Nodal Covariates Cinc and Polity + All Dyad
fit_SRRM_nodal_dayd <- ame(Y, Xdyad = Xdyad, Xrow = Xno[, 2:3], Xcol = Xno[, 2:3], family = "bin", symmetric = TRUE)
saveRDS(fit_SRRM_nodal_dayd, file = "analysis/models/fit_SRRM_nodal_dayd.rds")

fit_SRRM_nodal <- readRDS(file = "analysis/models/fit_SRRM_nodal.rds")
summary(fit_SRRM_nodal)




# Full AME Model ---------------------------------------------------------------
fit_AME_R2 <- ame(Y, Xdyad = Xdyad, Xrow = Xno[, 2:3], R = 3, Xcol = Xno[, 2:3], family = "bin", symmetric = TRUE)
saveRDS(fit_AME_R2, file = "analysis/models/fit_AME_R2.rds")

fit_AME_R2 <- readRDS(file = "analysis/models/fit_AME_R2.rds")
summary(fit_AME_R2)





# Comparison of R - Dimension of Latent Factors --------------------------------
fit_AME_R3 <- ame(Y, Xdyad = Xdyad, Xrow = Xno[, 2:3], R = 3, Xcol = Xno[, 2:3], family = "bin", symmetric = TRUE)
saveRDS(fit_AME_R3, file = "analysis/models/fit_AME_R3.rds")

fit_AME_R3 <- readRDS(file = "analysis/models/fit_AME_R3.rds")
summary(fit_AME_R3)






# Time Series Data -------------------------------------------------------------
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

fit_AME_Rep_R3 <- readRDS(file = "analysis/models/fit_AME_Rep_R3.rds")
summary(fit_AME_Rep_R3)




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

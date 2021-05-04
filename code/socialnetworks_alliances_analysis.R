# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# Daniel A. Seussler Becerra

library(amen)
library(statnet)
library(xergm.common)
library(ggnetwork)

# Data of Year 2000 ------------------------------------------------------------
data("alliances")

allyNet2000 <- allyNet[["2000"]]
lNet2000 <- lNet[["2000"]]
LSP2000 <- LSP[["2000"]]
warNet2000 <- warNet[["2000"]]
countries <- colnames(contigMat)

rm("allyNet", "lNet", "LSP", "warNet")


# SRRM Model -------------------------------------------------------------------








# Reduced (MCMC wise) AME Model ------------------------------------------------








# Full AME Model ---------------------------------------------------------------







# Comparison of R - Dimension of Latent Factors --------------------------------


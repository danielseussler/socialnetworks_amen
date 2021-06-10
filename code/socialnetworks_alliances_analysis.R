# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model
# Analysis 

library(amen)
library(bayesplot)
library(ggplot2)


# extended summary function of amen package to output a table ------------------
table_ame <- function(object, ...){ 
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

my_theme <- theme_minimal() + theme(text=element_text(size=20, family = "serif"))


ggplot2::theme_set(my_theme)
color_scheme_set("darkgray")

pars = c("logGDP.node", "GeoDistance.dyad", "CulturalSim.dyad", "TradeFlows.dyad", "SharedAllies.dyad", "ConflictInd.dyad", "PoliticalSim.dyad", "CapabilityRat.dyad" )
fitZNII0H <- readRDS(file = "analysis/models/fitZNII0H.rds")
coda::effectiveSize(fitZNII0H$BETA)
summary(fitZNII0H)
plot(fitZNII0H)
mcmc_areas(fitZNII0H$BETA)
mcmc_hist(fitZNII0H$BETA, pars = pars)
mcmc_intervals(fitZNII0H$BETA, pars = pars)


pars2 = c("logGDP.node", "GeoDistance.dyad", "CulturalSim.dyad", "TradeFlows.dyad", "ConflictInd.dyad", "PoliticalSim.dyad", "CapabilityRat.dyad" )
fitCPLTUK <- readRDS(file = "analysis/models/fitCPLTUK.rds")
coda::effectiveSize(fitCPLTUK$BETA)
summary(fitCPLTUK)
plot(fitCPLTUK)
mcmc_hist(fitCPLTUK$BETA)
mcmc_areas(fitCPLTUK$BETA)
mcmc_intervals(fitCPLTUK$BETA, pars = pars2)


# Comparison AME SRM
for(k in 1:5)
      {
        hist(fit$GOF[-1,k],xlim=range(fit$GOF[,k]),main="",prob=TRUE,
             xlab=colnames(fit$GOF)[k],col="lightblue",ylab="",yaxt="n")
             abline(v=fit$GOF[1,k],col="red")
      }
library(amen)
library(tictoc)
data(IR90s)

gdp <- IR90s$nodevars[, 2]
topgdp <- which(gdp >= sort(gdp, decreasing = TRUE)[30])

Y <- log(IR90s$dyadvars[, , 2] + 1)

fit_SRM <- ame(Y, family = "nrm")


#### nodal covariates
dimnames(IR90s$nodevars)
Xn <- IR90s$nodevars[ , ]
Xn[, 1:2] <- log(Xn[, 1:2])

### dyad covariates
dimnames(IR90s$dyadvars)
Xd <- IR90s$dyadvars[ , , c(1, 3, 4, 5)]
Xd[, , 3] <- log(Xd[, , 3])


fit_srrm <- ame(Y, Xd = Xd, Xr = Xn, Xc = Xn, family = "nrm")
summary(fit_srrm)


# iid assumption as linear reg
fit_rm <- ame(Y,
  Xd = Xd, Xr = Xn, Xc = Xn, family = "nrm",
  rvar = FALSE, cvar = FALSE, dcor = FALSE
)
summary(fit_rm)

# nodal effects as dyadic covariates
fit_srrm0 <- ame(Y, Xd = Xd[, , 1:2], Xr = Xn, Xc = Xn, family = "nrm")
summary(fit_srrm0)

# multiplicative addition: latent variable
fit_ame2 <- ame(Y, Xd = Xd, Xr = Xn, Xc = Xn, R = 2, family = "nrm")
summary(fit_ame2)


# Circplot with amen.R github
mu <- mean(fit_ame2$BETA[, 1])
br <- apply(fit_ame2$BETA[, 1 + 1:3], 2, mean)
bc <- apply(fit_ame2$BETA[, 1 + 3 + 1:3], 2, mean)
bd <- apply(fit_ame2$BETA[, 1 + 6 + 1:4], 2, mean)

# expected net export behaviour of countries?
EY2 <- mu +
  Xbeta(Xd, bd) +
  (Xn %*% br + fit_ame2$APM) %*% rep(1, nrow(Y)) +
  t((Xn %*% bc + fit_ame2$BPM) %*% rep(1, nrow(Y)))

circplot(1 * (Y - EY2 > 0), fit_ame2$U, fit_ame2$V, pscale = 1.1)

rm(list = ls())


# Modeling Binary Outcomes  ----------------------------------------------------
data("lazegalaw")

Y <- lazegalaw$Y[, , 2]
Xd <- lazegalaw$Y[, , -2]
Xn <- lazegalaw$X

dimnames(Xd)

dimnames(Xn)

netplot(lazegalaw$Y[, , 2], ncol = Xn[, 3], seed = 42)

# SRM Model with row column within dyad var
fit_SRM <- ame(Y, family = "bin")
summary(fit_SRM)

# Reduced Model that lacks SRM Terms
fit_SRG <- ame(Y, rvar = FALSE, cvar = FALSE, dcor = FALSE, family = "bin")
summary(fit_SRG)

# Include Regression Coefficients
Xno <- Xn[, c(1, 2, 4, 5, 6)]
fit_SRRM <- ame(Y, Xd = Xd, Xr = Xno, Xc = Xno, family = "bin")
summary(fit_SRRM)

# Same but symmetric
fit_SRRMsym <- ame(Y, Xd = Xd, Xr = Xno, Xc = Xno, symmetric = TRUE, family = "bin")
summary(fit_SRRMsym)

# Scrap nonsignificant + Latent Variable Model
fit_AME <- ame(Y, Xd = Xd[, , 2], R = 3, family = "bin")
fit_AMEsym <- ame(Y, Xd = Xd[, , 2], R = 3, symmetric = TRUE, family = "bin")

summary(fit_AME)
summary(fit_AMEsym)


U <- fit_AME$U
V <- fit_AME$V

round(cor(U, Xno), 2)
round(cor(V, Xno),2)



# Latent Variable viz with categorical value
xlb<-c(expression(u[1]),expression(u[2]),expression(u[3]) ) 
ylb<-c(expression(v[1]),expression(v[2]),expression(v[3]) ) 
par(mfcol=c(1,3))
for(k in 1:3)
{
  plot(U[,k],V[,k] , xlab=xlb[k],ylab=ylb[k],
       pch=1+(Xn[,1]-1)*15,
       col=Xn[,3]+2 ,
       xlim=range(U),ylim=range(V)) 
  abline(h=0,lty=2,col="gray") ; abline(v=0,lty=2,col="gray")
}




# SRRM Time Model
rm(list = ls())

data("dutchcollege")

#outcome 
Y = 1*(dutchcollege$Y >= 2)[,,2:7]
n <- dim(Y)[1]; t <- dim(Y)[3]

#nodal covariates
Xnode <- dutchcollege$X[,1:2]
Xnode <- array(Xnode, dim = c(n, ncol(Xnode), t))

dimnames(Xnode)[[2]] <- c("male", "smoker")

# dyadic covariates
Xdyad <- array(dim = c(n,n,5,t))      
Xdyad[,,1,]<-1*( dutchcollege$Y >= 2 )[,,1:6]                     # lagged value
Xdyad[,,2,]<-array(apply(Xdyad[,,1,],3,t),dim=c(n,n,t))           # lagged reciprocal value 
Xdyad[,,3,]<-tcrossprod(Xnode[,1,1])                              # both male
Xdyad[,,4,]<-tcrossprod(Xnode[,2,1])                              # both smokers 
Xdyad[,,5,]<-outer( dutchcollege$X[,3],dutchcollege$X[,3],"==")   # same program
dimnames(Xdyad)[[3]]<-c("Ylag","tYlag","bothmale","bothsmoke","sameprog")

tic("Go")
fit_ar1 <- ame_rep(Y, Xdyad = Xdyad, Xrow = Xnode, Xcol = Xnode, family = "bin")
toc()
summary(fit_ar1)



# Symmetric Case
data("coldwar")

# response
Y <- sign( apply(coldwar$cc, c(1,2), mean))

# nodal covariates
Xn <- cbind(apply(log(coldwar$gdp), 1, mean), 
            sign(apply(coldwar$polity, 1, mean)))

Xn[, 1] <- Xn[, 1] - mean(Xn[, 1])
dimnames(Xn)[[2]] <- c("lgdp", "polity")

#dyadic covariates
Xd <- array(dim = c(nrow(Y), nrow(Y), 3))
Xd[,,1] <- tcrossprod(Xn[,1]) #gdp interaction
Xd[,,2] <- tcrossprod(Xn[,2]) #polity interaction
Xd[,,3] <- log(coldwar$distance) #log distance

dimnames(Xd)[[3]] <- c("igdp", "ipol", "ldist")

tic("Coldwar Model")
fit_cw_R1 <- ame(Y, Xd, Xn, R = 1, family = "ord", symmetric = TRUE, 
                 burn = 1000, nscan = 100000, odens = 100)
toc()

# How to save model outputs
saveRDS(fit_cw_R1, "coldwarmodel.rds")
rm(fit_cw_R1)
my_model <- readRDS("coldwarmodel.rds")
summary(my_model)

## ----fig.height=5,fig.width=7,echo=FALSE---------------------------------
u<-fit_cw_R1$U
cnames<-rownames(u)
urank<-rank(u) 

plot(urank,u,type="n",xlab="rank order of u")
abline(h=0,col="gray") 
addlines(Y>0 , cbind(urank,u),col="green")
addlines(Y<0 , cbind(urank,u),col="pink")
text(urank,u,cnames,srt=-45,cex=1.0) 
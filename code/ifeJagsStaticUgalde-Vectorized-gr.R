# -*- coding: utf-8 -*-

##################################################################################
# Ugalde et al a la Bonica
# This code runs a Bonica-like algorithm to provide a dynamic view of Ugalde's
# IFE.  The run has already been saved ("DynUgaldeBonica.RData")
# March 19, 2013: Add party-based priors for councilors coming in from outside
# March 21, 2013: Added more precise priors for new entrants:
#	PRI-sponsored=N( 2,4)
#	PAN-sponsored=N(-1,4)
#	PRD-sponsored=N(-2,4)
##################################################################################

library(arm)
library (MCMCpack)
library (foreign)
library (car)
library (gtools)
#library (multicore)
library (R2jags)
library (mcmcplots)
library (sm)
library(lubridate)


rm(list = ls())
workdir <- c("/home/eric/Dropbox/data/rollcall/ife_cg/ife-update/data/")
setwd(workdir)

# Define colors and plotting names
# OJO: en tenure term==10 es a, term==11 es b etc. 
ids <- matrix(c("Ugalde",           "ugalde",      "PRI",  "4",
                "Albo",             "albo",        "PAN",  "456",
                "Andrade",          "andrade",     "PRI",  "4567", 
                "Gmz. Alcántar",    "alcantar",    "PVEM", "4567",
                "Glez. Luna",       "glezluna",    "PAN",  "456",
                "Latapí",           "latapi",      "PRI",  "45",
                "López Flores",     "lopezflores", "PRI",  "456",
                "Morales",          "morales",     "PAN",  "45",
                "Sánchez",          "sanchez",     "PAN",  "4567c",
                "Valdés",           "valdes",      "PRD",    "6789a",
                "Baños",            "banos",       "PRI",    "6789abcde",
                "Nacif",            "nacif",       "PAN",    "6789abcde",
                "Elizondo",         "elizondo",    "PAN",     "789a",
                "Figueroa",         "figueroa",    "PRD",     "789a",
                "Guerrero",         "guerrero",    "PRI",     "789a",
                "Córdova",          "cordova",     "PRD",       "9abcdef",
                "García Rmz.",      "garcia",      "PRI",       "9"  ,
                "Marván",           "marvan",      "PAN",       "9ab",
                "E. Andrade",       "andrade2",    "",             "cde",
                "Favela",           "favela",      "",             "cdef",
                "Santiago",         "santiago",    "",             "c",
                "Galindo",          "galindo",     "",             "c",
                "Murayama",         "murayama",    "",             "cdef",
                "Ruiz Saldaña",     "ruiz",        "",             "cdef",
                "San Martín",       "snmartin",    "",             "cde",
                "Santiago",         "santiago",    "",             "c",
                "Ravel",            "ravel",       "",              "def",
                "J. Rivera",        "rivera2",     "",              "def",
                "Zavala",           "zavala",      "",              "def",
                "De la Cruz",       "magana",      "",                "f",
                "Faz",              "faz",         "",                "f",
                "Humphrey",         "humphrey",    "",                "f",
                "Kib Espadas",      "kib",         "",                "f"),
              ncol = 4,
              byrow = TRUE)
#
ids <- as.data.frame(ids, stringsAsFactors = FALSE)
colnames(ids) <- c("name", "column", "pty", "tenure")                                           
#ids$tenure <- as.numeric(ids$tenure)
ids <- within(ids, party <- ifelse (pty=="PRI", 1,
                            ifelse (pty=="PAN", 2,
                            ifelse (pty=="PRD", 3, 
                            ifelse(pty=="PVEM", 4, 5)))))
ids <- within(ids, color <- ifelse (pty=="PRI", "red",
                            ifelse (pty=="PAN", "blue",
                            ifelse (pty=="PRD", "gold",
                            ifelse(pty=="PVEM", "green", "orangered4")))))

# select terms 4-8, more or less
tees <- 6:7
T <- length(tees)

sel    <- grep(pattern = "[67]", ids$tenure) #sel    <- grep(pattern = "[456789ab]", ids$tenure)
name   <- ids$name[sel]
party  <- ids$party[sel]
color  <- ids$color[sel]
column <- ids$column[sel]

## rgb.23 <- c(length=11)
## rgb.23[c(1,6,8,10)] <- rgb(1,       0, 0, 0.6) #red
## rgb.23[c(2:4,9)]    <- rgb(1, 215/255, 0, 0.6) #gold
## rgb.23[c(5,7,11)]   <- rgb(0,       0, 1, 0.6) #blue

###############################################################################
## Read votes (includes informative votes only, exported by code/data-prep.r ##
###############################################################################
vot <-read.csv("v456789ab.csv",  header=TRUE)
#


##########################
## Rosas, vectorization ##
##########################
sel <- which(vot$dunan==1)
vot <- vot[-sel,] # drop uncontested votes

library (runjags)
library (coda)
rc <- as.data.frame(t(vot[,1:18]))
leg.index  <- 1:nrow(rc)
vote.index <- 1:ncol(rc)

## Melt RC
rc.2 <- as.data.frame (rc)
colnames (rc.2) <- vote.index
rc.2$leg <- leg.index
molten.rc <- reshape2::melt(rc.2, id.vars="leg", variable.name="vote", value.name="rc")
molten.rc$rc <- car::recode (molten.rc$rc, "0=NA")
molten.rc <- na.omit (molten.rc)
molten.rc$rc <- car::recode (molten.rc$rc, "2=0; c(3,4,5)=NA")

cjr.data.vector <- dump.format(list(y=molten.rc$rc
                                    , n.legs=max(leg.index)
                                    , n.item=max(vote.index)
                                    , n.obs=nrow(molten.rc)
                                    , vote=molten.rc$vote
                                    , dep=molten.rc$leg
))

cjr.parameters = c("theta", "alpha", "beta", "deviance")

cjr.inits <- function() {
  dump.format(
    list(
#      theta = c(NA, NA, rnorm(max(leg.index)-2))
      theta = c(rnorm(3), NA, rnorm(4), NA, rnorm(max(leg.index)-9))
      , alpha = rnorm(max(vote.index))
      , beta = rnorm(max(vote.index))
      ,'.RNG.name'="base::Wichmann-Hill"
      ,'.RNG.seed'= 1971)   #randomNumbers(n = 1, min = 1, max = 1e+04,col=1))
  )
}

cjr.vector="model {
	for (i in 1:n.obs) {
		y[i] ~ dbern (pi[i])
		probit(pi[i]) <- beta[vote[i]]*theta[dep[i]] - alpha[vote[i]]
	}
# PRIORS
for (j in 1:n.item){ alpha[j] ~ dnorm(0, 0.25) }   
# Beta (discrimination, dimension 1)
for (j in 1:n.item){ beta[j] ~ dnorm(0, 0.1) }   
# ideal points
#theta[4] <- 1
#theta[9] <- 0 
theta[4] ~ dnorm( 1,4)T(0,) # normal + truncada
theta[9] ~ dnorm(-1,4)T(,0) # normal - truncada

for(i in 1:3)  { theta[i] ~ dnorm(0,1) }
for(i in 5:8)  { theta[i] ~ dnorm(0,1) }
for(i in 10:n.legs)  { theta[i] ~ dnorm(0,1) }
}"

cjr.model.v <- run.jags(
  model=cjr.vector,
  monitor=cjr.parameters,
  method="parallel",
  n.chains=2,
  data=cjr.data.vector,
  inits=list (cjr.inits(), cjr.inits()),
  #thin=50, burnin=10000, sample=200,
  thin=5, burnin=200, sample=200,
  check.conv=FALSE, plots=FALSE)

chainsCJR.v <- mcmc.list(list (cjr.model.v$mcmc[[1]], cjr.model.v$mcmc[[2]]))
gelman.diag (chainsCJR.v, multivariate=F) # convergence looks fine for both models

Alpha.v <- rbind ( chainsCJR.v[[1]][,grep("alpha", colnames(chainsCJR.v[[1]]))]
                   , chainsCJR.v[[2]][,grep("alpha", colnames(chainsCJR.v[[2]]))])
Beta.v <- rbind ( chainsCJR.v[[1]][,grep("beta", colnames(chainsCJR.v[[1]]))]
                  , chainsCJR.v[[2]][,grep("beta", colnames(chainsCJR.v[[2]]))])
Theta.v <- rbind ( chainsCJR.v[[1]][,grep("theta", colnames(chainsCJR.v[[1]]))]
                   , chainsCJR.v[[2]][,grep("theta", colnames(chainsCJR.v[[2]]))])

plot (colMeans (Alpha.v))  # difficulties
plot (colMeans (Beta.v))   # signal

par (las=2, mar=c(7,3,2,2))
plot (1:length(vot$date), colMeans (Beta.v)
      , type="n"
      , axes=F, ylim=c(-1,1)
      , xlab="", ylab="Ideal point")
axis (1, at=seq(1,length(vot$date),8)
      , labels=vot$date[seq(1,length(vot$date),8)], cex=0.8)
par (las=0)
mtext (side=1, line=6, text="Dates")
axis (2)
for (i in 1:ncol(Theta.v)){
    segments (x0=min (c(1:length(vot$date))[vot[,i] != 0])
          , x1=max (c(1:length(vot$date))[vot[,i] != 0])
          , y0=colMeans (Theta.v)[i]
          , y1=colMeans (Theta.v)[i]
          , col=color[i], lwd=3)
}


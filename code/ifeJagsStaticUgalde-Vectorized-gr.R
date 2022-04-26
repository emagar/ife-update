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
ids <- read.csv("../ancillary/consejo-general-ine.csv")
ids <- ids[, -grep("^x", colnames(ids))] # drop redundant "x" columns
ids[1,]
#ids$tenure <- as.numeric(ids$tenure)
ids <- within(ids, party <- ifelse(sponsor==   "pri",   1,
                            ifelse(sponsor==   "pan",   2,
                            ifelse(sponsor==   "prd",   3, 
                            ifelse(sponsor==  "pvem",   4,
                            ifelse(sponsor=="morena",   5,  6))))))
ids <- within(ids, color <- ifelse(sponsor==   "pri",        "red",
                            ifelse(sponsor==   "pan",       "blue",
                            ifelse(sponsor==   "prd",       "gold",
                            ifelse(sponsor==  "pvem",      "green",
                            ifelse(sponsor=="morena", "orangered4", "gray"))))))

# select terms 4-8, more or less
tees <- 4:11
T <- length(tees)

#sel    <- grep(pattern = "[67]", ids$tenure)
sel    <- grep(pattern = "[456789ab]", ids$tenure) # ugalde and valdés councils
name   <- ids$short[sel]
party  <- ids$party[sel]
color  <- ids$color[sel]
column <- ids$column[sel]

## rgb.23 <- c(length=11)
## rgb.23[c(1,6,8,10)] <- rgb(1,       0, 0, 0.6) #red
## rgb.23[c(2:4,9)]    <- rgb(1, 215/255, 0, 0.6) #gold
## rgb.23[c(5,7,11)]   <- rgb(0,       0, 1, 0.6) #blue

# adjusts approximate years with constant membership
yr.by.yr <- data.frame(
    n = 1:26, 
    cuts = c(
        ymd("19961031"), #  1
        ymd("19971031"), #  2
        ymd("19981031"), #  3
        ymd("19991031"), #  4
        ymd("20001211"), #  5
        ymd("20011031"), #  6
        ymd("20021031"), #  7
        ymd("20031031"), #  8
        ymd("20041031"), #  9
        ymd("20051031"), # 10
        ymd("20061031"), # 11
        ymd("20080215"), # 12
        ymd("20080821"), # 13
        ymd("20091031"), # 14
        ymd("20101031"), # 15
        ymd("20111215"), # 16
        ymd("20130220"), # 17
        ymd("20130401"), # 18
        ymd("20140411"), # 19
        ymd("20150411"), # 20
        ymd("20160411"), # 21
        ymd("20170405"), # 22
        ymd("20180405"), # 23
        ymd("20190405"), # 24
        ymd("20200417"), # 25
        ymd("20200723")  # 26
    ),
    term = c(2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 6, 7, 7, 8, 9, 10, 11, 12, 12, 12, 13, 14, 14, 15, 16)
)

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

## Melt RC (turn it into long format, see https://seananderson.ca/2013/10/19/reshape/)
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
theta[4] ~ dnorm( 1,4)T(0,) # normal + truncated
theta[9] ~ dnorm(-1,4)T(,0) # normal - truncated

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
  thin=50, burnin=10000, sample=200,
  # 			      thin=5, burnin=200, sample=200,
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


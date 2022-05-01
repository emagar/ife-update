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
library (runjags)
library (coda)

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

# select terms for analysis
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

# adjusts approximate years with constant membership for year-by-year estimations
yr.by.yr <- data.frame(
    n = 1:28, 
    start = c(
        ymd("19961031"), #  1
        ymd("19971031"), #  2
        ymd("19981031"), #  3
        ymd("19991031"), #  4
        ymd("20001211"), #  5
        ymd("20011031"), #  6
        ymd("20021031"), #  7
        ymd("20031105"), #  8
        ymd("20041105"), #  9
        ymd("20051105"), # 10
        ymd("20061105"), # 11
        ymd("20080215"), # 12
        ymd("20080821"), # 13
        ymd("20090821"), # 14
        ymd("20101031"), # 15
        ymd("20111215"), # 16
        ymd("20130220"), # 17
        ymd("20131031"), # 18
        ymd("20140411"), # 19
        ymd("20150411"), # 20
        ymd("20160411"), # 21
        ymd("20170405"), # 22
        ymd("20180405"), # 23
        ymd("20190405"), # 24
        ymd("20200417"), # 25
        ymd("20200723"), # 26
        ymd("20210723"), # 27
        ymd("20220723")  # 28
    ),
    end = c(
        ymd("19971030"), #  1
        ymd("19981030"), #  2
        ymd("19991030"), #  3
        ymd("20001210"), #  4
        ymd("20011030"), #  5
        ymd("20021030"), #  6
        ymd("20031104"), #  7
        ymd("20041104"), #  8
        ymd("20051104"), #  9
        ymd("20061104"), # 10
        ymd("20080214"), # 11
        ymd("20080820"), # 12
        ymd("20090820"), # 13
        ymd("20101030"), # 14
        ymd("20111214"), # 15
        ymd("20130219"), # 16
        ymd("20131030"), # 17
        ymd("20140410"), # 18
        ymd("20150410"), # 19
        ymd("20160410"), # 20
        ymd("20170404"), # 21
        ymd("20180404"), # 22
        ymd("20190404"), # 23
        ymd("20200416"), # 24
        ymd("20200722"), # 25
        ymd("20210722"), # 26
        ymd("20220722"), # 27
        ymd("20230112")  # 28 my 53rd bday
    ),
    approx.yr = c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014.1, 2014.2, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
    term = c(2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 6, 7, 7, 8, 9, 10, 11, 12, 12, 12, 13, 13, 13, 14, 15, 15, 15)
)

# term-by-term members
term.members <- data.frame(
    term = 1:15,
    period = c(1:9,"a","b","c","d","e","f"))
#
tmp <- rbind(
c("segob", "creel", "granados", "pinchetti", "pozas", "woldenberg", "zertuche", "senpri", "senprd", "dippri", "dippan"),
c("woldenberg", "barragan", "cantu", "cardenas", "lujambio", "merino", "molinar", "peschard", "zebadua", NA, NA),
c("woldenberg", "barragan", "cantu", "cardenas", "lujambio", "luken", "merino", "peschard", "rivera", NA, NA),
c("ugalde", "albo", "alcantar", "andrade", "glezluna", "latapi", "lopezflores", "morales", "sanchez", NA, NA),
c("albo", "alcantar", "andrade", "glezluna", "latapi", "lopezflores", "morales", "sanchez", NA, NA, NA),
c("valdes", "albo", "alcantar", "andrade", "banos", "glezluna", "lopezflores", "nacif", "sanchez", NA, NA),
c("valdes", "alcantar", "andrade", "banos", "elizondo", "figueroa", "guerrero", "nacif", "sanchez", NA, NA),
c("valdes", "banos", "elizondo", "figueroa", "guerrero", "nacif", NA, NA, NA, NA, NA),
c("valdes", "banos", "cordova", "elizondo", "figueroa", "garcia", "guerrero", "marvan", "nacif", NA, NA),
c("valdes", "banos", "cordova", "elizondo", "figueroa", "guerrero", "marvan", "nacif", NA, NA, NA),
c("cordova", "banos", "marvan", "nacif", NA, NA, NA, NA, NA, NA, NA),
c("cordova", "andrade2", "banos", "favela", "galindo", "murayama", "nacif", "ruiz", "sanchez", "santiago", "snmartin"),
c("cordova", "andrade2", "banos", "favela", "murayama", "nacif", "ravel", "rivera2", "ruiz", "snmartin", "zavala"),
c("cordova", "favela", "murayama", "ravel", "rivera2", "ruiz", "zavala", NA, NA, NA, NA),
c("cordova", "favela", "murayama", "faz", "humphrey", "kib", "magana", "ravel", "rivera2", "ruiz", "zavala")
)
colnames(tmp) <- paste0("m", 1:11)
term.members <- cbind(term.members, tmp)
# inspect
tmp <- term.members[4, grep("^m[0-9]", colnames(term.members))]
tmp[!is.na(tmp)]

###################
## member priors ##
###################
mem.priors <- data.frame(rbind(
    c("woldenberg" ,  "center"),
    c("barragan"   ,  "center"),
    c("cantu"      ,  "center"),
    c("cardenas"   ,  "center"),
    c("lujambio"   ,  "center"),
    c("merino"     ,  "center"),
    c("molinar"    ,  "center"),
    c("peschard"   ,  "center"),
    c("zebadua"    ,  "center"),
    c("luken"      ,  "center"),
    c("rivera"     ,  "center"),
    c("ugalde"     ,  "center"),
    c("albo"       ,  "center"),
    c("andrade"    ,  "center"),
    c("alcantar"   ,            "right"),
    c("glezluna"   ,  "center"),
    c("latapi"     ,  "center"),
    c("lopezflores",  "center"),
    c("morales"    ,  "center"),
    c("sanchez"    ,            "left"),
    c("valdes"     ,  "center"),
    c("banos"      ,  "center"),
    c("nacif"      ,  "center"),
    c("elizondo"   ,  "center"),
    c("figueroa"   ,  "center"),
    c("guerrero"   ,  "center"),
    c("cordova"    ,  "center"),
    c("garcia"     ,  "center"),
    c("marvan"     ,  "center"),
    c("andrade2"   ,  "center"),
    c("favela"     ,  "center"),
    c("galindo"    ,  "center"),
    c("murayama"   ,  "center"),
    c("ruiz"       ,  "center"),
    c("snmartin"   ,  "center"),
    c("santiago"   ,  "center"),
    c("ravel"      ,  "center"),
    c("rivera2"    ,  "center"),
    c("zavala"     ,  "center"),
    c("cruz"       ,  "center"),
    c("faz"        ,  "center"),
    c("humphrey"   ,  "center"),
    c("kib"        ,  "center")
))
colnames(mem.priors) <- c("name", "location")
rownames(mem.priors) <- mem.priors$name

##################################
## will receive point estimates ##
##################################
options(width=70)
ls()
head(mem.priors)
thetas  <- mem.priors # duplicate
tmp <- matrix(NA, ncol = 28, nrow = nrow(thetas))
tmp <- as.data.frame(tmp)
colnames(tmp) <- paste0("n", 1:28)
thetas <- cbind(tmp, thetas)
head(thetas)

##############################################
## Read votes, exported by code/data-prep.r ##
##############################################
vot <-read.csv("v456789ab.csv",  header=TRUE)

# add approx yr-by-yr
tmp <- vot$date # extract dates
tmp2 <- tmp # duplicate
tmp3 <- tmp # triplicate
for (i in 1:length(tmp)){
    sel <- which(yr.by.yr$start<=tmp[i] & yr.by.yr$end>=tmp[i])
    tmp2[i] <- yr.by.yr$approx.yr[sel] # returns yr vote belongs to
    tmp3[i] <- yr.by.yr$n[sel]         # returns n vote belongs to
}
vot$ye <- as.numeric(tmp2)
vot$n <-  as.numeric(tmp3)
# explore
table(vot$dunan, vot$ye)
table(vot$term, vot$ye)

############################
## Drop uncontested votes ##
############################
sel <- which(vot$dunan==1)
vot <- vot[-sel,]

##################################
## year-by-years to be analyzed ##
##################################
table(yr.by.yr$term, yr.by.yr$n) # inspect
enes <- 8:18 # years 8:18 cover terms 4:11, ug to 2014 reform

#####################################
## pick one year (make loop later) ##
#####################################
n <- enes[1]
# determine members
sel <- yr.by.yr[yr.by.yr$n==n,]$term
mems <- term.members[sel, grep("^m[0-9]", colnames(term.members))]
mems <- mems[!is.na(mems)]

###########################################
## subset votes to year n and it members ##
###########################################
rc <- vot[vot$n==n, mems]
dim(rc)

##########################
## Rosas, vectorization ##
##########################
rc <- as.data.frame(t(rc))
mem.index  <- 1:nrow(rc)
vote.index <- 1:ncol(rc)

## Melt RC (turn it into long format, see https://seananderson.ca/2013/10/19/reshape/)
rc.2 <- as.data.frame (rc)
colnames (rc.2) <- vote.index
rc.2$mem <- mem.index
molten.rc <- reshape2::melt(rc.2, id.vars="mem", variable.name="vote", value.name="rc")
molten.rc$rc <- car::recode (molten.rc$rc, "0=NA")
molten.rc <- na.omit (molten.rc)
molten.rc$rc <- car::recode (molten.rc$rc, "2=0; c(3,4,5)=NA")

ife.data.vector <- dump.format(list(y=molten.rc$rc
                                    , n.mems = max(mem.index)
                                    , n.item = max(vote.index)
                                    , n.obs  = nrow(molten.rc)
                                    , vote   = molten.rc$vote
                                    , dep    = molten.rc$mem
))

ife.parameters = c("theta", "alpha", "beta", "deviance")

ife.inits <- function() {
  dump.format(
    list(
      theta = c(rnorm(2), NA, rnorm(5), NA)
      , alpha = rnorm(max(vote.index))
      , beta  = rnorm(max(vote.index))
      ,'.RNG.name'="base::Wichmann-Hill"
      ,'.RNG.seed'= 1971)   #randomNumbers(n = 1, min = 1, max = 1e+04,col=1))
  )
}

# inspect spike priors to code ife.vector
mem.priors[mems,]

ife.vector = "model {
	for (i in 1:n.obs) {
		y[i] ~ dbern (pi[i])
		probit(pi[i]) <- beta[vote[i]]*theta[dep[i]] - alpha[vote[i]]
	}
# PRIORS
# Alpha (difficulty)
for (j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25) }   
# Beta (discrimination)
for (j in 1:n.item) { beta[j] ~ dnorm(0, 0.1) }   
# ideal points
theta[3] ~ dnorm( 1,4)T(0,) # normal + truncated
theta[9] ~ dnorm(-1,4)T(,0) # normal - truncated
for(i in c(1,2,4:8))  { theta[i] ~ dnorm(0,1) }
}"

ife.model.v <- run.jags(
  model    = ife.vector,
  monitor  = ife.parameters,
  method   = "parallel",
  n.chains = 2,
  data     = ife.data.vector,
  inits    = list (ife.inits(), ife.inits()),
  thin = 50, burnin = 10000, sample = 200,
  #thin = 5, burnin = 200, sample = 200,
  check.conv = FALSE, plots = FALSE)

chainsIFE.v <- mcmc.list(list (ife.model.v$mcmc[[1]], ife.model.v$mcmc[[2]]))
gelman.diag (chainsIFE.v, multivariate=F) # convergence looks fine for both models

############################
## store posterior sample ##
############################
post.samples <- vector("list", length(1:28))
names(post.samples) <- paste0("m", 1:28)
post.samples[[n]] <- chainsIFE.v
summary(post.samples)

Alpha.v <- rbind ( chainsIFE.v[[1]][,grep("alpha", colnames(chainsIFE.v[[1]]))]
                   , chainsIFE.v[[2]][,grep("alpha", colnames(chainsIFE.v[[2]]))])
Beta.v <- rbind ( chainsIFE.v[[1]][,grep("beta", colnames(chainsIFE.v[[1]]))]
                  , chainsIFE.v[[2]][,grep("beta", colnames(chainsIFE.v[[2]]))])
Theta.v <- rbind ( chainsIFE.v[[1]][,grep("theta", colnames(chainsIFE.v[[1]]))]
                   , chainsIFE.v[[2]][,grep("theta", colnames(chainsIFE.v[[2]]))])

plot (colMeans (Alpha.v))  # difficulties
plot (colMeans (Beta.v))   # signal


plot(enes, c(rep(min(Theta.v), length(enes)-1), max(Theta.v)), type = "n", xlab="Year", axes = FALSE)
axis(1, at=enes, labels=yr.by.yr$approx.yr[enes])
axis(2)
plot(rep(n, length(mem.index)), colMeans (Theta.v))


par (las=2, mar=c(7,3,2,2))
plot (1:nlength(vot$date), colMeans (Beta.v)
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


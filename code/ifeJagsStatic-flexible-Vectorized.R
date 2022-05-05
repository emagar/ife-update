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
ids <- ids[-grep("^1$", ids$tenure),]    # drop consejeros ciudadanos 1994-96
#
ids$party <- 0
ids$party[grep   ("pri", ids$sponsor)] <- 1
ids$party[grep   ("pan", ids$sponsor)] <- 2
ids$party[grep   ("prd", ids$sponsor)] <- 3
ids$party[grep  ("pvem", ids$sponsor)] <- 4
ids$party[grep("morena", ids$sponsor)] <- 5
#
ids <- within(ids, color <- ifelse(party== 1,        "red",
                            ifelse(party== 2,       "blue",
                            ifelse(party== 3,       "gold",
                            ifelse(party== 4,      "green",
                            ifelse(party== 5, "orangered4", "gray"))))))
ids[12,]

#####################################################################################
## adjusts approximate years with constant membership for year-by-year estimations ##
#####################################################################################
yr.by.yr <- data.frame(
    p = 1:28, 
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
    term =   c(2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 6, 7, 7, 8, 9, 10, 11, 12, 12, 12, 13, 13, 13, 14, 15, 15, 15),
    term.a = c(2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 6, 7, 7, 8, 9, "a","b","c","c","c","d","d","d","e","f","f","f")
)

###############################
## select terms for analysis ##
###############################
tees <- 4:11
tees.grep <- "[456789ab]"

######################################################
## subset ids and periodocization to relevant lines ##
######################################################
ids <- ids[grep(pattern = tees.grep, ids$tenure),]
yr.by.yr <- yr.by.yr[grep(pattern = tees.grep, yr.by.yr$term.a),]
ids[, c("column","sponsor","tenure")] # inspect

######################################
## discrete periods to be analyzed  ##
## e.g. year-by-year                ##
######################################
table(term=yr.by.yr$term, p=yr.by.yr$p) # inspect
pees <- yr.by.yr$p # years 8:18 cover terms 4:11, ug to 2014 reform
P <- length(pees)

##########################################
## Prep object to receive yearly priors ##
##########################################
prior.location <- data.frame(matrix(NA, nrow = length(ids$column), ncol = P))
rownames(prior.location) <- ids$column
colnames(prior.location) <- paste0("p", pees)
prior.precision <- prior.location
prior.precision[] <- 100 # for N=2-on
# assign priors for first round
prior.location[,1] <- 0
prior.location["alcantar",1] <-  2
prior.location ["sanchez",1] <- -2
prior.precision[,1] <- 1
prior.precision["alcantar",1] <- 4
prior.precision ["sanchez",1] <- 4

## # term-by-term members
## term.members <- data.frame(
##     term = 1:15,
##     term.a = c(1:9,"a","b","c","d","e","f"))
## #
## tmp <- rbind(
## c("segob", "creel", "granados", "pinchetti", "pozas", "woldenberg", "zertuche", "senpri", "senprd", "dippri", "dippan"),
## c("woldenberg", "barragan", "cantu", "cardenas", "lujambio", "merino", "molinar", "peschard", "zebadua", NA, NA),
## c("woldenberg", "barragan", "cantu", "cardenas", "lujambio", "luken", "merino", "peschard", "rivera", NA, NA),
## c("ugalde", "albo", "alcantar", "andrade", "glezluna", "latapi", "lopezflores", "morales", "sanchez", NA, NA),
## c("albo", "alcantar", "andrade", "glezluna", "latapi", "lopezflores", "morales", "sanchez", NA, NA, NA),
## c("valdes", "albo", "alcantar", "andrade", "banos", "glezluna", "lopezflores", "nacif", "sanchez", NA, NA),
## c("valdes", "alcantar", "andrade", "banos", "elizondo", "figueroa", "guerrero", "nacif", "sanchez", NA, NA),
## c("valdes", "banos", "elizondo", "figueroa", "guerrero", "nacif", NA, NA, NA, NA, NA),
## c("valdes", "banos", "cordova", "elizondo", "figueroa", "garcia", "guerrero", "marvan", "nacif", NA, NA),
## c("valdes", "banos", "cordova", "elizondo", "figueroa", "guerrero", "marvan", "nacif", NA, NA, NA),
## c("cordova", "banos", "marvan", "nacif", NA, NA, NA, NA, NA, NA, NA),
## c("cordova", "andrade2", "banos", "favela", "galindo", "murayama", "nacif", "ruiz", "sanchez", "santiago", "snmartin"),
## c("cordova", "andrade2", "banos", "favela", "murayama", "nacif", "ravel", "rivera2", "ruiz", "snmartin", "zavala"),
## c("cordova", "favela", "murayama", "ravel", "rivera2", "ruiz", "zavala", NA, NA, NA, NA),
## c("cordova", "favela", "murayama", "faz", "humphrey", "kib", "magana", "ravel", "rivera2", "ruiz", "zavala")
## )
## colnames(tmp) <- paste0("m", 1:11)
## term.members <- cbind(term.members, tmp)
## # inspect
## tmp <- term.members[4, grep("^m[0-9]", colnames(term.members))]
## tmp[!is.na(tmp)]

##############################################
## Read votes, exported by code/data-prep.r ##
##############################################
vot <-read.csv("v456789ab.csv",  header=TRUE)

##################################
## add temporal periodicization ##
##################################
tmp <- vot$date # extract dates
tmp2 <- tmp3 <- tmp # triplicate
for (i in 1:length(tmp)){
    sel <- which(yr.by.yr$start<=tmp[i] & yr.by.yr$end>=tmp[i])
    tmp2[i] <- yr.by.yr$approx.yr[sel] # returns yr vote belongs to
    tmp3[i] <- yr.by.yr$p[sel]         # returns p vote belongs to
}
vot$yr <- as.numeric(tmp2)
vot$p  <- as.numeric(tmp3)
# explore
table(dunan=vot$dunan, yr=vot$yr)
with(vot, table(term=term[dunan==0], yr=yr[dunan==0]))

###########################################
## summarize then drop uncontested votes ##
###########################################
table(factor(vot$dunan, labels = c("contested","not")), vot$term, useNA = "ifany")
table(factor(vot$dunan, labels = c("contested","not")), useNA = "ifany")
sel <- which(vot$dunan==1)
if (length(sel)>0) vot <- vot[-sel,]

#######################################################
## ################################################# ##
## ## THIS IS DONE BELOW, IN MEMO'S VECTORIZATION ## ##
## ################################################# ##
#######################################################
## #########################################################################
## ## summarize and recode vote values --- probit requires 0s and 1s only ##
## #########################################################################
## tmp <- vot[,column]
## table(vote=as.matrix(tmp), useNA = "always")
## # recode
## tmp[tmp==0] <- NA    ## 1s are ayes, 0s nays
## tmp[tmp>2]  <- NA
## tmp[tmp==2] <- 0
## table(vote=as.matrix(tmp), useNA = "always")
## # return to vote object
## vot[,column] <- tmp

##################################################
## will receive point estimates and 80pct bands ##
##################################################
point <- prior.location
point[,1] <- NA
lo <- hi <- point

####################################
## will receive posterior samples ##
####################################
post.samples <- vector("list", P)
names(post.samples) <- paste0("p", pees)
rm(i,sel,tmp,tmp2,tmp3) # clean

##########################################
## pick one year (turn into loop later) ##
##########################################
p <- c(1:P)[1]

#########################################
## determine members and their parties ##
#########################################
sel <- yr.by.yr[yr.by.yr$p==pees[p],]$term.a
sel    <- grep(pattern = sel, ids$tenure) # ugalde and valdés councils
party.p  <- ids$party [sel]
column.p <- ids$column[sel]

#############################################
## subset votes to period p and it members ##
#############################################
rc <- vot[vot$p==pees[p], column.p]
dim(rc)
#head(rc)

##########################
## Rosas, vectorization ##
##########################
rc <- as.data.frame(t(rc))
M <- nrow(rc) # total members in period
V <- ncol(rc) # total votes in period
map.vote.indices   <- data.frame(actual=as.numeric(colnames(rc)),
                                 sequential=1:V)
map.member.indices <- data.frame(actual=rownames(rc),
                                 sequential=1:M)
map.period.indices <- data.frame(actual=yr.by.yr$p,
                                 yr=yr.by.yr$approx.yr,
                                 sequential=1:P)
## mem.index  <- 1:M
## vote.index <- 1:V

#####################################################
## Melt RC (turn it into long format)              ##
## see https://seananderson.ca/2013/10/19/reshape/ ##
#####################################################
rc.2 <- as.data.frame (rc)
colnames (rc.2) <- 1:V
rc.2$member <- 1:M
molten.rc <- reshape2::melt(rc.2, id.vars="member", variable.name="vote", value.name="rc")
#head(molten.rc, 10)
molten.rc$rc <- car::recode (molten.rc$rc, "0=NA") # non-members' slots, if any, to NA
# will try w/o 4may2022 #molten.rc <- na.omit (molten.rc)                   # drops non-members' slots, if any
molten.rc$rc <- car::recode (molten.rc$rc, "2=0; c(3,4,5)=NA") # abstain|absent to NA

ife.data.vector <- dump.format(list(y = molten.rc$rc, 
#                                    n.mems = M,
                                    n.item = V,
                                    n.obs  = nrow(molten.rc),
                                    vote   = molten.rc$vote,
                                    member = molten.rc$member
))

ife.parameters = c("theta", "alpha", "beta", "deviance")

ife.inits <- function() {
  dump.format(
    list(
      theta = c(rnorm(3), NA, rnorm(4), NA)
      , alpha = rnorm(V)
      , beta  = rnorm(V)
      ,'.RNG.name'="base::Wichmann-Hill"
      ,'.RNG.seed'= 1971)   #randomNumbers(n = 1, min = 1, max = 1e+04,col=1))
  )
}

# inspect spike priors to code ife.vector
prior.location[,p]

ife.model = "model {
	for (i in 1:n.obs) {
		y[i] ~ dbern (pi[i])
		probit(pi[i]) <- beta[vote[i]]*theta[member[i]] - alpha[vote[i]]
	}
	# PRIORS
	# Alpha (difficulty)
	for (j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25) }   
	# Beta (discrimination)
	for (j in 1:n.item) { beta[j] ~ dnorm(0, 0.1) }   
	# ideal points
	theta[4] ~ dnorm( 2,4)T(0,) # normal + truncated
	theta[9] ~ dnorm(-2,4)T(,0) # normal - truncated
	for(i in c(1:3,5:8))  { theta[i] ~ dnorm(0,1) }
	#for(i in setdiff(1:n.mems, c(4,9)))  { theta[i] ~ dnorm(0,1) } # no parece gustarle a jags
}"

results <- run.jags(
  model    = ife.model,
  monitor  = ife.parameters,
  method   = "parallel",
  n.chains = 2,
  data     = ife.data.vector,
  inits    = list (ife.inits(), ife.inits()),
  thin = 50, burnin = 10000, sample = 200,
  #thin = 5, burnin = 200, sample = 200,
  check.conv = FALSE, plots = FALSE)

chains <- mcmc.list(list (results$mcmc[[1]], results$mcmc[[2]]))
# check model convergence 
gelman.diag (chains, multivariate=F)

############################
## store posterior sample ##
############################
post.samples[[p]] <- chains
#summary(post.samples)

################################################################
## store point estimates etc and update priors for next round ##
################################################################
thetas <- rbind ( chains[[1]][,grep("theta", colnames(chains[[1]]))] ,
                  chains[[2]][,grep("theta", colnames(chains[[2]]))] )
point[map.member.indices$actual,p] <- round(colMeans(thetas),3)
lo[map.member.indices$actual,p] <- apply(X=thetas, 2, FUN = function(X) quantile(X, probs = .1))
hi[map.member.indices$actual,p] <- apply(X=thetas, 2, FUN = function(X) quantile(X, probs = .9))
#
prior.location[,(p+1)] <- 0 # start with zeroes
prior.location[map.member.indices$actual,(p+1)] <- point[map.member.indices$actual,p]

##################
## plot results ##
##################
Alpha.v <- rbind ( chains[[1]][,grep("alpha", colnames(chains[[1]]))] ,
                   chains[[2]][,grep("alpha", colnames(chains[[2]]))] )
Beta.v  <- rbind ( chains[[1]][,grep("beta",  colnames(chains[[1]]))] ,
                   chains[[2]][,grep("beta",  colnames(chains[[2]]))] )
Theta.v <- rbind ( chains[[1]][,grep("theta", colnames(chains[[1]]))] ,
                   chains[[2]][,grep("theta", colnames(chains[[2]]))] )

plot (colMeans (Alpha.v))  # difficulties
plot (colMeans (Beta.v))   # signal


plot(1:P, c(rep(min(Theta.v), length(pees)-1), max(Theta.v)), type = "n", xlab="Year", axes = FALSE)
axis(1, at=1:P, labels=yr.by.yr$approx.yr[1:P])
axis(2)
plot(rep(p, M), colMeans (Theta.v))


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


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
library(plyr)

rm(list = ls())
workdir <- c("/home/eric/Dropbox/data/rollcall/ife_cg/ife-update/data/")
setwd(workdir)

# Define colors and plotting names
# OJO: en tenure term==10 es a, term==11 es b etc. 
ids <- read.csv("../ancillary/consejo-general-ife-ine.csv")
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
rownames(ids) <- ids$column
ids[12,]

#####################################################################################
## adjusts approximate years with constant membership for year-by-year estimations ##
#####################################################################################
yr.by.yr <- data.frame(
    yrn = 1:28, 
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

################################################################
## select temporal range of full analysis (broken down below) ##
################################################################
## terms <- 4:11
## terms.grep <- "[456789ab]"
terms <- 4:9
terms.grep <- "[456789]"

#############################################
## subset ids and periodicization to range ##
#############################################
ids <- ids[grep(pattern = terms.grep, ids$tenure),]
yr.by.yr <- yr.by.yr[grep(pattern = terms.grep, yr.by.yr$term.a),]
ids[, c("column","sponsor","tenure")] # inspect

#############################################################
## select temporal unit -- discrete periods to be analyzed ##
## e.g. terms, year-by-year, semester...                   ##
#############################################################
table(term=yr.by.yr$term, yrn=yr.by.yr$yrn) # inspect
#tees <- yr.by.yr$yrn # years 8:18 cover terms 4:11, ug to 2014 reform
tees <- c(4,6:9) # terms ugalde valdés I II and III
T <- length(tees)

##########################################
## Prep object to receive yearly priors ##
##########################################
prior.location <- data.frame(matrix(NA, nrow = length(ids$column), ncol = T))
rownames(prior.location) <- ids$column
colnames(prior.location) <- paste0("term", tees)
prior.precision <- prior.location
prior.precision[] <- 1 # for N=2-on
## # assign priors for first round only
## prior.location[,1] <- 0
## prior.location["alcantar",1] <-  2
## prior.location ["sanchez",1] <- -2
## prior.precision[,1] <- 1
## prior.precision["alcantar",1] <- 4
## prior.precision ["sanchez",1] <- 4
# assign priors for first round in council
prior.location[] <- 0
prior.location["alcantar",] <-  2
prior.location ["sanchez",] <- -2
prior.location["banos",] <-  2
prior.location["figueroa",] <- -2
prior.precision[,1] <- 1
prior.precision["alcantar",] <- 4
prior.precision ["sanchez",] <- 4
prior.precision ["banos",] <- 4
prior.precision ["figueroa",] <- 4

prior.location  # inspect
prior.precision # inspect

##############################
## will receive party means ##
##############################
party.locations <- data.frame(matrix(NA, ncol = T, nrow = 5))
colnames(party.locations) <- colnames(prior.location)
rownames(party.locations) <- c("pri", "pan", "prd", "pvem", "morena")

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
vot.raw <-read.csv("v456789ab.csv",  header=TRUE)
vot <- vot.raw # duplicate for manipulation

#########################################################
## term 5 has one contested vote only, merge to term 4 ##
#########################################################
sel <- which(vot$term==5)
vot$term[sel] <- 4

###########################################
## using terms: subset to desired terms  ##
###########################################
sel <- which(vot$term %in% tees)
vot <- vot[sel,]
# add temporal aggregation as column t
vot$t <- NA
for (i in 1:T){
    sel <- which(vot$term==tees[i])
    vot$t[sel] <- i
}
# explore
table(dunan=vot$dunan, t=vot$t)

## ####################################################
## ## using approx.yrs: add temporal periodicization ##
## ####################################################
## # approx yr version
## tmp <- vot$date # extract dates
## tmp2 <- tmp3 <- tmp # triplicate
## for (i in 1:length(tmp)){
##     sel <- which(yr.by.yr$start<=tmp[i] & yr.by.yr$end>=tmp[i])
##     tmp2[i] <- yr.by.yr$approx.yr[sel] # returns yr vote belongs to
##     tmp3[i] <- yr.by.yr$yrn[sel]         # returns t vote belongs to
## }
## vot$yrn <- as.numeric(tmp2)
## vot$t  <- as.numeric(tmp3)
## # explore
## table(dunan=vot$dunan, yr=vot$yrn)
## with(vot, table(term=term[dunan==0], yr=yr[dunan==0]))

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

####################################################################################################
## identify item anchors and place them as votes 1 (north) and 2 (south)                          ##
## if necessary, recode ayes/nays so that aye points to desired side                              ##
## ** term==4 **                                                                                  ##
## - folio 2401 Agenda power for President (PRI-sponsored): should candidate for top-level        ##
## appointment, proposed by Council President without relevant commission's consent, be           ##
## ratified? (Minority=pan minus Morales, Latapí, nay) Aye=right                                  ##
## - folio 2479 Scope of IFE authority: must PVEM statutes make party leaders accountable to      ##
## rank-and-file? (Andrade, Lpz Flores, Morales, Gmz Alcántar, nay) Aye=left                      ##
## ** term==6 **                                                                                  ##
## - folio 3641 Designación Director Ejecutivo Serv Prof Elect (Minority pan minus Nacif, nay)    ##
## Aye=right                                                                                      ##
## - folio 3924 Fine PRD and coalition partners for a negative campaign ad against the PRI in     ##
## Baja California (Minority pan minus Albo, nay) Aye=right                                       ##
## ** term==7 **                                                                                  ##
## - folio 6127 Penalty to PVEM federal deputies for TV advertisement promoting the death penalty ##
## (Minority PRI, PVEM, nay) Aye=left, needs justification (other than free speech?)              ##
## - folio 6174 Drop libel case against PAN for sopa de letras newspaper negative ad against the  ##
## PRI (Minority PRI, PVEM, nay) Aye=left                                                         ##
## ** term==8 **                                                                                  ##
## - folio 7421 Penalty to Enrique Peña (Minority pri), aye=left                                  ##
## - folio 7633 PRD-sponsored penalty to governor of state of Guerrero (Minority prd), aye=right  ## 
## ** term==9 **                                                                                  ##
## - folio 8317 Break PRI's denuncia against PAN in two, fine and FCH's direct responsibility     ##
## (Minority PRI with Córdova) Aye=left                                                           ##
## - folio 8814 Aristegui's vs PRD-PT with engrose (Minority PRD plus Nacif) Aye=left             ##
####################################################################################################
anchors <- which(vot$folio %in% c(2401,2479,  # term==4:5
                                  3641,3924,  # term==6
                                  6127,6174,  # term==7
                                  7421,7633,  # term==8
                                  8317,8814)) # term==9
# some votes need aye/nay reversal to point to correct direction
sel <- which(vot$folio[anchors] %in% c(3924, 6127, 7421, 7633, 8317))
tmp <- vot[anchors[sel], ids$column] # subset votes that must chg
for (i in 1:nrow(tmp)){
    tmp2 <- as.character(tmp[i,]) # extract vector, to character
    tmp2 <- mapvalues(tmp2, from = c(1,2), to = c(2,1))
    tmp[i,] <- as.numeric(tmp2)
}
vot[anchors[sel], ids$column] <- tmp
rm(tmp,tmp2)

#######################################################################
## move anchor votes to start of time series, so that they appear as ##
## votes 1 and 2 when term is subsetted                              ##
#######################################################################
vot <- rbind(vot[anchors,], vot[-anchors,])
#head(vot)
rm(anchors) # indices no longer valid

## ###########################################################
## ## use votes in 1st half of term==7 only for exploration ##
## ###########################################################
## sel <- which(vot$date > ymd("2009-06-30"))
## vot <- vot[-sel,]

####################################
## will receive posterior samples ##
####################################
post.samples <- vector("list", T)
names(post.samples) <- paste0("term", tees)
rm(i,sel) # clean

##########################################
## pick one year (turn into loop later) ##
##########################################
t <- c(1:T)[2]
#for (t in 1:7){

###################################################
## by terms: determine members and their parties ##
###################################################
sel <- c("4","6","7","8","9","a","b")
sel <- sel[t]
sel    <- grep(pattern = sel, ids$tenure)
party.t  <- ids$party [sel]
column.t <- ids$column[sel]

## ####################################################
## ## yr.approx: determine members and their parties ##
## ####################################################
## sel <- yr.by.yr[yr.by.yr$term==tees[t],]$term.a
## sel    <- grep(pattern = sel, ids$tenure) # ugalde and valdés councils
## party.t  <- ids$party [sel]
## column.t <- ids$column[sel]

##############################################
## subset votes to period t and its members ##
##############################################
rc <- vot[vot$t==t, column.t]
dim(rc)
## head(vot[vot$t==1,])
## head(rc)
x
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
map.time.indices <- data.frame(actual=tees,
                               #yr=yr.by.yr$approx.yr,
                               sequential=1:T)

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

ife.data.vector <-
    dump.format(list(y = molten.rc$rc, 
                     n.member = M,
                     n.item = V,
                     n.obs  = nrow(molten.rc),
#                     mean.theta = prior.location[map.member.indices$actual ,t],
#                     precision.theta = prior.precision[map.member.indices$actual ,t],
                     vote   = molten.rc$vote,
                     member = molten.rc$member
                     ))

ife.parameters = c("theta", "alpha", "beta", "deviance")

ife.inits <- function() {
  dump.format(
    list(
#      theta = c(rnorm(3), NA, rnorm(4), NA)
      theta   = rnorm(M)
      , alpha = rnorm(V)
#      , beta  = rnorm(V)
      , beta  = c(NA, NA, rnorm(V-2))
      ,'.RNG.name'="base::Wichmann-Hill"
      ,'.RNG.seed'= 1971)   #randomNumbers(n = 1, min = 1, max = 1e+04,col=1))
  )
}

## # inspect spike priors to code ife.vector
## point.est
## prior.location[map.member.indices$actual,t]
## prior.precision[map.member.indices$actual,t]

ife.model.members = "model {
	for (n in 1:n.obs) {
		y[n] ~ dbern (pi[n])
		probit(pi[n]) <- beta[vote[n]]*theta[member[n]] - alpha[vote[n]]
	}
	# PRIORS
	# Alpha (difficulty)
	for (j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25) }   
	# Beta (discrimination)
	for (j in 1:n.item) { beta[j] ~ dnorm(0, 0.1) }   
	# ideal points
	for(i in 1:n.member)  { theta[i] ~ dnorm( mean.theta[i], precision.theta[i] ) }
	## theta[4] ~ dnorm( 2,4)T(0,) # normal + truncated
	## theta[9] ~ dnorm(-2,4)T(,0) # normal - truncated
	## for(i in c(1:3,5:8))  { theta[i] ~ dnorm(0,1) }
	#for(i in setdiff(1:n.mems, c(4,9)))  { theta[i] ~ dnorm(0,1) } # no parece gustarle a jags
}"

ife.model.items = "model {
	for (n in 1:n.obs) {
		y[n] ~ dbern (pi[n])
		probit(pi[n]) <- beta[vote[n]]*theta[member[n]] - alpha[vote[n]]
	}
	# PRIORS
	# Alpha (difficulty)
	for (j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25) }   
	# Beta (discrimination) --- votes sorted such that 1-2 are north-south, rest uninformative
        for(j in 3:n.item){ 
            beta [j] ~ dnorm(0, 0.1)
        }
        beta [1] ~ dnorm( 4, 4)
        beta [2] ~ dnorm(-4, 4)
	# ideal points
	for(i in 1:n.member) { theta[i] ~ dnorm(0,1) }
}"

results <- run.jags(
  model    = ife.model.items,
  monitor  = ife.parameters,
  method   = "parallel",
  n.chains = 2,
  data     = ife.data.vector,
  inits    = list (ife.inits(), ife.inits()),
  thin = 250, burnin = 50000, sample = 200,
  #thin = 50, burnin = 10000, sample = 200,
  #thin = 5, burnin = 200, sample = 200,
  plots = FALSE)

chains <- mcmc.list(list (results$mcmc[[1]], results$mcmc[[2]]))
# check model convergence 
gelman.diag (chains, multivariate=F)

############################
## store posterior sample ##
############################
load("posterior-samples/theta-chains-statics-45-6-7-items.RData")
post.samples[[t]] <- list(map.vote.indices=map.vote.indices,
                          map.member.indices=map.member.indices,
                          map.time.indices=map.time.indices,
                          chains=chains)
summary(post.samples)
save(post.samples, file = "tmp.RData")

##################################################
## will receive point estimates and 80pct bands ##
##################################################
point.est <- prior.location
point.est[] <- NA
lo <- hi <- point.est

################################################
## store point estimates and confidence bands ##
################################################
indices <- function(x) return(which(rownames(point.est)==map.member.indices$actual[x])) # get target rows function
#indices <- unlist(lapply(1:M, FUN = indices)) # all at once
for (t in 1:T){
    #t <- 1
    #summary(post.samples[[i]])
    map.member.indices <- post.samples[[t]]$map.member.indices
    chains <- post.samples[[t]]$chains
    thetas <- rbind ( chains[[1]][,grep("theta", colnames(chains[[1]]))] ,
                      chains[[2]][,grep("theta", colnames(chains[[2]]))] )
    for (i in 1:nrow(map.member.indices)){
        #i <- 1
        sel.r <- indices(i)
        point.est[sel.r, t] <- round(colMeans(thetas)[i], 3)
        lo       [sel.r, t] <- round(quantile(thetas[,i], probs = .1),3)
        hi       [sel.r, t] <- round(quantile(thetas[,i], probs = .9),3)
    }
}

point.est <- -point.est

pdf(file = "../plots/statics-terms-45-6-7-item.pdf")
plot(c(.25,T+.75), c(min(point.est, na.rm = TRUE), max(point.est, na.rm = TRUE)), type="n", xlab = "term", ylab = "ideal point", axes = FALSE,
     main = "Static estims by term, item anchors") 
#axis(1, at = 1:T, labels = c("4-5", "6", "7"))
axis(1, at = 1:T, labels = c("Ugalde-Albo\n2003-08", "Valdés I", "Valdés II"))
axis(2)
for (t in 1:T){
    sel <- c("4","6","7","8","9","a","b"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t  <- ids$party [sel]
    column.t <- ids$column[sel]
    color.t <- ids$color[sel]
    tmp <- point.est[column.t, t]
    points(x = rep(t, length(tmp)),
           y = tmp,
           col = color.t)
}
for (i in 1:nrow(point.est)){
    lines(x = 1:T,
          y = point.est[i,],
          col = ids$color[i])
}
sel.r <- ids$column[which(!is.na(point.est[,1]))]
text(x = 1,
     y = point.est[sel.r, 1],
     labels = ids[sel.r, "short"],
     pos = 2 )
sel.r <- ids$column[which(!is.na(point.est[,3]))]
text(x = 3,
     y = point.est[sel.r, 3],
     labels = ids[sel.r, "short"],
     pos = 4 )
dev.off()


#########################
## update party means  ##
#########################
party.locations[,t] <- 0 # start with zeroes
for (p in 1:5){
    tmp <- round(mean(point.est[ids$party==p,t], na.rm = TRUE),3)
    if (!is.na(tmp)) party.locations[p,t] <- tmp
}
rm(p)
#party.locations # inspect

##################################
## update priors for next round ##
##################################
## # start with party means
## for (p in 1:5){
##     prior.location[which(ids$party==p), (t+1)] <- party.locations[p,t]
## }
## rm(p)
# 
## prior.location[map.member.indices$actual,(t+1)] <- point.est[map.member.indices$actual,t]
## prior.precision[map.member.indices$actual,(t+1)] <- 100 # change precision to 100 for non-new members
}

map.member.indices
point.est # inspect
party.locations
prior.location
prior.precision

###################
## end loop here ##
###################

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


plot(1:P, c(rep(min(Theta.v), length(tees)-1), max(Theta.v)), type = "n", xlab="Year", axes = FALSE)
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


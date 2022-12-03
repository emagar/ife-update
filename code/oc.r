# -*- coding: utf-8 -*-

#############################################
## Code to produce optimal classifications ##
#############################################

## library(arm)
## library (MCMCpack)
## library (foreign)
## library (car)
## library (gtools)
## #library (parallel)
## library (R2jags)
## library (mcmcplots)
## library (sm)
## library (runjags)
## library (coda)
## library(plyr)
library(lubridate)

options(width = 120)
rm(list = ls())
workdir <- c("/home/eric/Dropbox/data/rollcall/ife_cg/ife-update/data/")
# workdir <- c("~/Dropbox/ife-update/data/")
setwd(workdir)

# Define colors and plotting names
# OJO: en tenure term==10 es a, term==11 es b etc. 
ids <- read.csv("../ancillary/consejo-general-ife-ine.csv")
ids <- ids[, -grep("^x", colnames(ids))] # drop redundant "x" columns
ids <- ids[, -grep("^job", colnames(ids))] # drop jobs
ids <- ids[-grep("^1$", ids$tenure),]    # drop consejeros ciudadanos 1994-96
#
ids$party <- 0
ids$party[grep   ("pri", ids$sponsor)] <- 1
ids$party[grep   ("pan", ids$sponsor)] <- 2
ids$party[grep   ("prd", ids$sponsor)] <- 3
ids$party[grep  ("pvem", ids$sponsor)] <- 4
ids$party[grep("morena", ids$sponsor)] <- 5
#
ids <- within(ids, {
    color <- ifelse(party== 1, rgb(205/255,  0/255,  0/255),      # "red",
             ifelse(party== 2, rgb( 58/255, 95/255,205/255),      # "blue",
             ifelse(party== 3, rgb(205/255,215/255,  0/255),      # "gold",
             ifelse(party== 4, rgb(34/255, 139/255, 34/255),      # "green",
             ifelse(party== 5, rgb(139/255, 37/255,  0/255),      # "orangered4" 
                               rgb(190/255,190/255,190/255)))))); # "gray"
    color50 <- ifelse(party== 1, rgb(205/255,  0/255,  0/255, alpha=.25),     # "red",
               ifelse(party== 2, rgb( 58/255, 95/255,205/255, alpha=.25),     # "blue",
               ifelse(party== 3, rgb(205/255,215/255,  0/255, alpha=.25),     # "gold",
               ifelse(party== 4, rgb( 34/255,139/255, 34/255, alpha=.25),     # "green",
               ifelse(party== 5, rgb(139/255, 37/255,  0/255, alpha=.25),     # "orangered4" 
                                 rgb(190/255,190/255,190/255, alpha=.25)))))) # "gray"
    })
## ids <- within(ids, {
##     color <- ifelse(party== 1, "red",
##              ifelse(party== 2, "blue",
##              ifelse(party== 3, "gold",
##              ifelse(party== 4, "green",
##              ifelse(party== 5, "orangered4", 
##                                "gray")))));
##     color50 <- ifelse(party== 1, "red",
##                ifelse(party== 2, "blue",
##                ifelse(party== 3, "gold",
##                ifelse(party== 4, "green",
##                ifelse(party== 5, "orangered4", 
##                                  "gray")))));
##     })
rownames(ids) <- ids$column
ids[5,]

#####################################################################################
## adjusts approximate years with constant membership for year-by-year estimations ##
#####################################################################################
yr.by.yr <- data.frame(
    yrn = 1:28, 
    start = c(
        ymd("19961031"), #  1 1997
        ymd("19971031"), #  2 1998
        ymd("19981031"), #  3 1999
        ymd("19991031"), #  4 2000
        ymd("20001211"), #  5 2001
        ymd("20011031"), #  6 2002
        ymd("20021031"), #  7 2003
        ymd("20031105"), #  8 2004
        ymd("20041105"), #  9 2005
        ymd("20051105"), # 10 2006
        ymd("20061105"), # 11 2007
        ymd("20080215"), # 12 2008
        ymd("20080821"), # 13 2009
        ymd("20090821"), # 14 2010
        ymd("20101031"), # 15 2011
        ymd("20111215"), # 16 2012
        ymd("20130220"), # 17 2013
        ymd("20131031"), # 18 2014
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
terms <- 12:13
terms.grep <- "[cd]"
## terms <- 4:11
## terms.grep <- "[456789ab]"
## terms <- 4:10
## terms.grep <- "[45679a]"
terms <- 23
terms.grep <- "[23]"

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
## tees <- c(12:13) # post-2014 terms córdoba I and II
## tees <- c(4,6:11) # terms ugalde I+II valdés I II III IV and V+VI
## tees <- c(4,6:10) # terms ugalde I+II valdés I II III IV and V
tees <- c(2:3)
T <- length(tees)

## ##########################################
## ## Prep object to receive yearly priors ##
## ##########################################
## prior.location <- data.frame(matrix(NA, nrow = length(ids$column), ncol = T))
## rownames(prior.location) <- ids$column
## colnames(prior.location) <- paste0("term", tees)
## prior.precision <- prior.location
## prior.precision[] <- 1 # for N=2-on
## #
## ## # assign priors for first round in council term 12
## ## prior.location[] <- 0
## ## prior.location  ["favela",] <-  2
## ## prior.location["murayama",] <- -2
## ## prior.precision[,1] <- 1
## ## prior.precision  ["favela",] <- 4
## ## prior.precision["murayama",] <- 4
## ## #
## ## # assign priors for first round in council terms 4 to 11
## ## prior.location[,1] <- 0
## ## prior.location["alcantar",1] <-  2
## ## prior.location ["sanchez",1] <- -2
## ## prior.precision[,1] <- 1
## ## prior.precision["alcantar",1] <- 4
## ## prior.precision ["sanchez",1] <- 4
## ## # assign priors for first round in council
## ## prior.location[] <- 0
## ## prior.location["alcantar",] <-  2
## ## prior.location ["sanchez",] <- -2
## ## prior.location["banos",] <-  2
## ## prior.location["figueroa",] <- -2
## ## prior.precision[,1] <- 1
## ## prior.precision["alcantar",] <- 4
## ## prior.precision ["sanchez",] <- 4
## ## prior.precision ["banos",] <- 4
## ## prior.precision ["figueroa",] <- 4
## ## #
## # assign priors for first round in council terms 2 and 3
## prior.location[] <- 0
## prior.location  ["merino",] <-  2
## prior.location["cardenas",] <- -2
## prior.precision[,1] <- 1
## prior.precision  ["merino",] <- 4
## prior.precision["cardenas",] <- 4

## prior.location  # inspect
## prior.precision # inspect

## ##############################
## ## will receive party means ##
## ##############################
## party.locations <- data.frame(matrix(NA, ncol = T, nrow = 5))
## colnames(party.locations) <- colnames(prior.location)
## rownames(party.locations) <- c("pri", "pan", "prd", "pvem", "morena")

########################################
## term-by-term start-end and members ##
########################################
terms.dates <- data.frame(
    term = 1:15,
    term.a = c(1:9,"a","b","c","d","e","f"),
    vot1st =  ymd("19940603", "19961031", "20001211", "20031105", "20071217", "20080215", "20080821", "20101031", "20111215", "20130220", "20131031", "20140411", "20170405", "20200417", "20200723"),
    votlast = ymd("19960712", "20001114", "20031021", "20071128", "20080128", "20080814", "20101027", "20111214", "20130206", "20131028", "20140402", "20170328", "20200401", "20200708", "20230403"))
terms.dates$start <- terms.dates$vot1st
terms.dates$end <- c(terms.dates$vot1st[2:15], NA)
# merge term 5 to 4
terms.dates$end[terms.dates$term==4] <- terms.dates$end[terms.dates$term==5]
terms.dates <- terms.dates[-which(terms.dates$term==5),]
# mid-date
terms.dates$mid <- as.Date(terms.dates$start + as.duration(interval(terms.dates$start, terms.dates$end))/2) # as.Date drop UTC

#
## # members
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
## term.members <- cbind(terms.dates, tmp)
## # inspect
## tmp <- term.members[4, grep("^m[0-9]", colnames(terms.dates))]
## tmp[!is.na(tmp)]

##############################################
## Read votes, exported by code/data-prep.r ##
##############################################
vot.raw <-read.csv("v23.csv",  header=TRUE)
vot.raw <-read.csv("v456789ab.csv",  header=TRUE)
vot.raw <-read.csv("vcde.csv",  header=TRUE)
vot <- vot.raw # duplicate for manipulation

#########################################################
## term 5 has one contested vote only, merge to term 4 ##
#########################################################
sel <- which(vot$term==5)
if (length(sel)>0) vot$term[sel] <- 4
#######################################################################################
## term 11 has 2 contested votes, drop it (tried merging to term 10, no convergence) ##
#######################################################################################
sel <- which(vot$term==11)
#if (length(sel)>0) vot$term[sel] <- 10
if (length(sel)>0) vot <- vot[-sel,]

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
table(factor(vot$dunan, labels = c("contested","not")), vot$yr, useNA = "ifany")
sel <- which(vot$dunan==1)
if (length(sel)>0) vot <- vot[-sel,]


###########################
## Taken from piper code ##
###########################
## ONE-DIM ARRANGEMENT
# duplicate for manipulation
votes <- vot
# select term for estimation
sel <- which(votes$term==2)
# keep appropriate rows
votes <- votes[sel,]



ids[1,]
terms
ls()
x
# keep votes cols only
sel <- which(colnames(votes) %in% c("folio","date","yr","mo","dy","qtr","sem","term","dunan","t"))
votes <- votes[,-sel]
head(votes)
table(votes$lujambio)
# 
votes[votes==2] <- -1  # los nays se codifican -1s
votes[votes==3 | votes==4 | votes==5] <- 0  # abstentions and absences coded 0s
votes[votes==0] <- -1  # ABSTENCIONES == NAY

I <- dim(votes)[1]; J <- dim(votes)[2]
agreeMatrix <- matrix(NA, ncol=J, nrow=J); tmp <- rep(NA, times=I)
for (j in 1:J){
    agreeMatrix[j,j] <- 1  ## DIAGONAL
              }
for (j1 in 2:J){
    for (j2 in (j1-1):1){
        for (i in 1:I){
            tmp[i] <- ifelse(votes[i,j1]==votes[i,j2], 1, 0)
                      }
        agreeMatrix[j2,j1] <- sum(tmp)/I; agreeMatrix[j1,j2] <- agreeMatrix[j2,j1]
        print( paste("j1 =",j1,"; j2 =",j2) )
                        }
}


# SQUARED DISTANCES
sd <- (1-agreeMatrix)^2
## DOUBLE-CENTRED MATRIX
pmean <- rep(NA, times=J); mmat <- mean(sd); dc <- sd
for (j in 1:J){
    pmean[j] <- mean(sd[j,])
              }
for (r in 1:J){
    for (c in 1:J){
        dc[r,c] <- (sd[r,c] - pmean[r] - pmean[c] + mmat)/-2
                  }
              }
## SIMPLE ONE-DIM IDEAL POINTS
tmp <- sqrt(dc[1,1])
ip  <- c(tmp, dc[2:J,1]/tmp)
##
## EXTREMA DERECHA
plot(x=ip)
thr <- .14
data.frame(ip=ip[c(1:J)[ip>thr]], id=dipdat$id[c(1:J)[ip>thr]], nom=dipdat$nom[c(1:J)[ip>thr]], part=dipdat$part[c(1:J)[ip>thr]], noVote=dipdat$noVoteRate[c(1:J)[ip>thr]])
##EXTREMA IZQUIERDA
thr <- -.185
data.frame(ip=ip[c(1:J)[ip<thr]], id=dipdat$id[c(1:J)[ip<thr]], nom=dipdat$nom[c(1:J)[ip<thr]], part=dipdat$part[c(1:J)[ip<thr]], noVote=dipdat$noVoteRate[c(1:J)[ip< thr]])
##
plot(c(-.3,.3), c(1,7), type="n")
for (j in 1:J){
    points(ip[j], dipdat$part[j], pch=20,col=dipdat$color[j])
    
}


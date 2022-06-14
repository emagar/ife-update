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
## terms <- 12:13
## terms.grep <- "[cd]"
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

##########################################
## Prep object to receive yearly priors ##
##########################################
prior.location <- data.frame(matrix(NA, nrow = length(ids$column), ncol = T))
rownames(prior.location) <- ids$column
colnames(prior.location) <- paste0("term", tees)
prior.precision <- prior.location
prior.precision[] <- 1 # for N=2-on
#
## # assign priors for first round in council term 12
## prior.location[] <- 0
## prior.location  ["favela",] <-  2
## prior.location["murayama",] <- -2
## prior.precision[,1] <- 1
## prior.precision  ["favela",] <- 4
## prior.precision["murayama",] <- 4
## #
## # assign priors for first round in council terms 4 to 11
## prior.location[,1] <- 0
## prior.location["alcantar",1] <-  2
## prior.location ["sanchez",1] <- -2
## prior.precision[,1] <- 1
## prior.precision["alcantar",1] <- 4
## prior.precision ["sanchez",1] <- 4
## # assign priors for first round in council
## prior.location[] <- 0
## prior.location["alcantar",] <-  2
## prior.location ["sanchez",] <- -2
## prior.location["banos",] <-  2
## prior.location["figueroa",] <- -2
## prior.precision[,1] <- 1
## prior.precision["alcantar",] <- 4
## prior.precision ["sanchez",] <- 4
## prior.precision ["banos",] <- 4
## prior.precision ["figueroa",] <- 4
## #
# assign priors for first round in council terms 2 and 3
prior.location[] <- 0
prior.location  ["merino",] <-  2
prior.location["cardenas",] <- -2
prior.precision[,1] <- 1
prior.precision  ["merino",] <- 4
prior.precision["cardenas",] <- 4

prior.location  # inspect
prior.precision # inspect

##############################
## will receive party means ##
##############################
party.locations <- data.frame(matrix(NA, ncol = T, nrow = 5))
colnames(party.locations) <- colnames(prior.location)
rownames(party.locations) <- c("pri", "pan", "prd", "pvem", "morena")

#######################################
## term-by-term start-en and members ##
#######################################
terms.dates <- data.frame(
    term = 1:15,
    term.a = c(1:9,"a","b","c","d","e","f"),
    vot1st = ymd("19940603", "19961031", "20001211", "20031105", "20071217", "20080215", "20080821", "20101031", "20111215", "20130220", "20131031", "20140411", "20170405", "20200417", "20200723"),
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
## vot.raw <-read.csv("v456789ab.csv",  header=TRUE)
## vot.raw <-read.csv("vcde.csv",  header=TRUE)
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
## identify item anchors and place them as votes 1 and 2, both should point aye = north/right     ##
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
## - folio 8320 PRI's denuncia against FCH (Minority PRI plus Córdova) Aye=left                   ##
## - folio 8814 Aristegui's vs PRD-PT with engrose (Minority PRD plus Nacif) Aye=left             ##
## ** term==10 **                                                                                 ##
## - folio 9230 Sanción del pri al pan y gob huauchinango (Minority = pri) aye = left             ##
## - folio 9408 Declarar leve la multa a un periódico (Minority pri plus nacif) aye = right       ##
####################################################################################################
anch <- c(  385,  1045  # term==2  dates 16-12-97 14-11-00
        ,  1298,  1669  # term==3          6-4-01  28-1-03
        ,  2401,  2479  # term==4:5       23-8-04  31-1-05
        ,  3641,  3924  # term==6         20-2-08  23-5-08
        ,  6127,  6174  # term==7         29-3-09   6-4-09
        ,  7421,  7633  # term==8         18-1-11   6-6-11
        ,  8317,  8320  # term==9         21-3-12  21-3-12
        ,  9230,  9408  # term==10:11      8-5-13  26-9-13
        , 10096, 10268  # term==12         2-7-14  7-10-14
          )
anchors <- which(vot$folio %in% anch)
# some votes need aye/nay reversal to point North
sel <- which(vot$folio[anchors] %in% c(385, 2479, 6127, 6174, 7421, 8317, 8320, 9230, 10096, 10228))
tmp <- vot[anchors[sel], ids$column] # subset votes that must chg
for (i in 1:nrow(tmp)){
    tmp2 <- as.character(tmp[i,]) # extract vector, to character
    tmp2 <- mapvalues(tmp2, from = c(1,2), to = c(2,1))
    tmp[i,] <- as.numeric(tmp2)
}
vot[anchors[sel], ids$column] <- tmp
rm(tmp,tmp2,anch)

#########################################################
## move anchor votes to start of time series, so that  ##
## they appear as votes 1 and 2 when term is subsetted ##
#########################################################
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
report_prez <- function(x) {
    prez <- c("(carpizo 1994-96)", "(woldenberg I 1996-2000)", "(woldenberg II 2000-03)", "(ugalde 2003-07)", "(albo 2008)", "(valdés I 2008)", "(valdés II 2008-10)", "(valdés III 2010-11)", "(valdés IV 2011-13)", "(valdés V 2013)", "(valdés VI 2013-14)", "(córdoba I 2014-)")
    return(prez[x])
}
t <- c(1:T)[1]
paste("t =", t, "is term", tees[t], report_prez(tees[t])); rm(report_prez)

#for (t in 1:7){

###################################################
## by terms: determine members and their parties ##
###################################################
## sel <- c("c","d")
## sel <- c("4","6","7","8","9","a","b")
sel <- c("2","3")
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
molten.rc$rc <- car::recode (molten.rc$rc, "2=0; c(3,4,5,6)=NA") # abstain|absent|recuse to NA

ife.data.vector <-
    dump.format(list(y = molten.rc$rc 
                   , n.member = M
                   , n.item = V
                   , n.obs  = nrow(molten.rc)
#                   , mean.theta = prior.location[map.member.indices$actual ,t]
#                   , precision.theta = prior.precision[map.member.indices$actual ,t]
                   , vote   = molten.rc$vote
                   , member = molten.rc$member
#                   , a = c(2,1,1.5)   # known component sizes: 4 left, 2 pan, 3 pri 
#                   , a = c(1,1.5,2)   # known component sizes: 2 pan, 3 pri, 4 left
                   , a = c(2,2,2)    # agnostic component sizes
                     ))

ife.parameters = c("theta", "alpha", "beta", "deviance"
                   , "component", "promedios", "gaps")


ife.inits <- function() {
  dump.format(
    list(
#      theta = c(rnorm(3), NA, rnorm(4), NA)
      theta   = rnorm(M)
#                     w  b  c  c  l  m  m  p  z
      , component = c(3, 3, 1, 1, 2, 3, 2, 3, 1)
      , alpha = rnorm(V)
#      , beta  = rnorm(V)
      , beta  = c(NA, NA, rnorm(V-2))
#      , mix_proportions = c(0.33, .22, .45)  # known component sizes: 3 left, 2 pan, 4 pri 
#      , mix_proportions = c(0.45, .22, .33)  # known component sizes: 4 left, 2 pan, 3 pri 
      , mix_proportions = c(.33, .33, .33)  # agnostic component sizes
      ,'.RNG.name'="base::Wichmann-Hill"
      ,'.RNG.seed'= 1971)   #randomNumbers(n = 1, min = 1, max = 1e+04, col=1))
  )
}

## # inspect spike priors to code ife.vector
## point.est
## prior.location[map.member.indices$actual,t]
## prior.precision[map.member.indices$actual,t]

# read model
source ("../code/mix-model.r")

results <- run.jags(
  model    = ife.model.items.mix,
  monitor  = ife.parameters,
  method   = "parallel",
  n.chains = 2,
  data     = ife.data.vector,
  inits    = list (ife.inits(), ife.inits()),
  #thin = 20, burnin = 29000, sample = 3000,
  #thin = 250, burnin = 50000, sample = 200,
  thin =  50, burnin = 10000, sample = 200,
  #thin =   5, burnin =   200, sample = 200,
  plots = FALSE)

chains <- mcmc.list(list (results$mcmc[[1]], results$mcmc[[2]]))
dim(chains[[1]])
# check model convergence 
gelman.diag <- gelman.diag (chains, multivariate=F)
gelman.diag

############################
## store posterior sample ##
############################
getwd()
load("posterior-samples/in-git/theta-chains-statics-2-3-items.RData")
load("posterior-samples/in-git/theta-chains-statics-45-6-7-8-9-10-items.RData")
load("posterior-samples/not-in-git/posterior-chains-statics-mix-2-3-items.RData")
post.samples[[t]] <- list(map.vote.indices=map.vote.indices,
                          map.member.indices=map.member.indices,
                          map.time.indices=map.time.indices,
                          gelman.diag=gelman.diag,
                          chains=chains)
names(post.samples) <- paste0("term", tees)
summary(post.samples)
#save(post.samples, file = "posterior-samples/in-git/theta-chains-statics-45-6-7-8-9-10-items.RData")
save(post.samples, file = "posterior-samples/not-in-git/posterior-chains-statics-mix-2-3-items.RData")

#######################
## load saved chains ##
#######################
load("posterior-samples/in-git/theta-chains-statics-2-3-items.RData")
load("posterior-samples/in-git/theta-chains-statics-45-6-7-8-9-10-items.RData")
load("posterior-samples/not-in-git/posterior-chains-statics-mix-2-3-items.RData")

chains <- post.samples[[1]]$chains

#################################################################################
### Lines added Jun 3, 2022
consejero.grupos <- chains[[1]][,grep("component", colnames (chains[[1]]))]
colnames (consejero.grupos) <- column.t
dim(consejero.grupos)

# function extracting prob each member belongs in group 1 2 3
prob.group.belonging <- function (x,n=0) {
  g1 <- length (x[x==1])/length(x)
  g2 <- length (x[x==2])/length(x)
  g3 <- length (x[x==3])/length(x)
  if (n==0) g <- c (g1, g2, g3) # default n=0 returns all three probs
  if (n==1) g <- g1
  if (n==2) g <- g2
  if (n==3) g <- g3
  return (g)
}

# triplot functions
source("../code/triplots.r")

# Hacer triplot con objeto consejero.grupos
tmp <- apply (consejero.grupos, 2, prob.group.belonging)
rownames(tmp) <- c("one", "two", "three") 
#
# triplot
#pdf(file = "../plots/group-probs-wold1.pdf")
par(mfrow=c(3,3))
for (i in 1:9){
#i <- 5
tri.color <- function(x){
    the.max <- which.max(x)
    names(the.max) <- NULL
    if (the.max==1) the.max <- col.prd
    if (the.max==2) the.max <- col.pan
    if (the.max==3) the.max <- col.pri
    return(the.max)
}
la.ternera(tmp[,i], cex.pts = 2, color = tri.color(tmp[,i]), main = colnames(tmp)[i], add.sign=FALSE)
#         , labs=c("2","3","1"), left.right.up=c("two","three","one")) # expression(hat(v)[2018])
#
# sample posterior chains 100-by-100 with overlaps
jitter <- function() runif(1, min=-.02, max=.02)
tmp2 <- as.data.frame(consejero.grupos)
for (j in seq(from = 1, to = 2901, by = 20)){
    tmp3 <- tmp2[j:(j+99),i]
    tmp3 <- c(pr1=length(tmp3[tmp3==1]) / length(tmp3), pr2=length(tmp3[tmp3==2]) / length(tmp3), pr3=length(tmp3[tmp3==3]) / length(tmp3))
    tmp4 <- data.frame(t(tern2cart(tmp3))); colnames(tmp4) <- c("x","y")
    tmp4$x <- apply(tmp4, 1, function(x) x[1]+jitter()); tmp4$y <- apply(tmp4, 1, function(x) x[2]+jitter())
    points(tmp4 , col = tri.color(tmp3), cex = 1.2 )
    }
    points(t(tern2cart(tmp[,i])), cex = 2)#, col = tri.color(tmp[,i] ))
    points(t(tern2cart(tmp[,i])), cex = 1.2)#, col = tri.color(tmp[,i] ))
}
#dev.off()


promedios <- chains[[1]][,grep("promedios", colnames (chains[[1]]))]
gaps <- chains[[1]][,grep("gaps", colnames (chains[[1]]))]
thetas <- chains[[1]][,grep("theta", colnames (chains[[1]]))]
components <- chains[[1]][,grep("compo", colnames (chains[[1]]))]

colMeans (promedios)
colMeans (gaps)
th <- colMeans (thetas)
names(th) <- column.t
modas <- apply(components, 2, FUN = function(x) {
        uniqv <- unique(x)
        return(uniqv[which.max(tabulate(match(x, uniqv)))])
    }
)
names(modas) <- column.t
cbind(modas, th)

x

#################################################################################


##################################################
## will receive point estimates and 80pct bands ##
##################################################
load("posterior-samples/in-git/theta-chains-statics-2-3-items.RData")
load("posterior-samples/in-git/theta-chains-statics-45-6-7-8-9-10-items.RData")
load("posterior-samples/not-in-git/posterior-chains-statics-mix-2-3-items.RData")
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

#################################################
## t 1--4 (terms 45-6-7-8) used votes inverted ##
#################################################
point.est[,1:4] <- -point.est[,1:4]
lo[,1:4] <- -lo[,1:4]; hi[,1:4] <- -hi[,1:4]
tmp <- lo[,1:4]; lo[,1:4] <- hi[,1:4]; hi[,1:4] <- tmp

######################################
## t 2 (term 3) used votes inverted ##
######################################
point.est[,2] <- -point.est[,2]
lo[,2] <- -lo[,2]; hi[,2] <- -hi[,2]
tmp <- lo[,2]; lo[,2] <- hi[,2]; hi[,2] <- tmp

#######################
## normalize to -1 1 ##
#######################
tmp <- point.est
m <- apply(tmp, 2, function(x) min(x, na.rm = TRUE))
M <- apply(tmp, 2, function(x) max(x, na.rm = TRUE))
for (i in 1:T){
    tmp[,i] <- (tmp[,i]-m[i]) / (M[i]-m[i])
}
point.est <- tmp

#####################################
## plot terms 4:11 point estimates ##
#####################################
#pdf(file = "../plots/statics-terms-45-6-7-8-9-10-item.pdf", width = 10, height = 7)
plot(c(.25,T+.75), c(min(point.est, na.rm = TRUE), max(point.est, na.rm = TRUE)), type="n", xlab = "term", ylab = "ideal point", axes = FALSE,
     main = "Static estimates by term, item-identified") 
axis(1, at = 1:T, labels = c("Ugalde\n2003-08", "Valdés I\n2008", "Valdés II\n2008-10", "Valdés III\n2010-11", "Valdés IV\n2011-13", "Valdés V\n2013"), padj = .25)
axis(2)
for (t in 1:T){
    sel <- c("4","6","7","8","9","a","b"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t   <- ids$party [sel]
    column.t  <- ids$column[sel]
    color.t   <- ids$color[sel]
    color50.t <- ids$color50[sel]
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
sel.r <- ids$column[which(!is.na(point.est[,5]))]
text(x = 5,
     y = point.est[sel.r, 5],
     labels = ids[sel.r, "short"],
     pos = 4 )
#dev.off()

#################################################################
## plot terms 4:11 with time-scale X axis and confidence bands ##
#################################################################
#pdf(file = "../plots/statics-terms-45-6-7-8-9-10-item-time-scale.pdf", width = 10, height = 7)
sel <- which(terms.dates$term %in% 4:10)
terms.dates <- terms.dates[sel,] # subset terms.dates
# set plot
plot(c(min(terms.dates$start), max(terms.dates$end)+100),
     c(min(lo, na.rm = TRUE)-.5, max(hi, na.rm = TRUE)+.15), type="n", xlab = "Year", ylab = "Ideal point", axes = FALSE,
     main = "Static estimates by term, item-identified")
#axis(1, at = c(min(terms.dates$start), max(terms.dates$end)), labels = FALSE)
axis(1, at = seq(from = ymd("19960101"), to = ymd("20220101"), by = "year"), labels = FALSE)
axis(1, at = seq(from = ymd("20040701"), to = ymd("20130701"), by = "year"), tick = FALSE, labels = 2004:2013)
#axis(1, at = terms.dates$mid, labels = c("Ugalde\n2003-08", "Valdés I\n2008", "Valdés II\n2008-10", "Valdés III\n2010-11", "Valdés IV\n2011-13", "Valdés V\n2013"), padj = .25)
axis(2)
# lines connecting point estimates
for (i in 1:nrow(point.est)){
    #i <- 1
    lines(x = terms.dates$mid,
          y = point.est[i,],
          col = ids$color[i])
}
# add point estimates and confidence bands
for (t in 1:T){
    #t <- 2
    sel <- c("4","6","7","8","9","a","b"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t  <- ids$party [sel]
    column.t <- ids$column[sel]
    color.t <- ids$color[sel]
    color50.t <- ids$color50[sel]
    tmp <- point.est[column.t, t]
    points(x = rep(terms.dates$mid[t], length(tmp)),
           y = tmp,
           col = "white", pch = 19) # makes points non-transparent
    points(x = rep(terms.dates$mid[t], length(tmp)),
           y = tmp,
           col = color.t)           # draws point estimates
    # prepares data for conf bands
    tmp <- data.frame(y1 = lo[column.t, t],
                      y2 = lo[column.t, t],
                      y3 = hi[column.t, t],
                      y4 = hi[column.t, t])
    tmp$x1 <- terms.dates$start[t]
    tmp$x2 <- terms.dates$end[t]
    tmp$x3 <- terms.dates$end[t]
    tmp$x4 <- terms.dates$start[t]
    tmp$color <- color50.t
    tmp$who <- column.t
    # turn into list
    tmp1 <- lapply(1:length(sel), function(x){
        res <- list(xx = tmp[x, grep("^x", colnames(tmp))],
                    yy = tmp[x, grep("^y", colnames(tmp))],
                    color = tmp$color[x],
                    who = tmp$who[x])
        return(res)
    })
    lapply(tmp1, function(x){polygon(x$xx,x$yy, col=x$color, border=NA)}) # draws transparent confidence bands
    if (t==1) abline(v = tmp1[[1]]$xx$x1, lty = 3)
    abline(v = tmp1[[1]]$xx$x2, lty = 3)
}
# add member names
sel.r <- ids$column[which(!is.na(point.est[,1]))]
text(x = terms.dates$mid[1],
     y = point.est[sel.r, 1],
     labels = ids[sel.r, "short"],
     cex = .9, 
     pos = c(2,2,4,2,2,2,2,2,2) )
sel.r <- ids$column[which(!is.na(point.est[,2]))]
sel.r <- sel.r[7] # keep valdés only
text(x = terms.dates$mid[2],
     y = point.est[sel.r, 2],
     labels = ids[sel.r, "short"],
     cex = .9, 
     pos = c(2) )
sel.r <- ids$column[which(!is.na(point.est[,5]))]
sel.r <- sel.r[-c(1,5,7,9)] # don't put figueroa here
text(x = terms.dates$mid[5],
     y = point.est[sel.r, 5],
     labels = ids[sel.r, "short"],
     cex = .9, 
     pos = c(2,4,2,2,2) )
sel.r <- ids$column[which(!is.na(point.est[,6]))]
sel.r <- sel.r[c(5,7,8)] # figueroa marván here
text(x = terms.dates$mid[6],
     y = point.est[sel.r, 6],
     labels = ids[sel.r, "short"],
     cex = .9, 
     pos = c(4,4,4) )
#
# add term labels on top
text(x = terms.dates$mid,
     y = max(hi, na.rm = TRUE)+.25,
     labels = c("Ugalde", "Valdés I", "Valdés II", "Valdés III", "Valdés IV", "Valdés V"), cex = .8)
#
# add elections
text(x=ymd(c("20060702", "20090705", "20120701", "20150607", "20180701", "20210606")),
     y=min(lo, na.rm = TRUE)-.7 ,
     labels="*")
#
# contested votes histogram at bottom
vot$date <- ymd(vot$date)
# aggregate weekly split votes
tmp <- data.frame(
    dt = vot$date,
    wk = floor_date(vot$date, "weeks"))
tmp <- tmp[order(tmp$wk),]
tmp$n <- 0
tmp$n <- ave(tmp$n, as.factor(tmp$wk), FUN=length, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$wk)==FALSE,]
# change scale
tmp$n <- (tmp$n/(2*max(tmp$n))) + (min(lo, na.rm = TRUE)-.5)
# to list
tmp1 <- lapply(1:nrow(tmp), function(x){
    res <- list(xx = c(tmp$wk[x], tmp$wk[x]),
                yy = c((min(lo, na.rm = TRUE)-.5), tmp$n[x]))
    return(res)
})
#
lapply(tmp1, function(x){lines(x$xx,x$yy, col = "gray")}) # draws transparent confidence bands
#dev.off()

#####################################
## plot terms 2:3 point estimates ##
#####################################
#pdf(file = "../plots/statics-terms-2-3-item.pdf", width = 10, height = 7)
plot(c(.25,T+.75), c(min(point.est, na.rm = TRUE), max(point.est, na.rm = TRUE)), type="n", xlab = "term", ylab = "ideal point", axes = FALSE,
     main = "Static estimates by term, item-identified") 
axis(1, at = 1:T, labels = c("Woldenberg I\n1996-2000", "Woldenberg II\n2000-2003"), padj = .25)
axis(2)
for (t in 1:T){
    sel <- c("2","3"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t   <- ids$party [sel]
    column.t  <- ids$column[sel]
    color.t   <- ids$color[sel]
    color50.t <- ids$color50[sel]
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
sel.r <- ids$column[which(is.na(point.est[,1]))]
text(x = 2,
     y = point.est[sel.r, 2],
     labels = ids[sel.r, "short"],
     pos = 4 )
#dev.off()

#################################################################
## plot terms 2:3 with time-scale X axis and confidence bands ##
#################################################################
#pdf(file = "../plots/statics-terms-2-3-item-time-scale.pdf", width = 10, height = 7)
sel <- which(terms.dates$term %in% 2:3)
terms.dates <- terms.dates[sel,] # subset terms.dates
# set plot
plot(c(min(terms.dates$start), max(terms.dates$end)+100),
     c(min(lo, na.rm = TRUE)-.5, max(hi, na.rm = TRUE)+.15), type="n", xlab = "Year", ylab = "Ideal point", axes = FALSE,
     main = "Static estimates by term, item-identified")
#axis(1, at = c(min(terms.dates$start), max(terms.dates$end)), labels = FALSE)
axis(1, at = seq(from = ymd("19960101"), to = ymd("20220101"), by = "year"), labels = FALSE)
axis(1, at = seq(from = ymd("19970701"), to = ymd("20030701"), by = "year"), tick = FALSE, labels = 1997:2003)
#axis(1, at = terms.dates$mid, labels = c("Ugalde\n2003-08", "Valdés I\n2008", "Valdés II\n2008-10", "Valdés III\n2010-11", "Valdés IV\n2011-13", "Valdés V\n2013"), padj = .25)
axis(2)
# lines connecting point estimates
for (i in 1:nrow(point.est)){
    #i <- 1
    lines(x = terms.dates$mid,
          y = point.est[i,],
          col = ids$color[i])
}
# add point estimates and confidence bands
for (t in 1:T){
    #t <- 2
    sel <- c("2","3"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t  <- ids$party [sel]
    column.t <- ids$column[sel]
    color.t <- ids$color[sel]
    color50.t <- ids$color50[sel]
    tmp <- point.est[column.t, t]
    points(x = rep(terms.dates$mid[t], length(tmp)),
           y = tmp,
           col = "white", pch = 19) # makes points non-transparent
    points(x = rep(terms.dates$mid[t], length(tmp)),
           y = tmp,
           col = color.t)           # draws point estimates
    # prepares data for conf bands
    tmp <- data.frame(y1 = lo[column.t, t],
                      y2 = lo[column.t, t],
                      y3 = hi[column.t, t],
                      y4 = hi[column.t, t])
    tmp$x1 <- terms.dates$start[t]
    tmp$x2 <- terms.dates$end[t]
    tmp$x3 <- terms.dates$end[t]
    tmp$x4 <- terms.dates$start[t]
    tmp$color <- color50.t
    tmp$who <- column.t
    # turn into list
    tmp1 <- lapply(1:length(sel), function(x){
        res <- list(xx = tmp[x, grep("^x", colnames(tmp))],
                    yy = tmp[x, grep("^y", colnames(tmp))],
                    color = tmp$color[x],
                    who = tmp$who[x])
        return(res)
    })
    lapply(tmp1, function(x){polygon(x$xx,x$yy, col=x$color, border=NA)}) # draws transparent confidence bands
    if (t==1) abline(v = tmp1[[1]]$xx$x1, lty = 3)
    abline(v = tmp1[[1]]$xx$x2, lty = 3)
}
# add member names
sel.r <- ids$column[which(!is.na(point.est[,1]))]
text(x = terms.dates$mid[1],
     y = point.est[sel.r, 1],
     labels = ids[sel.r, "short"],
     cex = .9, 
     pos = c(4,2,2,2,4,2,2,2,2) )
sel.r <- ids$column[which(is.na(point.est[,1]))]
text(x = terms.dates$mid[2],
     y = point.est[sel.r, 2],
     labels = ids[sel.r, "short"],
     pos = 4 )
#
# add term labels on top
text(x = terms.dates$mid,
     y = max(hi, na.rm = TRUE)+.25,
     labels = c("Woldenberg I", "Woldenberg II"), cex = .8)
#
# add elections
text(x=ymd(c("19970702", "20000702", "20030701")),
     y=min(lo, na.rm = TRUE)-.63 ,
     labels="*")
#
# contested votes histogram at bottom
vot$date <- ymd(vot$date)
# aggregate weekly split votes
tmp <- data.frame(
    dt = vot$date,
    wk = floor_date(vot$date, "weeks"))
tmp <- tmp[order(tmp$wk),]
tmp$n <- 0
tmp$n <- ave(tmp$n, as.factor(tmp$wk), FUN=length, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$wk)==FALSE,]
# change scale
tmp$n <- (tmp$n/(2*max(tmp$n))) + (min(lo, na.rm = TRUE)-.58)
# to list
tmp1 <- lapply(1:nrow(tmp), function(x){
    res <- list(xx = c(tmp$wk[x], tmp$wk[x]),
                yy = c((min(lo, na.rm = TRUE)-.58), tmp$n[x]))
    return(res)
})
#
lapply(tmp1, function(x){lines(x$xx,x$yy, col = "gray")}) # draws transparent confidence bands
#dev.off()




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


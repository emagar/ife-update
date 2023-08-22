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

options(width = 110)
rm(list = ls())
workdir <- c("/home/eric/Dropbox/data/rollcall/ife_cg/ife-update/data/")
# workdir <- c("~/Dropbox/ife-update/data/")
setwd(workdir)

################################################################
## select temporal range of full analysis (broken down below) ##
################################################################
#terms <- 12:13
terms <- 4

# term letter equivalence
term.eq <- data.frame(term = 1:16,
                      term.a = c(1:9,"a","b","c","d","e","f","g")
                      )
# create pattern for regex search
terms.grep <- paste(c("[",term.eq$term.a[terms],"]"), collapse = '')
#
# Define colors and plotting names
# OJO: en tenure term==10 es a, term==11 es b etc. 
ids <- read.csv("../ancillary/consejo-general-ife-ine.csv")
ids <- ids[order(ids$ord),] # sort
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
ids[1,]
#
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
#
##############################################
## Read votes, exported by code/data-prep.r ##
##############################################
vot.raw <-read.csv("v23456789abcdef.csv",  header=TRUE)
#
#############################################
## subset ids and periodicization to range ##
#############################################
ids <- ids[grep(pattern = terms.grep, ids$tenure),]
yr.by.yr <- yr.by.yr[grep(pattern = terms.grep, yr.by.yr$term.a),]
ids[, c("column","sponsor","tenure")] # inspect
#
# period name columns
per.cols <- ids$column
#
## #############################################################
## ## select temporal unit -- discrete periods to be analyzed ##
## ## e.g. terms, year-by-year, semester...                   ##
## #############################################################
## table(term=yr.by.yr$term, yrn=yr.by.yr$yrn) # inspect
## #tees <- yr.by.yr$yrn # years 8:18 cover terms 4:11, ug to 2014 reform
## ## tees <- c(12:13) # post-2014 terms córdoba I and II
## ## tees <- c(4,6:11) # terms ugalde I+II valdés I II III IV and V+VI
## ## tees <- c(4,6:10) # terms ugalde I+II valdés I II III IV and V
## tees <- c(2:3)
## T <- length(tees)
#
## ########################################
## ## term-by-term start-end and members ##
## ########################################
## terms.dates <- data.frame(
##     term = 1:16,
##     term.a = c(1:9,"a","b","c","d","e","f","g"),
##     vot1st =  ymd("19940603", "19961031", "20001211", "20031105", "20071217", "20080215", "20080821", "20101031", "20111215", "20130220", "20131031", "20140411", "20170405", "20200417", "20200723", "20230404"),
##     votlast = ymd("19960712", "20001114", "20031021", "20071128", "20080128", "20080814", "20101027", "20111214", "20130206", "20131028", "20140402", "20170328", "20200401", "20200708", "20230403", NA))
## terms.dates$start <- terms.dates$vot1st
## terms.dates$end <- c(terms.dates$vot1st[2:16], NA)
## # merge term 5 to 4
## terms.dates$end[terms.dates$term==4] <- terms.dates$end[terms.dates$term==5]
## terms.dates <- terms.dates[-which(terms.dates$term==5),]
## # mid-date
## terms.dates$mid <- as.Date(terms.dates$start + as.duration(interval(terms.dates$start, terms.dates$end))/2) # as.Date drop UTC
#
##################
## info columns ##
##################
info.cols <- c("folio","date","yr","mo","dy","qtr","sem","term","dunan","dpass")
sel.r <- which(vot.raw$term %in% terms)
#
########################################
## subset to term's votes and members ##
########################################
vot <- vot.raw[sel.r, c(per.cols, info.cols)]
#
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
#
###########################################
## summarize then drop uncontested votes ##
###########################################
table(factor(vot$dunan, labels = c("contested","not")), vot$term, useNA = "ifany")
table(factor(vot$dunan, labels = c("contested","not")), useNA = "ifany")
table(factor(vot$dunan, labels = c("contested","not")), vot$yr, useNA = "ifany")
sel <- which(vot$dunan==1)
if (length(sel)>0) vot <- vot[-sel,]
#
###########################################################
## RECODE ABSENCES AND ABSTENTIONS AS VOTE WITH MAJORITY ##
###########################################################
sel <- which(vot$dpass==1)
tmp <- vot[sel, -which(colnames(vot) %in% info.cols)] # subset vote columns
tmp[1,]
tmp[tmp==3 | tmp==4 | tmp==5] <- 1
vot[sel, -which(colnames(vot) %in% info.cols)] <- tmp # return manipulation to data
#
sel <- which(vot$dpass==0)
tmp <- vot[sel, -which(colnames(vot) %in% info.cols)] # subset vote columns
tmp[tmp==3 | tmp==4 | tmp==5] <- 2
vot[sel, -which(colnames(vot) %in% info.cols)] <- tmp # return manipulation to data
#
###########################
## Taken from piper code ##
###########################
## ONE-DIM ARRANGEMENT
#
# keep votes cols only
votes <- vot[,-which(colnames(vot) %in% info.cols)] # duplicate votes only
table(votes$murayama)
# 
votes[votes==2] <- -1  # los nays se codifican -1s
#
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
#
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
names(ip) <- per.cols



plot(x = c(min(ip)*1.1, max(ip)*1.1), y = c(0,0), type = "n", axes = FALSE, xlab = "", ylab = "", main = paste0("Term =  ", terms))
abline(h=0)
points(x = ip, y = rep(0, length(ip)), pch = 19)
#text(x = ip, y = rep(-.1, length(ip)), labels = round(ip, 1))
for (i in 1:length(ip)){
    text(x = ip[i], y = 0, labels = per.cols[i], srt = 90, pos = 4)
}

# Alexader Tahc's OC method
library(npideal)
library(pscl)     # simon jackman's package

# keep votes cols only
votes <- vot[,-which(colnames(vot) %in% info.cols)] # duplicate votes only
votes <- as.matrix(votes)
votes <- t(votes)
head(vot)
# prep roll-call object
votes <- rollcall(votes,
                  yea = 1,
                  nay = 2,
                  missing = NA,
                  notInLegis = c(3,4,5),
                  legis.names = rownames(votes),
                  legis.data = ids,
                  vote.names = vot$folio,
                  vote.data = vot[,info.cols]
                  )

table(votes$votes)
with(votes, voteest(votes))
with(votes, rankAgreeR(votes, n, m))
computeCombinations(4)

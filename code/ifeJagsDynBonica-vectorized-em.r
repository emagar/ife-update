##################################################################################
# Dynamic + vectorized 
# This code runs a Bonica-like algorithm to provide a longitudinal view of IFE's
# Council General.  
# 27 Mar 2021 
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
#
library (runjags)
library (coda)


rm(list = ls())
workdir <- c("/home/eric/Dropbox/data/rollcall/ife_cg/ife-update/data/")
setwd(workdir)

# Define colors and plotting names
# OJO: en tenure term==10 es a, term==11 es b etc. 

ids <- matrix(c(
    ## "Woldenberg",       "woldenberg", "PRI", "23",
    ## "Barragán",         "barragan",   "PRD", "23",
    ## "Cantú",            "cantu",      "PRD", "23",
    ## "Cárdenas",         "cardenas",   "PRD", "23",
    ## "Lujambio",         "lujambio",   "PAN", "23",
    ## "Merino",           "merino",     "PRI", "23",
    ## "Molinar",          "molinar",    "PAN", "2" ,
    ## "Peschard",         "peschard",   "PRI", "23",
    ## "Zebadúa",          "zebadua",    "PRD", "2" ,
    ## "Rivera",           "rivera",     "PRI",  "3",
    ## "Luken",            "luken",      "PAN",  "3",
    #
    "Ugalde",           "ugalde",      "PRI",  "4",
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
ids # inspect


####################################################################
## select terms for analysis, e.g. for 4-11(b) "[456789ab]" works ##
####################################################################
tees <- c("6","7","8","9","a")
tees.grep <- "[6789a]"
T <- length(tees)
#
sel    <- grep(pattern = tees.grep, ids$tenure)
name   <- ids$name[sel]
party  <- ids$party[sel]
color  <- ids$color[sel]
column <- ids$column[sel]

## rgb.23 <- c(length=11)
## rgb.23[c(1,6,8,10)] <- rgb(1,       0, 0, 0.6) #red
## rgb.23[c(2:4,9)]    <- rgb(1, 215/255, 0, 0.6) #gold
## rgb.23[c(5,7,11)]   <- rgb(0,       0, 1, 0.6) #blue

############################################
## Read votes                             ##
## exported to disk by code/data-prep.r   ##
############################################
vot23  <- read.csv("v23.csv"        ,  header=TRUE)
vot4on <- read.csv(  "v456789ab.csv",  header=TRUE)
# choose what votes will be analyzed
#vot <- vot23  # Woldenberg
vot <- vot4on # Ugalde-Valdés-Córdova
#colnames(vot)
# subset to votes in chosen periods
sel.r <- which(vot$term %in% tees)
drop.c <- ids$column[grep(pattern = tees.grep, ids$tenure)] # column names not in terms selected
drop.c <- setdiff(ids$column, drop.c)
drop.c <- which(colnames(vot) %in% drop.c)
if (length(drop.c)>0) vot <- vot[sel.r, -drop.c]
colnames(vot)
# total members
J <- length(name); J

########################
## recode vote values ##
########################
vs <- vot[,1:J]
#table(v$albo, useNA = "always")
vs[vs==0] <- NA    ## Version probit requiere 0s y 1s
vs[vs>2]  <- NA
vs[vs==2] <- 0

# format dates
vot$date <- ymd(vot$date)
# summarize then drop uncontested votes
table(factor(vot$dunan, labels = c("contested","not")), vot$term, useNA = "ifany")
table(factor(vot$dunan, labels = c("contested","not")), useNA = "ifany")
sel <- which(vot$dunan==1)
if (length(sel)>0){
    vot <- vot[-sel,] # drop uncontested votes
    vs  <- vs [-sel,] # drop uncontested votes
}



## ###########
## ## MODEL ##
## ###########
## model1Dj.irt <- function() {
##     for (j in 1:J){        ## loop over respondents
##         for (i in 1:I){## loop over items
##             v[j,i] ~ dbern(p[j,i]); ## voting rule
##             probit(p[j,i]) <- mu[j,i]; ## sets 0<p<1 as function of mu
##             mu[j,i] <- signal[i]*x[j] - difficulty[i];              ## utility differential
## 		}
## 	}
## 	## priors ################
## 	for (j in 1:J){
## 		x[j] ~ dnorm (x.mean[j], x.tau[j]);
## 	}
## 	for (i in 1:I){
## 		signal[i] ~ dnorm(0, 0.1);
## 		difficulty[i] ~ dnorm(0, 0.25);
## 	}
## 	for (p in 1:4){ # need 5 when morena also considered
##             partyPos[p] <- mean (x[sponsors[p]]); # 4mar21: should be median, unknown function in bugs?
## 	}
## }
## #end model##############

###########
## MODEL ##
###########
## alpha is vote's   difficulty
## beta  is vote's   signal strength
## theta is member's ideal point
ife.vector <- "model {
    for (n in 1:n.obs) {
        y[n] ~ dbern (pi[n])
        probit(pi[n]) <- beta[vote[n]]*theta[member[n]] - alpha[vote[n]]
    }
    # PRIORS
    for (j in 1:n.item){
        alpha[j] ~ dnorm(0, 0.25);   
        beta [j] ~ dnorm(0, 0.1)
    }
    for (p in 1:4){ # need 5 when morena also considered
        partyPos[p] <- mean (x[sponsors[p]]); # 4mar21: should be median, unknown to jags?
    }
    # IDEAL POINTS, auto-regressive process --- 31mar21 DUDO QUE LE GUSTE north==NA CUANDO OCURRA
    for(i in setdiff(1:n.members, c(north, south))){
        theta[i] ~ dnorm ( x.mean[i], x.tau[i] ) 
    }
    theta[north] ~ dnorm ( x.mean[north], x.tau[north] ) T(0, ) # truncated positive normal
    theta[south] ~ dnorm ( x.mean[south], x.tau[south] ) T( ,0) # truncated negative normal
}"
### END MODEL ######

# Center on vote (for date), extend windows to both sides
I <- nrow (vot)
item <- 1:I  # Need to define I before
inicio <- item-15; inicio[inicio<0] <- 1
final  <- item+15; final[final>I] <- I
item.date <- vot$date #ymd(vot$yr*10000+vot$mo*100+vot$dy)
S <- length(inicio)


# We need a matrix showing whether each councilor is actually in IFE the moment the vote takes place --- will be subset to J below
IsCouncilor <- matrix (1, ncol=18, nrow=S)
IsCouncilor[ vot$term < 4 | vot$term >  4,1 ] <- NA  #      ugalde  1
IsCouncilor[ vot$term < 4 | vot$term >  6,2 ] <- NA  #        albo  2
IsCouncilor[ vot$term < 4 | vot$term >  7,3 ] <- NA  #     andrade  3
IsCouncilor[ vot$term < 4 | vot$term >  7,4 ] <- NA  #    alcantar  4
IsCouncilor[ vot$term < 4 | vot$term >  6,5 ] <- NA  #    glezluna  5
IsCouncilor[ vot$term < 4 | vot$term >  5,6 ] <- NA  #      latapi  6
IsCouncilor[ vot$term < 4 | vot$term >  6,7 ] <- NA  # lopezflores  7
IsCouncilor[ vot$term < 4 | vot$term >  5,8 ] <- NA  #     morales  8
IsCouncilor[ vot$term < 4 |(vot$term >  7 & vot$term!=12),9 ] <- NA  # sanchez  9
IsCouncilor[ vot$term < 6 | vot$term > 10,10] <- NA  #      valdes 10
IsCouncilor[ vot$term < 6 | vot$term > 14,11] <- NA  #       banos 11
IsCouncilor[ vot$term < 6 | vot$term > 14,12] <- NA  #       nacif 12
IsCouncilor[ vot$term < 7 | vot$term > 10,13] <- NA  #    elizondo 13
IsCouncilor[ vot$term < 7 | vot$term > 10,14] <- NA  #    figueroa 14
IsCouncilor[ vot$term < 7 | vot$term > 10,15] <- NA  #    guerrero 15
IsCouncilor[ vot$term < 9 | vot$term > 15,16] <- NA  #     cordova 16
IsCouncilor[ vot$term < 9 | vot$term >  9,17] <- NA  #  garcia rmz 17
IsCouncilor[ vot$term < 9 | vot$term > 11,18] <- NA  #      marvan 18
####################################################
## vector to select members present in estimation ##
####################################################
sel.members <- which(ids$name %in% name)
IsCouncilor <- IsCouncilor[, sel.members]


# Round 1 ideal points to anchor ideological space
# (later entrants at party mean)
#                 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#                 u  a  a  a  g  l  l  m  s  v  b  n  e  f  g  c  g  m  a  f  g  m  r  s  s  
#                 g  l  n  l  l  a  p  o  a  a  a  a  l  i  u  o  a  a  n  a  a  u  u  n  a
#                 a  b  d  c  z  t  z  r  n  l  ñ  c  i  g  e  r  r  r  d  v  l  r  i  m  n
#                 l  o  r  a  l  a  f  a  c  d  o  i  z  u  r  d  c  v  r  e  i  a  z  a  t
x.location <-   c(0, 0, 0, 2, 0, 0, 0, 0,-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)[sel.members] # sel = members in estimation
x.precision  <- c(1, 1, 1, 4, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)[sel.members]
#x.location <-   c(1, 0, 0, 2,-2, 0, 0, 2,-2, 0, 2,-1,-2, 0, 2,-2, 2,-2, 0, 0, 0, 2, 0, 0, 0)[sel.members] # sel = members in estimation
#x.precision  <- c(4, 1, 1, 4, 4, 4, 1, 4, 4, 1, 4, 4, 4, 1, 4, 4, 4, 4, 1, 1, 1, 4, 1, 1, 1)[sel.members]
# x.location and x.precision are manipulated by loop, keep a version to use as prior for new entrants instead of party mean
x.prior.location <-  x.location
x.prior.precision <- x.precision
#
window.results <- list () ## WILL ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
partyPlacement <- rep (NA,J)
x.mean <- numeric ()
x.tau  <- numeric ()


for (s in 1:S){        # <= loop over S windows
    #
    #s <- 1 # debug
    councilor.in <- apply (IsCouncilor[inicio[s]:final[s],], 2, invalid)
    councilors   <- name [councilor.in==FALSE] # window's info
    abbrev       <- column [councilor.in==FALSE] # window's info
    sponsors     <- party[councilor.in==FALSE] # window's info
    #
    for (c in 1:J){ # does it for all councilors whther or not present in window (hence J)
# 5mar21: this uses partyPlacement for new entrants
#        x.mean[c] <- ifelse (!is.na(x.location[c]),  x.location[c],  partyPlacement[sponsors[c]])
#        x.tau[c]  <- ifelse (!is.na(x.precision[c]), x.precision[c], 4)
# 5mar21: this uses fixed x0 prior for new entrants
        x.mean[c] <- ifelse (!is.na(x.location [c]), x.location [c], x.prior.location [c])
        x.tau[c]  <- ifelse (!is.na(x.precision[c]), x.precision[c], x.prior.precision[c])
    }
    #
    v <- vs[inicio[s]:final[s], 1:J][, councilor.in==FALSE]; ## EXTRACT 30 VOTES EACH TIME
    v <- as.data.frame(t(v))        ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
    #
    ###################
    ## Vectorization ##
    ###################
    #J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
    member.index  <- 1:nrow(v)
    vote.index <- 1:ncol(v)
    
    ## Melt RC
    rc <- as.data.frame (v)
    colnames (rc) <- vote.index
    rc$member <- member.index
    #
    # determine north/south indexes in vote window
    north <- which(rownames(rc)=="alcantar")
    north <- ifelse (length(north)==0, NA, north) # NA if member absent, else her index 
    south <- which(rownames(rc)=="sanchez")
    south <- ifelse (length(south)==0, NA, south) # NA if member absent, else her index 
    #
    molten.rc <- reshape2::melt(rc, id.vars="member", variable.name="vote", value.name="rc")
    #molten.rc$rc <- car::recode (molten.rc$rc, "0=NA")
    molten.rc <- na.omit (molten.rc)
    #molten.rc$rc <- car::recode (molten.rc$rc, "2=0; c(3,4,5)=NA")
    #
    ife.data.vector <- dump.format(list(
        y         = molten.rc$rc
      , n.members = max(member.index)
      , n.item    = max(vote.index)
      , n.obs     = nrow(molten.rc)
      , vote      = molten.rc$vote
      , member    = molten.rc$member
      , north     = north
      , south     = south
      , x.mean    = x.mean
      , x.tau     = x.tau
      , sponsors  = sponsors
    ))
    #
    ife.parameters = c("theta", "alpha", "beta", "partyPos", "deviance")
    #
    ife.inits <- function() {
        dump.format(
            list(
                theta = rnorm(max(member.index))
                #theta = c(rnorm(3), NA, rnorm(4), NA, rnorm(max(member.index)-9)) # NAs sólo en caso de spike priors, correcto?
              , alpha = rnorm(max(vote.index))
              , beta = rnorm(max(vote.index))
              , '.RNG.name'="base::Wichmann-Hill"
              , '.RNG.seed'= 1971)   #randomNumbers(n = 1, min = 1, max = 1e+04,col=1))
        )
    }
    #
    ife.model.v <- run.jags(
        model   = ife.vector,
        monitor = ife.parameters,
        method  = "parallel",
        data    = ife.data.vector,
        inits   = list (ife.inits(), ife.inits()),
        n.chains=2, thin=50, burnin=10000, sample=200,
        #n.chains=1, thin=5,  burnin=200,   sample=3,
        check.conv=FALSE, plots=FALSE
    )
    #
    chainsIFE.v <- mcmc.list(list (ife.model.v$mcmc[[1]], ife.model.v$mcmc[[2]])) 
    gelman.diag (chainsIFE.v, multivariate=F)
    #
    Alpha.v <- rbind ( chainsIFE.v[[1]][,grep("alpha", colnames(chainsIFE.v[[1]]))]
                    , chainsIFE.v[[2]][,grep("alpha", colnames(chainsIFE.v[[2]]))])
    Beta.v <- rbind ( chainsIFE.v[[1]][,grep("beta", colnames(chainsIFE.v[[1]]))]
                   , chainsIFE.v[[2]][,grep("beta", colnames(chainsIFE.v[[2]]))])
    Theta.v <- rbind ( chainsIFE.v[[1]][,grep("theta", colnames(chainsIFE.v[[1]]))]
                    , chainsIFE.v[[2]][,grep("theta", colnames(chainsIFE.v[[2]]))])
    #
    plot (colMeans (Alpha.v))  # difficulties
    plot (colMeans (Beta.v))   # signal
    #
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
                , col=ids$color[i], lwd=3)
    }
	# ADD COUNCILOR NAMES AND VOTE INFO TO RESULTS OBJECT
        results <- c(results, councilors=list(councilors), abbrev=list(abbrev));
        results <- c(results, folio.date=list(vot[s,c("folio","dy","mo","yr")])); # add vote on which window is centered
        window.results <- c(window.results, list(results)); ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
#        results[[4]] <- GHconv; rm (GHconv)
#
	# Update location of ideal point at time s, to be used as location prior at time s+1
	x.location  <- rep (NA, J)
	x.precision <- rep (100, J)
#	locs <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
#	partyPlacement <- apply( rbind (results[[1]]$BUGSoutput$sims.list$partyPos, results[[2]]$BUGSoutput$sims.list$partyPos), 2, median)
	locs <- apply( results$BUGSoutput$sims.list$x, 2, median)
	partyPlacement <- apply( results$BUGSoutput$sims.list$partyPos, 2, median)
	for (n in 1:J){
		if (length( which(councilors==name[n]) )==0) {               # if councilor not member current round
			x.location[n] <- NA                                  # then prior for next round set to NA
			x.precision[n] <- NA                                 # (and line above sets it to party placement)
		}
		else { x.location[n] <-  locs[which (councilors==name[n])] } # councilor's prior for next round is current x 
	}
	# Precision prior is always constant at 100, implying standard deviation = sqrt (1/100) = 0.1
}  # <---   END OF LOOP OVER WINDOWS


# plot results
tit <- "Terms 67 (Valdés 2007-2010), Bonica method"
source("../code/plot-posteriors.r")
x

# rename object with posterior sims
summary(window.results[[190]])
window.results.67 <- window.results
rm(window.results)

# clean
ls()
rm(c, s, n, v, sel, ife.inits, ife.parameters, ife.data)
rm(councilors, sponsors, inicio, final, councilor.in)
rm(x.location, x.mean, x.precision, x.tau, item, results, item.date)
rm(color, column, locs, name, party, partyPlacement)

# save
summary(window.results.67)
#summary(window.results[[232]]) # 11 members, overlap
save.image(file = "posterior-samples/vald67-window-results-compress.RData", compress = "xz")
#save(window.results.23, file = "posterior-samples/wold23-window-results-compress.RData")
x



# Save window.results, containing all chains from all runs
# save (window.results, file="DynUgaldeBonicaMarch21.RData") # With more anchors
# save (window.results, file="DynUgaldeBonicaMarch19.RData")
# save (window.results, file="DynUgaldeBonica.RData")

source("http://rtm.wustl.edu/code/sendEmail.R")
#sendEmail (subject="Ugalde et al esta listo", text="", address="rosas.guillermo@gmail.com")
sendEmail (subject="Ugalde et al esta listo", text="", address="emagar@gmail.com")

# RData file with runs carried out in Mexico, early March
# load ("DynUgaldeBonica.RData")

# RData file with runs carried out in Wash U, March 19
# These runs omit non-sitting Councilors and party precisions for new Councilors
load ("DynUgaldeBonicaMarch21.RData")




S <- length (semester.results)
multiGelman.hat <- numeric ()
for (i in 1:S){
	chainsConv <- mcmc.list(list (as.mcmc (semester.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (semester.results[[i]][[1]]$BUGSoutput$sims.list$x)))
	tmp <- gelman.diag (chainsConv)[[2]]
	multiGelman.hat <- c(multiGelman.hat, tmp)
}
rm (tmp, chainsConv)

nonConverged <- ifelse (multiGelman.hat > 2, 1, 0)  # Should be 0


CouncilorIn <- matrix (1, nrow=J, ncol=S)
CouncilorIn[1,  all45678901$date > 20071217] <- NA
CouncilorIn[2,  all45678901$date > 20080814] <- NA
CouncilorIn[3,  all45678901$date > 20101027] <- NA
CouncilorIn[4,  all45678901$date > 20101027] <- NA
CouncilorIn[5,  all45678901$date > 20080814] <- NA
CouncilorIn[6,  all45678901$date > 20071217] <- NA
CouncilorIn[7,  all45678901$date > 20080814] <- NA
CouncilorIn[8,  all45678901$date > 20071217] <- NA
CouncilorIn[9,  all45678901$date > 20101027] <- NA
CouncilorIn[10,  all45678901$date < 20080215] <- NA
CouncilorIn[11,  all45678901$date < 20080215] <- NA
CouncilorIn[12,  all45678901$date < 20080215] <- NA
CouncilorIn[13,  all45678901$date < 20080829] <- NA
CouncilorIn[14,  all45678901$date < 20080829] <- NA
CouncilorIn[15,  all45678901$date < 20080829] <- NA
#CouncilorIn[16,  all45678901$date < 20111215] <- NA
#CouncilorIn[17,  all45678901$date < 20111215] <- NA
#CouncilorIn[18,  all45678901$date < 20111215] <- NA



# If using DynUgaldeBonica.RData, use the following code to extract ideal points
ideal.points <- matrix (NA, nrow=S, ncol=J)
for (i in 1:S){
	ideal.points[i,] <- apply (rbind (semester.results[[i]][[1]]$BUGSoutput$sims.list$x, semester.results[[i]][[2]]$BUGSoutput$sims.list$x), 2, median)
}

# If using DynUgaldeBonicaMarch19.RData or DynUgaldeBonicaMarch21.RData, use the following code to extract ideal points
ideal.points <- matrix (NA, nrow=S, ncol=J)
ideal.points.var <- matrix (NA, nrow=S, ncol=J)
for (i in 1:S){
	for (j in 1:J){
		councilor <- names45678901[j]
		num <- which (semester.results[[i]][[3]]==councilor)
		if ( length (num)==0 ) {
			ideal.points[i,j] <- 1
			ideal.points.var[i,j]  <- 0
		} else {
				ideal.points[i,j] <- median (c (semester.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], semester.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]))
				ideal.points.var[i,j]  <- var (c (semester.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], semester.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]))
		}
	}
}


# Plot different individual councilor paths:
# There is definitely a problem at different spots of the estimation:
# Between i=200 and i=500 and after i=600
# The problem really obtains when we get new guys in
# Valdes, Banos, Nacif, Elizondo, Figueroa, Guerrero come in in quick succession and destroy everything
# Maybe the best solution is to break down estimation of this very long period in two or three chunks
# Or maybe we need to combine priors on ideal points with priors on item parameters
k <-  12
plot (ideal.points[ideal.points.var[,k] != 0, k], type="l")


# Get SDs of estimates, the width should be useful to plot thickness of data points
ideal.points.var <- sqrt (ideal.points.var)

# Non-smoothed ideal point time-paths
setwd ("/Users/grosas/Dropbox/ifesharedge/graphs")
pdf ("UgaldeEtAlNonSmooth.pdf", h=7, w=9)
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:J){
# 	lines(ideal.points[1:S,j], lwd=3, col=color45678901[j])
	lines(CouncilorIn[j,1:S] * ideal.points[1:S,j], lwd=3, col=rgb.45678901[j])
}
dev.off()



# Smoothed ideal point time-paths
# pdf ("UgaldeEtAlSmooth.pdf", h=7, w=9)
pdf ("UgaldeEtAlSmoothMarch21.pdf", h=7, w=9)
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:J){
	lines(smooth.spline(c(1:S)[!is.na(CouncilorIn[j,1:S])], ideal.points[!is.na(CouncilorIn[j,1:S]),j], df=10), lwd=3, col=rgb.45678901[j])
# 	lines(smooth.spline(c(1:S), ideal.points[,j], df=10), lwd=3, col=color45678901[j])
}
dev.off()



setwd ("/Users/grosas/Dropbox/ifesharedge/graphs/animBits/ugalde/")

# Smoothed ideal point time-paths, daily change movie
Smooth <- list ()
for (j in 1:J){
	Smooth[[j]] <- smooth.spline(c(1:S), ideal.points[,j], df=10)
}

for (j in 1:J){
	Smooth[[j]]$y[is.na(CouncilorIn[j,1:S])] <- NA
}

snapshot <- seq (1, 1091, by=10)
for (s in snapshot){
	which.s <- which (snapshot==s)
	if (which.s < 10) {name = paste('Ugalde00', which.s,'plot.png',sep='')}
	if (which.s >= 10 && which.s < 100) {name = paste('Ugalde0', which.s,'plot.png', sep='')}
	if (which.s >= 100) {name = paste('Ugalde', which.s,'plot.png', sep='')}

	jpeg (name, quality=100, height=500, width=500)

	plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
	for (j in 1:J){
		lines ( Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s], lwd=6, col=rgb.45678901[j])
	}
	# Eric: Creo que tú tienes más información para hacer los cambios en las siguientes tres líneas
	legend ("topright", bty="n", legend=paste ("vote", s, sep=" ")) #Change the legend for the date of the vote
	# Add vertical lines for elections here, but only for some realizations of S
	# Add names of councilors here as well, but only for some realizations of S
	dev.off()
}

# Make a short film
# system("convert -loop 1 -delay 40 *.png IFEwolden.gif")
system("convert -loop 1 -delay 20 *.png IFEugaldeSofis.gif")
# The number after loop controls the number of automatic replays (0 stands for infinite loop)
# The number after delay determines length of transition between slides)







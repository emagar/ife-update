# Invoca Jags desde R
##
library(arm)
library (MCMCpack)
library (foreign)
library (car)
library (gtools)
library (multicore)


#library (R2WinBUGS)
library (R2jags)
#library(BRugs)

rm(list = ls())
##
#workdir <- c("~/Dropbox/data/rollcall/ife_cg")
workdir <- c("~/Dropbox/ifeSharedGE/data/")
#workdir <- c("d:/01/Dropbox/data/rollcall/ife_cg")
#workdir <- c("d:/01/Dropbox/ifeSharedGE/code")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/ife_cg")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/ife_cg")
setwd(workdir)
##
#set.seed(1970)

## names.1 <- c("Woldenberg", "segob", "senpri", "senprd", "dippri", "dippan", "Creel", "Granados", "Zertuche", "Pinchetti", "Pozas")
## names.2 <- c("Woldenberg", "Barrag?n", "Cant?", "C?rdenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebad?a")
## names.3 <- c("Woldenberg", "Barrag?n", "Cant?", "C?rdenas", "Lujambio", "Merino", "Peschard", "Rivera", "Luken")
names.23 <- c("Woldenberg", "Barrag?n", "Cant?", "C?rdenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebad?a", "Rivera", "Luken")
## names.4 <- c("Ugalde", "Albo", "Andrade", "Alc?ntar", "Glez. Luna", "Latapi", "L?pez Flores", "Morales", "S?nchez")
## names.5 <- c("Albo", "Andrade", "Alc?ntar", "Glez. Luna", "Latapi", "L?pez Flores", "Morales", "S?nchez")
## names.6 <- c("Vald?s", "Albo", "Andrade", "Alc?ntar", "Ba?os", "Glez. Luna", "L?pez Flores", "Nacif", "S?nchez")
## names.7 <- c("Vald?s", "Andrade", "Alc?ntar", "Ba?os", "Elizondo", "Figueroa", "Guerrero", "Nacif", "S?nchez")
## names.8 <- c("Vald?s", "Ba?os", "Elizondo", "Figueroa", "Guerrero", "Nacif")
## names.9 <- c("Vald?s", "Ba?os", "Elizondo", "Figueroa", "Guerrero", "Nacif", "Marv?n", "C?rdova", "Garc?a Ram?rez")
## names.4567 <- c("Ugalde", "Albo", "Andrade", "Alc?ntar", "Glez. Luna", "Latapi", "L?pez Flores", "Morales", "S?nchez", "Vald?s", "Ba?os", "Nacif", "Elizondo", "Figueroa", "Guerrero")
## names.45678 <- c("Ugalde", "Albo", "Andrade", "Alc?ntar", "Glez. Luna", "Latapi", "L?pez Flores", "Morales", "S?nchez", "Vald?s", "Ba?os", "Nacif", "Elizondo", "Figueroa", "Guerrero")
names.456789 <- c("Ugalde", "Albo", "Andrade", "Alc?ntar", "Glez. Luna", "Latapi", "L?pez Flores", "Morales", "S?nchez", "Vald?s", "Ba?os", "Nacif", "Elizondo", "Figueroa", "Guerrero", "Marv?n", "C?rdova", "Garc?a Ram?rez")

color.23 <- c("red", "gold", "gold", "gold",  "blue", "red", "blue", "red", "gold", "red", "blue")
rgb.23<-matrix(NA,11,3)
rgb.23[1,]<- c(1,0,0) #red
rgb.23[2,]<- c(1,215/255,0) #gold
rgb.23[3,]<- c(1,215/255,0) #gold
rgb.23[4,]<- c(1,215/255,0) #gold
rgb.23[5,]<- c(0,0,1) #blue
rgb.23[6,]<- c(1,0,0) #red
rgb.23[7,]<- c(0,0,1) #blue
rgb.23[8,]<- c(1,0,0) #red
rgb.23[9,]<- c(1,215/255,0) #gold
rgb.23[10,]<- c(1,0,0) #red
rgb.23[11,]<- c(0,0,1) #blue
#
color.4567 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red")
rgb.4567<-matrix(NA,15,3)
rgb.4567[1,] <- c(1,0,0) #red
rgb.4567[2,] <- c(0,0,1) #blue
rgb.4567[3,] <- c(1,0,0) #red
rgb.4567[4,] <- c(34/255,139/255,34/255) #verde
rgb.4567[5,] <- c(0,0,1) #blue
rgb.4567[6,] <- c(1,0,0) #red
rgb.4567[7,] <- c(1,0,0) #red
rgb.4567[8,] <- c(0,0,1) #blue
rgb.4567[9,] <- c(0,0,1) #blue
rgb.4567[10,]<- c(1,215/255,0) #gold
rgb.4567[11,]<- c(1,0,0) #red
rgb.4567[12,]<- c(0,0,1) #blue
rgb.4567[13,]<- c(0,0,1) #blue
rgb.4567[14,]<- c(1,215/255,0) #gold
rgb.4567[15,]<- c(1,0,0) #red
#
color.45678 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red")
rgb.45678 <- rgb.4567
color.456789 <- c(color.45678, "blue", "gold", "red")
rgb.456789 <- rgb.45678
rgb.456789 <- rbind(rgb.456789, c(0,0,1)) #blue
rgb.456789 <- rbind(rgb.456789, c(1,215/255,0)) #gold
rgb.456789 <- rbind(rgb.456789, c(1,0,0)) #red
#
#c(1,215/255,0) #gold
#c(1,140/255,0) #naranja
#c(0,0,1) #blue
#c(1,0,0) #red
#c(34/255,139/255,34/255) #verde

greys.4567 <- c("grey50", "grey30", "grey50", "grey50", "grey30", "grey50", "grey50", "grey30", "grey30",
                "grey70", "grey50", "grey30", "grey30", "grey70", "grey50")
greyLines.4567 <- matrix(NA, nrow=15, ncol=2)
greyLines.4567[,1] <- c("grey80", "white", "grey80", "grey80", "white", "grey80", "grey80", "white", "white",
                        "black", "grey80", "white", "white", "black", "grey80")
greyLines.4567[,2] <- c(1,1,1,3,1,1,1,1,1,1,1,1,1,1,1)
#
#  area    lines   linetype
#"grey30" "white"    1 #pan
#"grey50" "grey80"   1 #pri
#"grey70" "black"    1 #prd
#"grey70" "black"    3 #pt
#"grey50" "grey80"   3 #pvem

#all1 <-read.csv("tmp1.out",  header=TRUE)
#all2 <-read.csv("tmp2.out",  header=TRUE)
#all3 <-read.csv("tmp3.out",  header=TRUE)
all23 <-read.csv("tmp23.csv",  header=TRUE)
#all4 <-read.csv("tmp4.out",  header=TRUE)
#all4.1 <- all4[1:145,]      # votos hasta julio 2006
#all4.2 <- all4[146:188,]    # votos despues de julio 2006
#all4.3 <- all4[1:167,]      # votos hasta agosto 2007, antes de la reforma electoral
#all5 <-read.csv("tmp5.out",  header=TRUE)
#all6 <-read.csv("tmp6.out",  header=TRUE)
#all7 <-read.csv("tmp7.out",  header=TRUE)
#all4567 <-read.csv("tmp4567.out",  header=TRUE)
#all45678 <-read.csv("tmp45678.out",  header=TRUE)
#all456789 <-read.csv("tmp456789.out",  header=TRUE)

# sort by date and add session counter
tmp <- all23
xx <- rep(0, nrow(tmp)); xx[1] <- 1;
tmp <- tmp[order(tmp$yr, tmp$mo, tmp$dy, tmp$folio),]
tmp$sess <- rep(0, nrow(tmp)); tmp$date <- tmp$yr*10000+tmp$mo*100+tmp$dy
tmp$sess[1] <- 1
for (i in 2:nrow(tmp)){
  tmp$sess[i] <- ifelse(tmp$date[i]==tmp$date[i-1], tmp$sess[i-1], tmp$sess[i-1]+1);
  xx[i] <- ifelse(tmp$date[i]==tmp$date[i-1], 0, 1);
}
all23 <- tmp
## tmp <- all456789
## xxx <- rep(0, nrow(tmp)); xxx[1] <- 1;
## tmp <- tmp[order(tmp$yr, tmp$mo, tmp$dy, tmp$folio),]
## tmp$sess <- rep(0, nrow(tmp)); tmp$date <- tmp$yr*10000+tmp$mo*100+tmp$dy
## tmp$sess[1] <- 1
## for (i in 2:nrow(tmp)){
##   tmp$sess[i] <- ifelse(tmp$date[i]==tmp$date[i-1], tmp$sess[i-1], tmp$sess[i-1]+1)
##   xxx[i] <- ifelse(tmp$date[i]==tmp$date[i-1], 0, 1);
## }
## all456789 <- tmp
rm(tmp)

## Create object with session traits (choose one)
library(lubridate)
tmp <- all23
nv <- as.numeric(table(tmp$date)); tmp <- tmp[xx==1,]
tmp2 <- rep(NA, max(tmp$sess))
sess.dat <- data.frame(sess=1:max(tmp$sess), yr=tmp2, mo=tmp2, dy=tmp2, date=tmp2, nvot=tmp2, term=tmp2)
sess.dat$yr <- tmp$yr; sess.dat$mo <- tmp$mo; sess.dat$dy <- tmp$dy; sess.dat$date <- tmp$date; sess.dat$term <- tmp$term
sess.dat$nvot <- nv; sess.dat$term <- tmp$term;
sess.dat$date <- ymd(sess.dat$date)
###
## tmp <- all456789
## nv <- as.numeric(table(tmp$date)); tmp <- tmp[xxx==1,]
## tmp2 <- rep(NA, max(tmp$sess))
## sess.dat <- data.frame(sess=1:max(tmp$sess), yr=tmp2, mo=tmp2, dy=tmp2, date=tmp2, nvot=tmp2, term=tmp2)
## sess.dat$yr <- tmp$yr; sess.dat$mo <- tmp$mo; sess.dat$dy <- tmp$dy; sess.dat$date <- tmp$date; sess.dat$term <- tmp$term
## sess.dat$nvot <- nv; sess.dat$term <- tmp$term;
## sess.dat$date <- ymd(sess.dat$date)
## rm(xx,xxx,tmp,tmp2,nv)
##
S <- max(sess.dat$sess)

##################################################
###     DIN?MICA DE WOLDENBERGS EST?TICOS      ###
##################################################

## MODEL
model1Dj.irt <- function() {
for (j in 1:J){                ## loop over respondents
    for (i in 1:I){              ## loop over items
      v[j,i] ~ dbern(p[j,i]);                                 ## voting rule
      probit(p[j,i]) <- mu[j,i];                              ## sets 0<p<1 as function of mu
#      mu[j,i] <- beta[i]*theta[j] - alpha[i];                 ## utility differential
      mu[j,i] <- signal[i]*x[j] - difficulty[i];                            ## utility differential
                  }
                }
## ESTO LO PUEDO SACAR POST ESTIMACION
#  for (i in 1:I){
#  m[i] <- difficulty[i] / signal[i]  ## cutpoint
#  }
  ## priors ################
    for (j in 1:J){
        x[j] ~ dnorm (mean.x[j], tau.x[j]);
                  }
    for (i in 1:I){
        signal[i] ~ dnorm(0, 0.1);
        difficulty[i] ~ dnorm(0, 0.25);
                 }
}
#end model##############

## Create objects for prior info
I <- nrow(all23); J <- 11;  ## Total members and items in all sessions that will be analyzed
tmp <- data.frame ( matrix(NA, nrow = S, ncol = J) ); colnames(tmp) <- names.23
priors <- list(mean.x=tmp, tau.x=tmp)
##
## INPUT PRIORS FOR s=1 HERE (x0)
s <- 1
#priors$mean.x[s,1]  <- 1  ; priors$tau.x[s,1]  <- 4   #Woldenberg   # Is this precise prior needed?
priors$mean.x[s,1]  <- 0  ; priors$tau.x[s,1]  <- 1   #Woldenberg (GR)
priors$mean.x[s,2]  <- 2  ; priors$tau.x[s,2]  <- 4   #barragan
priors$mean.x[s,3]  <- 0  ; priors$tau.x[s,3]  <- 1   #cantu
priors$mean.x[s,4]  <- -2 ; priors$tau.x[s,4]  <- 4   #cardenas
priors$mean.x[s,5]  <- 0  ; priors$tau.x[s,5]  <- 1   #lujambio
priors$mean.x[s,6]  <- 0  ; priors$tau.x[s,6]  <- 1   #merino
priors$mean.x[s,7]  <- 0  ; priors$tau.x[s,7]  <- 1   #molinar
priors$mean.x[s,8]  <- 0  ; priors$tau.x[s,8]  <- 1   #peschard
priors$mean.x[s,9]  <- 0  ; priors$tau.x[s,9]  <- 1   #zebadua
priors$mean.x[s,10] <- 0  ; priors$tau.x[s,10] <- 1   #rivera
priors$mean.x[s,11] <- 0  ; priors$tau.x[s,11] <- 1   #luken


new.prior.mean <- c(0,2,0,-2,rep(0,7))
new.prior.tau <- c(1,4,1,4,rep(1,7))


## JAGS VERSION
s <- 1
mean.x <- rep(NA, J); tau.x <- rep(NA, J) ## PREPARE OBJECTS FOR SESSION'S x PRIORS
for (j in 1:J){
  mean.x[j] <- priors$mean.x[s,j];        ## EXTRACT x PRIORS FOR JAGS CODE
  tau.x[j] <- priors$tau.x[s,j];
              }
##
v <- all23[all23$sess==s,1:11]; ## EXTRACT VOTES
v[v==0] <- NA; v[v==-1] <- 0    ## Versi?n probit requiere 0s y 1s
v <- t(v)                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
##
print(cat("\tSession no.",s,"of",S,", with", I, "votes \n"))
##
ife.data <- list ("J", "I", "v", "mean.x", "tau.x")
ife.inits <- function (){
    list (
    x=rnorm(J),
    signal=rnorm(I),
    difficulty=rnorm(I)
    )
    }
ife.parameters <- c("x", "signal", "difficulty")#, "deviance")



#test ride to see program works
start.time <- proc.time()
results <- jags (data=ife.data, inits=ife.inits, ife.parameters,
                 model.file=model1Dj.irt, n.chains=2,
                n.iter=100, n.thin=10
                 )
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")); rm(time.elapsed)

#longer run
start.time <- proc.time()
results <- jags (data=ife.data, inits=ife.inits, ife.parameters,
                 model.file=model1Dj.irt, n.chains=2,
                n.iter=20000, n.burnin=10000, n.thin=100
                 )
time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)

sessions.results <- list (results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
# jotas <- array(data=NA, dim = c(S,3,J)); dimnames(jotas)[2] <- list(c("q025","q5","q975")); dimnames(jotas)[3] <- list(names.23)
# for (j in 1:J){
#   jotas[s,,j] <- quantile (results$BUGSoutput$sims.list$x[,j], probs=c(.025,.5,.975), names=F) ## ADD IDEAL POINT SUMMARY TO OBJECT
#               }
# 
## ADD PRIORS FOR NEXT SESSION
priors$mean.x[s+1,] <- results$BUGSoutput$median$x
priors$tau.x[s+1,] <- 1 # 1/results$BUGSoutput$sd$x^2   (GR)
## Exceptions for Rivera and Luken ( first session is ymd(20001211) )
priors$mean.x[s+1,10] <- ifelse((sess.dat$date[s]<ymd(20001211))=="TRUE", 0, results$BUGSoutput$median$x[10])
priors$mean.x[s+1,11] <- ifelse((sess.dat$date[s]<ymd(20001211))=="TRUE", 0, results$BUGSoutput$median$x[11])
priors$tau.x[s+1,10] <- 1 #ifelse((sess.dat$date[s]<ymd(20001211))=="TRUE", 1, 1/results$BUGSoutput$sd$x[10]^2)   (GR)
priors$tau.x[s+1,11] <- 1 #ifelse((sess.dat$date[s]<ymd(20001211))=="TRUE", 1, 1/results$BUGSoutput$sd$x[11]^2)   (GR)
## Exceptions for Molinar and Zebadua ( last session is ymd(20001114) )


## AQUI UN DIAGNOSTICO DE CONVERGENCIA QUE PODAMOS GUARDAR SISTEMATICAMENTE
# results.mcmc <- as.mcmc(results)
# library(superdiag)
# superdiag(results.mcmc) ## NO JALA
# Si mal no recuerdo, generalmente se atora cuando hay problemas con gelman.diag()


inicio <- c ( 1:543 )
final  <- c ( 40:582)
## LOOP FOR REMAINDER OF SESSIONS
s <- 2
#S <- length(final)
for (s in 2:S){        # <= BIG FUNCTION STARTS
mean.x <- rep(NA, J); tau.x <- rep(NA, J) ## PREPARE OBJECTS FOR SESSION'S x PRIORS
for (j in 1:J){
#   mean.x[j] <- priors$mean.x[s,j];        ## EXTRACT x PRIORS FOR JAGS CODE
#   tau.x[j] <- 1 # priors$tau.x[s,j];     (GR)
	mean.x[j] <- new.prior.mean[j]
	tau.x[j]  <- new.prior.tau[j]
}
##
v <- all23[inicio[s]:final[s],1:11]; ## EXTRACT VOTES
#v <- all23[all23$sess==s,1:11]; ## EXTRACT VOTES
v[v==0] <- NA; v[v==-1] <- 0    ## Versi?n probit requiere 0s y 1s
v <- t(v)                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
##
print(cat("\tSession no.",s,"of",S,", with", I, "votes \n"))
##
ife.data <- list ("J", "I", "v", "mean.x", "tau.x")
ife.inits <- function (){
    list (
    x=rnorm(J),
    signal=rnorm(I),
    difficulty=rnorm(I)
    )
    }
ife.parameters <- c("x", "signal", "difficulty")   #, "deviance")

## ##test ride to see program works
## start.time <- proc.time()
## results <- jags (data=ife.data, inits=ife.inits, ife.parameters,
##                  model.file=model1Dj.irt, n.chains=2,
##                 n.iter=100, n.thin=10
##                  )
## time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
## print(cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")); rm(time.elapsed)

#longer run
start.time <- proc.time()

	results <- mclapply(1:2, function(x) {
		model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
								   model.file=model1Dj.irt, n.chains=1,
								   n.iter=20000, n.burnin=10000, n.thin=100
		))
		if(inherits(model.jags.re,"try-error")) {return()}
		return(model.jags.re)
	}, mc.cores=2 )
time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)

# results <- jags (data=ife.data, inits=ife.inits, ife.parameters,
# 							 model.file=model1Dj.irt, n.chains=2,
# 							 n.iter=20000, n.burnin=10000, n.thin=100
# 			)
# 
# time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
# print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)

sessions.results[length(sessions.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
# for (j in 1:J)
# {
#   jotas[s,,j] <- quantile (results$BUGSoutput$sims.list$x[,j], probs=c(.025,.5,.975), names=F) ## ADD IDEAL POINT SUMMARY TO OBJECT
# }

## ADD PRIORS FOR NEXT SESSION
# priors$mean.x[s+1,] <- results$BUGSoutput$median$x
# priors$tau.x[s+1,] <- 1/results$BUGSoutput$sd$x^2 ## MULTIPLIED BY 10 TO DROP AUTOREGRESSIVE VOLATILITY
# ## Exceptions for Rivera and Luken ( first session is ymd(20001211) )
# priors$mean.x[s+1,10] <- ifelse((sess.dat$date[s]<ymd(20001211))=="TRUE", 0, results$BUGSoutput$median$x[10])
# priors$mean.x[s+1,11] <- ifelse((sess.dat$date[s]<ymd(20001211))=="TRUE", 0, results$BUGSoutput$median$x[11])
# priors$tau.x[s+1,10] <- ifelse((sess.dat$date[s]<ymd(20001211))=="TRUE", 1, 1/results$BUGSoutput$sd$x[10]^2)
# priors$tau.x[s+1,11] <- ifelse((sess.dat$date[s]<ymd(20001211))=="TRUE", 1, 1/results$BUGSoutput$sd$x[11]^2)
## Exceptions for Molinar and Zebadua ( last session is ymd(20001114) )

## AQUI UN DIAGNOSTICO DE CONVERGENCIA QUE PODAMOS GUARDAR SISTEMATICAMENTE
#results.mcmc <- as.mcmc(results)
#library(superdiag)
#superdiag(results.mcmc) ## No jala
}   # <= BIG FUNCTION ENDS





save (sessions.results, file="SessionsResults.RData")

sendEmail (subject="Listo", text="Listo, listo", address="emagar@itam.mx")
sendEmail (subject="Listo", text="Listo, listo", address="grosas@wustl.edu")

######################################
# Quick plots
######################################


load ("SessionsResults.RData")
for (i in 2:S){
	priors$mean.x[i,] <- sessions.results[[i]][[2]]$BUGSoutput$median$x
}


j <- 1
plot(c(1:S), priors$mean.x[1:S,j], main=names.23[j], ylim=c(-2,2), type="n", xlab="", ylab="Ideal points")
for (j in 1:11){
 lines(smooth.spline(c(2:S), priors$mean.x[2:S,j], df=10), lwd=3, col=color.23[j])
}

# Less elegant plot to also look at precision of estimation (GR)
plot(sess.dat$date, priors$mean.x[2:76,j], main=names.23[j], ylim=c(-2,2), pch=19, cex=0.3)
segments (x0=sess.dat$date, y0=priors$mean.x[2:76,j]+1.96*(1/priors$tau.x[2:76,j]), x1=sess.dat$date, y1=priors$mean.x[2:76,j]-1.96*(1/priors$tau.x[2:76,j]))


#########################################################################################################
#########################################################################################################
# What's happening here is a version of the mistake that I made in the governors paper.   (GR)
# Assume the following sequence of votes:
# 1,1,1,0,0,1,0,1,0,1,1,1,1,0,1,0,1,1....
# This is an individual that will be on the right.
# The procedure is doing the following:
# It takes the first x votes and estimates an ideal point.  The posterior becomes the new prior, and it carries information contained in the first x votes
# It then takes votes x+1 through x+y and does the same thing.
# We will always see Councilors converging toward the point estimated on the entire voting record, even if there is no dynamic position change.
# It is an artifact of the model
# We need to find a way of only considering information from the most immediate votes (ie., we need a moving window, a la Bonica), which probably means renouncing to the comfort of precisely-estimated ideal points
# Given the artifactual nature of the result, it is remarkable that Cardenas and Barragan look so crazy.
# The step that I've tried here is to never update the precision of the estimate.  This is obviously not great
# The next step would be to really consider moving averages, not by dates, but by votes.
# There are 582 votes in 76 days, for an average of 7.7 votes per day.
# Why don't we build 582-10 overlapping vote matrices with 10 votes each and perform 582-10 different estimations only using the posterior mean from the previous window?
#########################################################################################################
#########################################################################################################

min.x <- min(sess.dat$date); max.x <- max(sess.dat$date); min.y <- min(priors$mean.x[2:76,1:11]); max.y <- max(priors$mean.x[2:76,1:11])
plot(c(min.x, max.x), c(min.y, max.y), xlab="session date", ylab="ideal point       ", type="n")
for (j in 1:J){
    lines(smooth.spline(sess.dat$date, priors$mean.x[2:76,j], df=10), lwd=3, col=color.23[j])
}

## #TODOS LOS CONSEJEROS EN COLOR
## plot(-2:11,c(rep(-.8,times=13),.9),type="n",xlab="semester",
##     ylab="ideal point       ",
##     xaxp=c(1,8,7), yaxp=c(-.6,.6,6), axes="FALSE", main="Period III")
## axis(1, at=c(1:8), labels = FALSE)
## axis(1, tick= FALSE, cex.axis=.55, at=c(1:8), labels = rep(1:2,4), line=-0.8)
## axis(1, tick= FALSE, cex.axis=.8, at=c(1.5,3.5,5.5,7.5), labels = 2004:2007 )
## axis(2, at=c(-.8,-.6,-.4,-.2,0,.2,.4,.6,.8), labels = FALSE)
## axis(2, tick= FALSE, cex.axis=.8, at=c(-.8,-.6,-.4,-.2,0,.2,.4,.6,.8), labels = TRUE )
## #lines(c(2,2),c(.9,-.9),lwd=1,lty=3, col="grey50")
## #lines(c(8,8),c(.9,-.9),lwd=1,lty=3, col="grey50")
## #lines(c(14,14),c(.9,-.9),lwd=1,lty=3, col="grey50")
## #lines(c(8.5,8.5),c(.9,-.9),lwd=1,lty=3, col="grey50")
## #text(c(8.5,8.5),c(.9,.85),c("Relevo ","parcial "), adj=1, cex=.5, col="grey50")
## ###text(1,med.alcantar[1],c("Gmz. Alc?ntar"), pos=2, cex=.8, col="black")
## ###text(1,med.morales[1],c("Morales"), pos=2, cex=.8, col="black")
## ###text(1,med.andrade[1],c("Andrade"), pos=2, cex=.8, col="black")
## ###text(1,med.lpzFlores[1]+.025,c("Lpz. Flores"), pos=2, cex=.8, col="black")
## ###text(1,med.ugalde[1]+.025,c("Ugalde"), pos=2, cex=.8, col="black")
## ###text(1,med.latapi[1],c("Latapi"), pos=2, cex=.8, col="black")
## ###text(1,med.albo[1]-.025,c("Albo"), pos=2, cex=.8, col="black")
## ###text(1,med.sanchez[1],c("S?nchez"), pos=2, cex=.8, col="black")
## ###text(1,med.glezLuna[1],c("Glz. Luna"), pos=2, cex=.8, col="black")
## ##lines(c(1,1.4),c(med.woldenberg[1],med.woldenberg[1]+.27),lwd=1,lty=2, col="grey70")
## ##lines(c(1.2,1.5),c(med.peschard[1],med.woldenberg[1]+.2),lwd=1,lty=2, col="grey70")
## ##lines(c(1.3,1.6),c(med.merino[1],med.woldenberg[1]+.13),lwd=1,lty=2, col="grey70")
## ##lines(c(11,11.5),c(med.rivera[11],med.rivera[11]+.2),lwd=1,lty=2, col="grey70")
## ##lines(c(14,13.5),c(med.luken[14],med.luken[14]-.07),lwd=1,lty=2, col="grey70")
## ##lines(c(3,4),c(med.lujambio[3],-.3),lwd=1,lty=2, col="grey70")
## ##lines(c(6,6.1),c(med.zebadua[6],-.1),lwd=1,lty=2, col="grey70")
## ##text(1.3,med.woldenberg[1]+.27,c("Woldenberg"), pos=4, adj=0, cex=.8, col="black")
## ##text(1.4,med.woldenberg[1]+.2,c("Peschard"), pos=4, adj=0, cex=.8, col="black")
## ##text(1.5,med.woldenberg[1]+.13,c("Merino"), pos=4, adj=0, cex=.8, col="black")
## ##text(11,.7,c("Rivera"), pos=4, adj=0, cex=.8, col="black")
## ##text(12,.3,c("Luken"), pos=4, adj=0, cex=.8, col="black")
## lines(spline(1:8, med.albo), lwd=3, col="blue")
## lines(spline(1:8, med.glezLuna), lwd=3, col="blue")
## lines(spline(1:8, med.morales), lwd=3, col="blue")
## lines(spline(1:8, med.sanchez), lwd=3, col="blue")
## lines(spline(1:8, med.ugalde), lwd=3, col="red")
## lines(spline(1:8, med.latapi), lwd=3, col="red")
## lines(spline(1:8, med.lpzFlores), lwd=3, col="red")
## lines(spline(1:8, med.andrade), lwd=3, col="red")
## lines(spline(1:8, med.alcantar), lwd=3, col="green")
## points(all[2,], pch=17)
## text(6,-0.82,c("*"))



###############################################
###     WOLDENBERG A LA MARTIN-QUINN        ###
##############################################

### MODEL: 11 members and 15 semesters (WOLDENBERG 1y2 juntos) extremist anchors # # # # # #
model1Dj.irt <- function() {
  for (j in 1:J){                ## loop over councilors
    for (i in 1:I){              ## loop over items
      v[j,i] ~ dbern(pi[j,i]);                                 ## voting rule
      probit(pi[j,i]) <- mu[j,i];                              ## sets 0<p<1 as function of mu
      mu[j,i] <- signal[i]*(x[j]) - difficulty[i];
#      mu[j,i] <- signal[i]*(x[j]*p[j]) - difficulty[i];
#       mu[j,i] <- signal[i]*(x1[j]*d1[i]*p1[j] + x2[j]*d2[i]*p2[j]
#                            + x3[j]*d3[i]*p3[j] + x4[j]*d4[i]*p4[j]
#                            + x5[j]*d5[i]*p5[j] + x6[j]*d6[i]*p6[j]
#                            + x7[j]*d7[i]*p7[j] + x8[j]*d8[i]*p8[j]
#                            + x9[j]*d9[i]*p9[j] + x10[j]*d10[i]*p10[j]
#                            + x11[j]*d11[i]*p11[j] + x12[j]*d12[i]*p12[j]
#                            + x13[j]*d13[i]*p13[j] + x14[j]*d14[i]*p14[j]) - difficulty[i];     ## utility differential
                }
#       x1[j] ~ dnorm (x0[j],50);  # todos tienen varianza .1 en slack
#       x2[j] ~ dnorm (x1[j],50);  # todos tienen varianza .1 en slack
#       x3[j] ~ dnorm (x2[j],50);  # todos tienen varianza .1 en slack
#       x4[j] ~ dnorm (x3[j],50);  # todos tienen varianza .1 en slack
#       x5[j] ~ dnorm (x4[j],50);  # todos tienen varianza .1 en slack
#       x6[j] ~ dnorm (x5[j],50);  # todos tienen varianza .1 en slack
#       x7[j] ~ dnorm (x6[j],50);  # todos tienen varianza .1 en slack
#       x8[j] ~ dnorm (x7[j],50);  # todos tienen varianza .1 en slack
#       x9[j] ~ dnorm (x8[j],50);  # todos tienen varianza .1 en slack
#       x10[j] ~ dnorm (x9[j],50);  # todos tienen varianza .1 en slack
#       x11[j] ~ dnorm (x10[j],50);  # todos tienen varianza .1 en slack
#       x12[j] ~ dnorm (x11[j],50);  # todos tienen varianza .1 en slack
#       x13[j] ~ dnorm (x12[j],50);  # todos tienen varianza .1 en slack
#       x14[j] ~ dnorm (x13[j],50);  # todos tienen varianza .1 en slack
              }
  ## for (i in 1:I){
  ##    m[i] <- difficulty[i] / signal[i]                                      ## midpoint
  ## }
  ## priors
for (j in 1:J){
	x[j] ~ dnorm(x.mean[j], x.tau[j]);    #Woldenberg
}
#   	x0[1] ~ dnorm(1, 4);    #Woldenberg
#     x0[2] ~ dnorm(2, 4);    #barragan
#     x0[3] ~ dnorm(0, 1);    #cantu
#     x0[4] ~ dnorm(-2, 4);   #cardenas
#     x0[5] ~ dnorm(0, 1);    #lujambio
#     x0[6] ~ dnorm(0, 1);    #merino
#     x0[7] ~ dnorm(0, 1);    #molinar
#     x0[8] ~ dnorm(0, 1);    #peschard
#     x0[9] ~ dnorm(0, 1);    #zebadua
#     x0[10] ~ dnorm(0, 1);   #rivera
#     x0[11] ~ dnorm(0, 1);   #luken
    for(i in 1:I){
        signal[i] ~ dnorm( 0, 0.1);
                 }
    for(i in 1:I){
        difficulty[i] ~ dnorm( 0, 0.25);
                 }
}
## END MODEL

## JAGS VERSION
time <- all23$t
d1 <- ifelse(time==1,1,0)
d2 <- ifelse(time==2,1,0)
d3 <- ifelse(time==3,1,0)
d4 <- ifelse(time==4,1,0)
d5 <- ifelse(time==5,1,0)
d6 <- ifelse(time==6,1,0)
d7 <- ifelse(time==7,1,0)
d8 <- ifelse(time==8,1,0)
d9 <- ifelse(time==9,1,0)
d10 <- ifelse(time==10,1,0)
d11 <- ifelse(time==11,1,0)
d12 <- ifelse(time==12,1,0)
d13 <- ifelse(time==13,1,0)
d14 <- ifelse(time==14,1,0)
# d15 <- ifelse(time==15,1,0)

p1 <-  c(1,1,1,1,1,1,1,1,1,0,0) # presente o ausente en el semestre
p2 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p3 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p4 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p5 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p6 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p7 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p8 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p9 <-  c(1,1,1,1,1,1,0,1,0,1,1)
p10 <- c(1,1,1,1,1,1,0,1,0,1,1)
p11 <- c(1,1,1,1,1,1,0,1,0,1,1)
p12 <- c(1,1,1,1,1,1,0,1,0,1,1)
p13 <- c(1,1,1,1,1,1,0,1,0,1,1)
p14 <- c(1,1,1,1,1,1,0,1,0,1,1)
# p15 <- c(1,1,1,1,1,1,0,1,0,1,1)

d <- list (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)
ps <- list (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14)

semester.results <- list () ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS

x.location <- c(1,2,0,-2,rep(0,7))
x.precision  <- c(4,4,1,4,rep(1,7))


inicio <- c ( 1:553 )
final  <- c ( 30:582)

S <- length(inicio)
for (s in 1:S){        # <= BIG FUNCTION STARTS
	x.mean <- x.location
	x.tau  <- x.precision

v <- all23[inicio[s]:final[s],1:11]; ## EXTRACT VOTES
# v <- all23[d[[s]]==1,1:11]; ## EXTRACT VOTES
# p <- ps[[s]]
v[v==0] <- NA; v[v==-1] <- 0    ## Versi?n probit requiere 0s y 1s
v <- t(v)                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
##
# ife.data <- list ("J", "I", "v", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13", "d14", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14")



ife.data <- list ("J", "I", "v", "x.mean","x.tau")
ife.inits <- function (){
    list (
#     	x1=rnorm(J),
    x=rnorm(J),
#     x2=rnorm(J),
#     x3=rnorm(J),
#     x4=rnorm(J),
#     x5=rnorm(J),
#     x6=rnorm(J),
#     x7=rnorm(J),
#     x8=rnorm(J),
#     x9=rnorm(J),
#     x10=rnorm(J),
#     x11=rnorm(J),
#     x12=rnorm(J),
#     x13=rnorm(J),
#     x14=rnorm(J),
    signal=rnorm(I),
    difficulty=rnorm(I)
    )
    }
ife.parameters <- c("x", "signal", "difficulty")#, "deviance")

print(cat("\tSession no.",s,"of",S,", with", I, "votes \n"))
	
#full run
start.time <- proc.time()

results <- mclapply(1:2, function(x) {
		model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
								   model.file=model1Dj.irt, n.chains=1,
								   n.iter=60000, n.burnin=30000, n.thin=300
		))
		if(inherits(model.jags.re,"try-error")) {return()}
		return(model.jags.re)
	}, mc.cores=2 )
time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)
	

semester.results[length(semester.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
	
x.location  <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
x.precision <- rep (100, 11)
}


ideal.points <- matrix (NA, nrow=S, ncol=11)
for (i in 1:S){
	ideal.points[i,] <- apply (rbind (semester.results[[i]][[1]]$BUGSoutput$sims.list$x, semester.results[[i]][[2]]$BUGSoutput$sims.list$x), 2, median)
}


j <- 1
plot(c(1:S), ideal.points[1:S,j], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:11){
	lines(smooth.spline(c(1:S), ideal.points[1:S,j], df=10), lwd=3, col=color.23[j])
}



# save (semester.results, file="SemesterResults.RData")

load ("SemesterResults.RData")


# texto <- "Ventanas de 30 votos, con slack de 1/100"
# sendEmail (subject="Listo SemesterResults", text=texto, address="emagar@itam.mx")
# sendEmail (subject="Listo SemesterResults", text=texto, address="grosas@wustl.edu")

library (mcmcplots)
library (sm)

S <- length (semester.results)
idealpoints <- matrix (NA, nrow=S, ncol=11)
for (i in 1:S){
	idealpoints[i,] <- semester.results[[i]][[2]]$BUGSoutput$median$x
}

j <- 1
plot(c(1:S), idealpoints[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:11){
	lines(smooth.spline(c(1:S), idealpoints[1:S,j], df=10), lwd=3, col=color.23[j])
}
lines (smooth.spline(c(1:S), rowMeans(idealpoints), df=10), lwd=5, col="black")
# chainsConv <- mcmc.list(list (as.mcmc (semester.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (semester.results[[i]][[1]]$BUGSoutput$sims.list$x)))
# gelman.diag  (chainsConv)
# heidel.diag  (chainsConv)
# raftery.diag (chainsConv, q = 0.025, r = 0.01, s = 0.95)
# gewDiag <- geweke.diag (chainsConv)
# sm.density (gewDiag[[1]]$z, model="Normal")
# sm.density (gewDiag[[2]]$z, model="Normal")
# 
# # Traceplots of a random set of parameters
# mcmcplot (chainsConv, dir=getwd()
# 		  , filename="traceplotsIFE")   # <= Change name here)
# 



#xxxxxx viejo

attach.bugs(ife.23)
jotas <- array(NA, dim=c(T,3,J))
for (c in 1:3){
    for (j in 1:J){
        jotas[1,c,j] <- quantile (x1[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[2,c,j] <- quantile (x2[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[3,c,j] <- quantile (x3[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[4,c,j] <- quantile (x4[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[5,c,j] <- quantile (x5[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[6,c,j] <- quantile (x6[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[7,c,j] <- quantile (x7[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[8,c,j] <- quantile (x8[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[9,c,j] <- quantile (x9[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[10,c,j] <- quantile (x10[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[11,c,j] <- quantile (x11[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[12,c,j] <- quantile (x12[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[13,c,j] <- quantile (x13[,j], probs=(.5+(c-2)*.475), names=F)
        jotas[14,c,j] <- quantile (x14[,j], probs=(.5+(c-2)*.475), names=F)
                 }
              }
## Consejeros fuera del IFE
jotas[9:T,1:3,7]   <- NA #Molinar
jotas[9:T,1:3,9]   <- NA #Zebadua
jotas[1:8,1:3,10]  <- NA #Rivera
jotas[1:8,1:3,11]   <- NA #Luken
#
j1 <- jotas[,,1]    #Woldenberg
j2 <- jotas[,,2]    #Barrag?n
j3 <- jotas[,,3]    #Cant?
j4 <- jotas[,,4]    #C?rdenas
j5 <- jotas[,,5]    #Lujambio
j6 <- jotas[,,6]    #Merino
j7 <- jotas[,,7]    #Molinar
j8 <- jotas[,,8]    #Peschard
j9 <- jotas[,,9]    #Zebad?a
j10 <- jotas[,,10]  #Rivera
j11 <- jotas[,,11]  #Luken

item.midpoints <- matrix(NA, ncol=3, nrow=I)
for (i in 1:I){
    item.midpoints[i,1] <- quantile (m[,i], 0.10, names=F)
    item.midpoints[i,2] <- quantile (m[,i], 0.50, names=F)
    item.midpoints[i,3] <- quantile (m[,i], 0.90, names=F)
              }
tmp <- t(m)
sem <- d1+d2*2+d3*3+d4*4+d5*5+d6*6+d7*7+d8*8+d9*9+d10*10+d11*11+d12*12+d13*13+d14*14
sem.midpoints <- matrix(NA, ncol=3, nrow=T)
for (i in 1:T){
    sem.midpoints[i,1] <- quantile (tmp[sem==i,], 0.10, names=F)
    sem.midpoints[i,2] <- quantile (tmp[sem==i,], 0.50, names=F)
    sem.midpoints[i,3] <- quantile (tmp[sem==i,], 0.90, names=F)
              }


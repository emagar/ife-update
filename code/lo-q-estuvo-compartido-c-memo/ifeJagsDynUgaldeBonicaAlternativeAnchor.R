##################################################################################
# Ugalde et al a la Bonica
# This code runs a Bonica-like algorithm to provide a dynamic view of Woldenberg's
# IFE.  The run has already been saved ("DynUgaldeBonica.RData")
# March 19, 2013: Add party-based priors for councilors coming in from outside
# March 20, 2013: We use an alternative anchoring mechanism, putting informative
#    priors on common item parameters
##################################################################################

library (arm)
library (MCMCpack)
library (foreign)
library (car)
library (gtools)
library (multicore)
library (R2jags)
library (mcmcplots)
library (sm)
library (lubridate)



rm(list = ls())
workdir <- c("~/Dropbox/ifeSharedGE/data/")
setwd(workdir)

# Define colors and plotting names
names.456789 <- c("Ugalde", "Albo", "Andrade", "Alcantar", "Glez. Luna", "Latapi", "Lopez Flores", "Morales", "Sanchez", "Valdes", "Banos", "Nacif", "Elizondo", "Figueroa", "Guerrero", "Marvan", "Cordova", "Garcia Ramirez")

color.4567 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red")
party.4567 <- c("PRI", "PAN", "PRI", "PVEM",  "PAN", "PRI", "PRI", "PAN", "PAN", "PRD", "PRI", "PAN", "PAN", "PRD", "PRI")
party.4567 <- ifelse (party.4567=="PRI", 1, ifelse (party.4567=="PAN", 2, ifelse (party.4567=="PRD", 3, 4)))

rgb.4567 <- c (length=15)
rgb.4567[c(1,3,6,7,11,15)] <- rgb(1,0,0,0.6) #red
rgb.4567[c(2,5,8,9,12,13)] <- rgb(0,0,1,0.6) #blue
rgb.4567[4] <- rgb(34/255,139/255,34/255,0.6) #verde
rgb.4567[c(10,14)]<- rgb(1,215/255,0,0.6) #gold
#
color.45678 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red")
color.456789 <- c(color.45678, "blue", "gold", "red")
party.456789 <- c(party.4567, 2, 3, 1)

rgb.456789 <- rgb.4567
rgb.456789 <- c(rgb.456789, rgb(0,0,1,0.6)) #blue
rgb.456789 <- c(rgb.456789, rgb(1,215/255,0,0.6)) #gold
rgb.456789 <- c(rgb.456789, rgb(1,0,0,0.6)) #red

# greys.4567 <- c("grey50", "grey30", "grey50", "grey50", "grey30", "grey50", "grey50", "grey30", "grey30",
# 				"grey70", "grey50", "grey30", "grey30", "grey70", "grey50")
# greyLines.4567 <- matrix(NA, nrow=15, ncol=2)
# greyLines.4567[,1] <- c("grey80", "white", "grey80", "grey80", "white", "grey80", "grey80", "white", "white",
# 						"black", "grey80", "white", "white", "black", "grey80")
# greyLines.4567[,2] <- c(1,1,1,3,1,1,1,1,1,1,1,1,1,1,1)

# Read Ugalde et al's IFE's votes, only informative votes
all456789 <-read.csv("tmp456789.csv",  header=TRUE)

# sort by date and add session counter
tmp <- all456789
xxx <- rep(0, nrow(tmp)); xxx[1] <- 1;
tmp <- tmp[order(tmp$yr, tmp$mo, tmp$dy, tmp$folio),]
tmp$sess <- rep(0, nrow(tmp)); tmp$date <- tmp$yr*10000+tmp$mo*100+tmp$dy
tmp$sess[1] <- 1
for (i in 2:nrow(tmp)){
  tmp$sess[i] <- ifelse(tmp$date[i]==tmp$date[i-1], tmp$sess[i-1], tmp$sess[i-1]+1)
  xxx[i] <- ifelse(tmp$date[i]==tmp$date[i-1], 0, 1);
}
all456789 <- tmp
rm(tmp)

## Create object with session traits (choose one)
tmp <- all456789
nv <- as.numeric(table(tmp$date)); tmp <- tmp[xxx==1,]
tmp2 <- rep(NA, max(tmp$sess))
sess.dat <- data.frame(sess=1:max(tmp$sess), yr=tmp2, mo=tmp2, dy=tmp2, date=tmp2, nvot=tmp2, term=tmp2)
sess.dat$yr <- tmp$yr; sess.dat$mo <- tmp$mo; sess.dat$dy <- tmp$dy; sess.dat$date <- tmp$date; sess.dat$term <- tmp$term
sess.dat$nvot <- nv; sess.dat$term <- tmp$term;
sess.dat$date <- ymd(sess.dat$date)
rm(xxx,tmp,tmp2,nv)


#############################
###     UGALDE ET AL      ###
#############################

## MODEL
model1Dj.irt <- function() {
	for (j in 1:J){                ## loop over respondents
		for (i in 1:I){              ## loop over items
			v[j,i] ~ dbern(p[j,i]);                                 ## voting rule
			probit(p[j,i]) <- mu[j,i];                              ## sets 0<p<1 as function of mu
			mu[j,i] <- signal[i]*x[j] - difficulty[i];                            ## utility differential
		}
	}
	## priors ################
	for (j in 1:J){
		x[j] ~ dnorm (x.mean[j], x.tau[j]);
	}
	for (i in 1:I){
		signal[i] ~ dnorm(mu.signal[i], prec.signal[i]);
		difficulty[i] ~ dnorm(mu.difficulty[i], prec.difficulty[i]);
	}
	for (p in 1:4){
		partyPos[p] <- mean (x[party[p]]);
	}
}
#end model##############

# Establish moving windows of 30 votes each
inicio <- c ( 1:1081 )
final  <- c ( 30:1110 )
S <- length(inicio)

# # Alternative: center on vote (for date), extend windows to both sides
# item <- 1:I  # Need to define I before
# inicio <- item-15; inicio[inicio<0] <- 0
# final  <- item+15; final[final>I] <- I

# Added March 19: We need a matrix showing whether each councilor is actually in IFE the moment the vote takes place
IsCouncilor <- matrix (1, ncol=18, nrow=max(final))
IsCouncilor[ all456789$date > 20071217,1 ] <- NA 
IsCouncilor[ all456789$date > 20080814,2 ] <- NA 
IsCouncilor[ all456789$date > 20101027,3 ] <- NA 
IsCouncilor[ all456789$date > 20101027,4 ] <- NA 
IsCouncilor[ all456789$date > 20080814,5 ] <- NA 
IsCouncilor[ all456789$date > 20071217,6 ] <- NA 
IsCouncilor[ all456789$date > 20080814,7 ] <- NA 
IsCouncilor[ all456789$date > 20071217,8 ] <- NA 
IsCouncilor[ all456789$date > 20101027,9 ] <- NA 
IsCouncilor[ all456789$date < 20080215,10] <- NA 
IsCouncilor[ all456789$date < 20080215,11] <- NA 
IsCouncilor[ all456789$date < 20080215,12] <- NA 
IsCouncilor[ all456789$date < 20080829,13] <- NA 
IsCouncilor[ all456789$date < 20080829,14] <- NA 
IsCouncilor[ all456789$date < 20080829,15] <- NA 
IsCouncilor[ all456789$date < 20111215,16] <- NA 
IsCouncilor[ all456789$date < 20111215,17] <- NA 
IsCouncilor[ all456789$date < 20111215,18] <- NA 


# Initial ideal points to anchor ideological space
x.location <- c(1,0,0,2,-2,0,0,2,-2,rep(0,9))
x.precision  <- c(4,1,1,4,4,1,1,4,4,rep(1,9))
semester.results <- list () ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
partyPlacement <- rep (NA,18)
x.mean <- numeric ()
x.tau  <- numeric ()


# Run once on the first 30 votes to obtain estimates for item parameters of the first 30 votes
councilor.in <- apply (IsCouncilor[inicio[1]:final[1],], 2, invalid)
councilors <- names.456789[councilor.in==FALSE]
party      <- party.456789[councilor.in==FALSE]

for (c in 1:18){
	x.mean[c] <- ifelse (!is.na(x.location[c]), x.location[c], NA)
	x.tau[c]  <- ifelse (!is.na(x.precision[c]), x.precision[c], 4)
}

v <- all456789[inicio[1]:final[1],1:18][,councilor.in==FALSE]; ## EXTRACT 30 VOTES EACH TIME
v[v==0] <- NA; v[v==-1] <- 0    ## Version probit requiere 0s y 1s
v <- t(v)                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
prec.signal <- rep (0.1, 30)
prec.difficulty <- rep (0.25, 30)
mu.signal <- rep (0, 30)
mu.difficulty <- rep (0, 30)

ife.data <- list ("J", "I", "v", "x.mean","x.tau","party","prec.signal","prec.difficulty","mu.signal","mu.difficulty")
ife.inits <- function (){
	list (
		x=rnorm(J),
		signal=rnorm(I),
		difficulty=rnorm(I)
	)
}
ife.parameters <- c("x", "signal", "difficulty", "partyPos")

# JAGS run
start.time <- proc.time()
# Use dual core capabilities
results <- mclapply(1:2, function(x) {
	model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
							   model.file=model1Dj.irt, n.chains=1,
							   n.iter=240000, n.burnin=60000, n.thin=1000
	))
	if(inherits(model.jags.re,"try-error")) {return()}
	return(model.jags.re)
}, mc.cores=2 )
time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)


diff.priors <- apply( rbind (results[[1]]$BUGSoutput$sims.list$difficulty, results[[2]]$BUGSoutput$sims.list$difficulty), 2, median)
sign.priors <- apply( rbind (results[[1]]$BUGSoutput$sims.list$signal, results[[2]]$BUGSoutput$sims.list$signal), 2, median)
diff.priors.var <- apply( rbind (results[[1]]$BUGSoutput$sims.list$difficulty, results[[2]]$BUGSoutput$sims.list$difficulty), 2, var)
sign.priors.var <- apply( rbind (results[[1]]$BUGSoutput$sims.list$signal, results[[2]]$BUGSoutput$sims.list$signal), 2, var)

# Convergence is extremely important here
chainsConv.x <- mcmc.list(list (as.mcmc (results[[1]]$BUGSoutput$sims.list$x), as.mcmc (results[[2]]$BUGSoutput$sims.list$x)))
chainsConv.signal <- mcmc.list(list (as.mcmc (results[[1]]$BUGSoutput$sims.list$signal), as.mcmc (results[[2]]$BUGSoutput$sims.list$signal)))
chainsConv.difficulty <- mcmc.list(list (as.mcmc (results[[1]]$BUGSoutput$sims.list$difficulty), as.mcmc (results[[2]]$BUGSoutput$sims.list$difficulty)))

gelman.diag (chainsConv.x)[[2]]
gelman.diag (chainsConv.signal)[[2]]
gelman.diag (chainsConv.difficulty)[[2]]

# Convergence seems OK, though still nothing to write home about

for (s in 1:S){        # <= BIG FUNCTION STARTS (loop over 1081 windows)

	# Added March 20: We will use item parameters as anchors.
	# Basically, for run s we use as priors for the first 29 votes the point estimates of the posteriors of the last 29 votes from run s-1
	# We continue to avoid estimating ideal points for councilors that are not in the mix
	councilor.in <- apply (IsCouncilor[inicio[s]:final[s],], 2, invalid)
	councilors <- names.456789[councilor.in==FALSE]
	party      <- party.456789[councilor.in==FALSE]

	x.mean <- rep(0, length(councilors))
	x.tau  <- rep(2, length(councilors))
	
	prec.signal <- c(rep (100, 29), 0.1)       # Relatively high precision on item parameters, in the hope of achieving identification
	prec.difficulty <- c(rep (100, 29), 0.25)  # However, the "new" vote has low precision
	v <- all456789[inicio[s]:final[s],1:18][,councilor.in==FALSE]; ## EXTRACT 30 VOTES EACH TIME
	v[v==0] <- NA; v[v==-1] <- 0    ## Version probit requiere 0s y 1s
	v <- t(v)                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
	J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
	
	ife.data <- list ("J", "I", "v", "x.mean","x.tau","party","prec.signal","prec.difficulty","mu.signal","mu.difficulty")
	ife.inits <- function (){
		list (
			x=rnorm(J),
			signal=rnorm(I),
			difficulty=rnorm(I)
		)
	}
	ife.parameters <- c("x", "signal", "difficulty", "partyPos")
	
	print(cat("Session no.",s,"of",S,", with", I, "votes \n"))
		
	#full JAGS run
	start.time <- proc.time()
	
	# Use dual core capabilities
	results <- mclapply(1:2, function(x) {
		model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
								   model.file=model1Dj.irt, n.chains=1,
								   n.iter=100000, n.burnin=40000, n.thin=150
		))
		if(inherits(model.jags.re,"try-error")) {return()}
		return(model.jags.re)
	}, mc.cores=2 )
	time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
	print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)
	
	results[[3]] <- councilors
 	semester.results[length(semester.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
	
	# Quick check on convergence of ideal point chains
	GHconv <- gelman.diag(mcmc.list(list (as.mcmc (results[[2]]$BUGSoutput$sims.list$x), as.mcmc (results[[1]]$BUGSoutput$sims.list$x))))[[2]]
	print (cat ("Gelman-Rubin R-hat:", GHconv, "\n")); rm (GHconv)
		
	# Update location of ideal point at time s, to be used as location prior at time s+1
	diff.priors <- c(apply( rbind (results[[1]]$BUGSoutput$sims.list$difficulty, results[[2]]$BUGSoutput$sims.list$difficulty), 2, median)[2:30], 0)
	sign.priors <- c(apply( rbind (results[[1]]$BUGSoutput$sims.list$signal, results[[2]]$BUGSoutput$sims.list$signal), 2, median)[2:30], 0)
	
	# Precision prior is always constant at 100, implying standard deviation = sqrt (1/100) = 0.1
}  # <---   END OF LOOP OVER WINDOWS


# Save semester.results, containing all chains from all runs
# save (semester.results, file="DynUgaldeBonicaMarch19.RData")
# save (semester.results, file="DynUgaldeBonica.RData")

source("http://rtm.wustl.edu/code/sendEmail.R")
sendEmail (subject="Ugalde et al esta listo", text="", address="grosas@wustl.edu")
# sendEmail (subject="Ugalde et al esta listo", text="", address="emagar@itam.mx")

# RData file with runs carried out in Mexico, early March
# load ("DynUgaldeBonica.RData")

# RData file with runs carried out in Wash U, March 19
# These runs omit non-sitting Councilors and party precisions for new Councilors
load ("DynUgaldeBonicaMarch19.RData")




S <- length (semester.results)
multiGelman.hat <- numeric ()
for (i in 2:S){
	chainsConv <- mcmc.list(list (as.mcmc (semester.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (semester.results[[i]][[1]]$BUGSoutput$sims.list$x)))
	tmp <- gelman.diag (chainsConv)[[2]]
	multiGelman.hat <- c(multiGelman.hat, tmp)
}
rm (tmp, chainsConv)


# A few are not converged, very likely because of inability to avoid reflectional invariance


CouncilorIn <- matrix (1, nrow=18, ncol=S)
CouncilorIn[1,  all456789$date[-c(1:29)] > 20071217] <- NA 
CouncilorIn[2,  all456789$date[-c(1:29)] > 20080814] <- NA 
CouncilorIn[3,  all456789$date[-c(1:29)] > 20101027] <- NA 
CouncilorIn[4,  all456789$date[-c(1:29)] > 20101027] <- NA 
CouncilorIn[5,  all456789$date[-c(1:29)] > 20080814] <- NA 
CouncilorIn[6,  all456789$date[-c(1:29)] > 20071217] <- NA 
CouncilorIn[7,  all456789$date[-c(1:29)] > 20080814] <- NA 
CouncilorIn[8,  all456789$date[-c(1:29)] > 20071217] <- NA 
CouncilorIn[9,  all456789$date[-c(1:29)] > 20101027] <- NA 
CouncilorIn[10,  all456789$date[-c(1:29)] < 20080215] <- NA 
CouncilorIn[11,  all456789$date[-c(1:29)] < 20080215] <- NA 
CouncilorIn[12,  all456789$date[-c(1:29)] < 20080215] <- NA 
CouncilorIn[13,  all456789$date[-c(1:29)] < 20080829] <- NA 
CouncilorIn[14,  all456789$date[-c(1:29)] < 20080829] <- NA 
CouncilorIn[15,  all456789$date[-c(1:29)] < 20080829] <- NA 
CouncilorIn[16,  all456789$date[-c(1:29)] < 20111215] <- NA 
CouncilorIn[17,  all456789$date[-c(1:29)] < 20111215] <- NA 
CouncilorIn[18,  all456789$date[-c(1:29)] < 20111215] <- NA 



# If using DynUgaldeBonica.RData, use the following code to extract ideal points
ideal.points <- matrix (NA, nrow=S, ncol=18)
for (i in 1:S){
	ideal.points[i,] <- apply (rbind (semester.results[[i]][[1]]$BUGSoutput$sims.list$x, semester.results[[i]][[2]]$BUGSoutput$sims.list$x), 2, median)
}

# If using DynUgaldeBonicaMarch19.RData, use the following code to extract ideal points
ideal.points <- matrix (NA, nrow=S, ncol=18)
ideal.points.var <- matrix (NA, nrow=S, ncol=18)
for (i in 1:S){
	for (j in 1:18){
		councilor <- names.456789[j]
		num <- which (semester.results[[i]][[3]]==councilor)
		if ( length (num)==0 ) {
			ideal.points[i,j] <- 1
			ideal.points.var[i,j]  <- 0
		} else {
			# Temporarily comment out one of the chains, since there was no convergence
			ideal.points[i,j] <- median (c (semester.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], semester.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]))
			ideal.points.var[i,j]  <- var (c (semester.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], semester.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]))
		}
	}
}

# Get SDs of estimates, the width should be useful to plot thickness of data points
ideal.points.var <- sqrt (ideal.points.var)

# Non-smoothed ideal point time-paths
# pdf ("UgaldeEtAlNonSmooth.pdf", h=7, w=9)
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:18){
# 	lines(ideal.points[1:S,j], lwd=3, col=color.456789[j])
	lines(CouncilorIn[j,1:S] * ideal.points[1:S,j], lwd=3, col=rgb.456789[j])
}
# dev.off()



# Smoothed ideal point time-paths
# pdf ("UgaldeEtAlSmooth.pdf", h=7, w=9)
pdf ("UgaldeEtAlSmoothMarch19.pdf", h=7, w=9)
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-10,10), type="n", xlab="", ylab="Ideal points")
for (j in 1:18){
	lines(smooth.spline(c(1:S)[!is.na(CouncilorIn[j,1:S])], ideal.points[!is.na(CouncilorIn[j,1:S]),j], df=10), lwd=3, col=rgb.456789[j])
# 	lines(smooth.spline(c(1:S), ideal.points[,j], df=10), lwd=3, col=color.456789[j])
}
dev.off()




# Smoothed ideal point time-paths, daily change movie
Smooth <- list ()
for (j in 1:18){
	Smooth[[j]] <- smooth.spline(c(1:S), ideal.points[,j], df=10)
}

for (j in 1:18){
	Smooth[[j]]$y[is.na(CouncilorIn[j,1:S])] <- NA
}

snapshot <- seq (1, 1081, by=10)
for (s in snapshot){
	which.s <- which (snapshot==s)
	if (which.s < 10) {name = paste('Ugalde00', which.s,'plot.png',sep='')}
	if (which.s >= 10 && which.s < 100) {name = paste('Wolden0', which.s,'plot.png', sep='')}
	if (which.s >= 100) {name = paste('Ugalde', which.s,'plot.png', sep='')}
	
	jpeg (name, quality=100, height=500, width=500)
	
	plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-10,10), type="n", xlab="", ylab="Ideal points")
	for (j in 1:18){
		lines ( Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s], lwd=6, col=rgb.456789[j])
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







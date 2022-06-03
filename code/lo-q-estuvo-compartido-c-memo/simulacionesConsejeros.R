###################################################################
# Simulaciones de consejeros
# GR, Marzo 7 2013
# Este archivo simula votos segun el proceso de generacion de IRT
# La idea es comparar la calidad de las estimaciones de votos ideales
# en dos circunstancias:
# 1. Cuando hay un numero variable de votos en cada semestre, pero la 
#    distribucion de "casos" es similar en cada semestre
# 2. Cuando la distribucion de casos es distinta semestre a semestre,
#    pero la cantidad de votos es la misma
###################################################################

library(arm)
library (MCMCpack)
library (foreign)
library (car)
library (gtools)
library (multicore)
library (R2jags)

rm(list = ls())

workdir <- c("~/Dropbox/ifeSharedGE/code/")
setwd(workdir)

set.seed (1971)

consejero <- sort (runif (9,-2.5,2.5))
signal  <- c (runif (100, 1, 3), runif (100, -3, -1), runif (100, -1, 1), runif (200, -3, 3))
dificul <- c (rnorm (500, 0, 2))

varSemester <- c (15, 20, 15, 20, 80, 60, 40, 15, 20, 15, 20, 80, 60, 40)
reorderSignal <- sample (1:500, 500)
signal.R  <- signal[reorderSignal]	
dificul.R <- dificul[reorderSignal]


linPredVarCases <- matrix (NA, nrow=length (consejero), ncol=length (signal))
linPredVarSemes <- matrix (NA, nrow=length (consejero), ncol=length (signal))
for (i in 1:length(consejero)){
	for (j in 1:length(signal)){
		linPredVarCases[i,j] <- signal[j]*consejero[i] - dificul[j] + rnorm (1,0,0.5)
		linPredVarSemes[i,j] <- signal.R[j]*consejero[i] - dificul.R[j] + rnorm (1,0,0.5)
	}
}

YVarCases <- round (invlogit(linPredVarCases))
YVarSemes <- round (invlogit(linPredVarSemes))


EstVarCases <- MCMCirt1d (YVarCases, theta.constraints=list(V1=-2, V9=2), mcmc=20000, burnin=10000, thin=20, verbose=1000)
EstVarSemes <- MCMCirt1d (YVarSemes, theta.constraints=list(V1=-2, V9=2), mcmc=20000, burnin=10000, thin=20, verbose=1000)



# Estimaciones
model1Dj.irt <- function() {
	for (j in 1:J){                ## loop over councilors
		for (i in 1:I){              ## loop over items
			v[j,i] ~ dbern(pi[j,i]);                                 ## voting rule
			probit(pi[j,i]) <- mu[j,i];                              ## sets 0<p<1 as function of mu
			mu[j,i] <- signal[i]*(x[j]) - difficulty[i];
		}
	}
	## priors
	for (j in 1:J){
		x[j] ~ dnorm(x.mean[j], x.tau[j]);    #Woldenberg
	}
	for(i in 1:I){
		signal[i] ~ dnorm( 0, 0.1);
	}
	for(i in 1:I){
		difficulty[i] ~ dnorm( 0, 0.25);
	}
}
## END MODEL


## Version con casos diferentes en cada semestre
## JAGS VERSION
time <- c(rep(1,35), rep(2,35), rep(3,35), rep(4,35), rep(5,35), rep(6,35), rep(7,35), rep(8,35), rep(9,35), rep(10,35), rep(11,35), rep(12,35), rep(13,35), rep(14,35))
semester.results <- list () ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
x.location <- c(-2,rep(0,7),2)
x.precision  <- c(4,rep(1,7),4)

S <- 14
for (s in 1:S){        # <= BIG FUNCTION STARTS
	x.mean <- x.location
	x.tau  <- x.precision
	
	v <- YVarCases[1:9,time==s]; ## EXTRACT VOTES
	J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
	
	ife.data <- list ("J", "I", "v", "x.mean","x.tau")
	ife.inits <- function (){
		list (
			x=rnorm(J),
			signal=rnorm(I),
			difficulty=rnorm(I)
		)
	}
	ife.parameters <- c("x", "signal", "difficulty")
	
	print(cat("\tSession no.",s,"of",S,", with", I, "votes \n"))
	
	#full run
	start.time <- proc.time()
	results <- mclapply(1:2, function(x) {
		model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
								   model.file=model1Dj.irt, n.chains=1,
								   n.iter=10000, n.burnin=5000, n.thin=50
		))
		if(inherits(model.jags.re,"try-error")) {return()}
		return(model.jags.re)
	}, mc.cores=2 )
	time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
	print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)
	
	semester.results[length(semester.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
	
	x.location  <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
	x.precision <- rep (50, 9)
}

for (i in 1:S){
	chainsConv <- mcmc.list(list (as.mcmc (semester.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (semester.results[[i]][[1]]$BUGSoutput$sims.list$x)))
	print (gelman.diag (chainsConv))
}

ideal.points.1 <- matrix (NA, nrow=S, ncol=9)
for (i in 1:S){
	ideal.points.1[i,] <- apply (rbind (semester.results[[i]][[1]]$BUGSoutput$sims.list$x, semester.results[[i]][[2]]$BUGSoutput$sims.list$x), 2, median)
}

colors <- c("red","black","orange","blue","gold","green","gray","brown","purple")

plot(c(1:S), ideal.points.1[1:S,1], main="", ylim=c(-4,4), type="n", xlab="", ylab="Ideal points")
abline (h=summary (EstVarCases)[[1]][,1], lty=3, lwd=2, col=colors)
for (j in 1:9){
	lines(smooth.spline(c(1:S), ideal.points.1[1:S,j], df=10), lwd=3, col=colors[j])
}









## Version con informacion cambiante en cada semestre
## JAGS VERSION
time <- numeric ()
for (i in 1:length(varSemester)){
	x <- rep (i, varSemester[i])
	time <- c(time, x)
}
semester.results <- list () ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
x.location <- c(-2,rep(0,7),2)
x.precision  <- c(4,rep(1,7),4)

S <- 14
for (s in 1:S){        # <= BIG FUNCTION STARTS
	x.mean <- x.location
	x.tau  <- x.precision
	
	v <- YVarSemes[1:9,time==s]; ## EXTRACT VOTES
	J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
	
	ife.data <- list ("J", "I", "v", "x.mean","x.tau")
	ife.inits <- function (){
		list (
			x=rnorm(J),
			signal=rnorm(I),
			difficulty=rnorm(I)
		)
	}
	ife.parameters <- c("x", "signal", "difficulty")
	
	print(cat("\tSession no.",s,"of",S,", with", I, "votes \n"))
	
	#full run
	start.time <- proc.time()
	results <- mclapply(1:2, function(x) {
		model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
								   model.file=model1Dj.irt, n.chains=1,
								   n.iter=10000, n.burnin=5000, n.thin=50
		))
		if(inherits(model.jags.re,"try-error")) {return()}
		return(model.jags.re)
	}, mc.cores=2 )
	time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
	print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)
	
	semester.results[length(semester.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
	
	x.location  <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
	x.precision <- rep (50, 9)
}

mult <- c(1,1,1,1,1,1,1,1,1,1,1,1,-1,1)
mult <- rep (1,14)
for (i in 1:S){
	chainsConv <- mcmc.list(list (as.mcmc (semester.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (semester.results[[i]][[1]]$BUGSoutput$sims.list$x)))
	print (gelman.diag (chainsConv))
}

ideal.points <- matrix (NA, nrow=S, ncol=9)
for (i in 1:S){
	ideal.points[i,] <- apply (rbind (semester.results[[i]][[1]]$BUGSoutput$sims.list$x, semester.results[[i]][[2]]$BUGSoutput$sims.list$x)*mult[i], 2, median)
# 	ideal.points[i,] <- apply (semester.results[[i]][[1]]$BUGSoutput$sims.list$x*mult[i], 2, median)
}

colors <- c("red","black","orange","blue","gold","green","gray","brown","purple")

plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-4,4), type="n", xlab="", ylab="Ideal points")
abline (h=summary (EstVarCases)[[1]][,1], lty=3, lwd=2, col=colors)
for (j in 1:9){
	lines (ideal.points[1:S,j], lwd=3, col=colors[j])
}


lines (varSemester/20)


##########################################################
##########################################################
##########################################################
##########################################################
# Let's do the same thing, but now with dynamic points
##########################################################
##########################################################
##########################################################
##########################################################

consejeroDyn <- matrix (NA, ncol=20, nrow=9)
consejeroDyn[,1] <- consejero
for (j in 2:20) {
	for (i in 1:9) {
		consejeroDyn[i,j] <- consejeroDyn[i,(j-1)] + rnorm (1,0,0.13)
	} 
}

changePref <- sort ( rep (1:20, 25) )

linPredVarCases2 <- matrix (NA, nrow=length (consejero), ncol=length (signal))
linPredVarSemes2 <- matrix (NA, nrow=length (consejero), ncol=length (signal))
for (i in 1:length(consejero)){
	for (j in 1:length(signal)){
		linPredVarCases2[i,j] <- signal[j]*consejeroDyn[i,changePref[j]] - dificul[j] + rnorm (1,0,0.5)
		linPredVarSemes2[i,j] <- signal.R[j]*consejeroDyn[i,changePref[j]] - dificul.R[j] + rnorm (1,0,0.5)
	}
}

YVarCases2 <- round (invlogit(linPredVarCases2))
YVarSemes2 <- round (invlogit(linPredVarSemes2))


EstVarCases2 <- MCMCirt1d (YVarCases2, theta.constraints=list(V1=-2, V9=2), mcmc=20000, burnin=10000, thin=20, verbose=1000)
EstVarSemes2 <- MCMCirt1d (YVarSemes2, theta.constraints=list(V1=-2, V9=2), mcmc=20000, burnin=10000, thin=20, verbose=1000)

plot (consejeroDyn[1,], type="l", col=colors[1], ylim=c(-3,3))
for (j in 2:9){
	lines (consejeroDyn[j,], lwd=3, col=colors[j])
}
abline (h=summary (EstVarCases2)[[1]][,1], lty=3, lwd=2, col=colors)








## We expect that there will be change produced by different information sets,
## but with some luck we will also pick up the "real" variation in ideal points
## Version con informacion cambiante en cada semestre
## JAGS VERSION
time <- numeric ()
for (i in 1:length(varSemester)){
	x <- rep (i, varSemester[i])
	time <- c(time, x)
}
semester.results <- list () ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
x.location <- c(-2,rep(0,7),2)
x.precision  <- c(4,rep(1,7),4)

S <- 14
for (s in 1:S){        # <= BIG FUNCTION STARTS
	x.mean <- x.location
	x.tau  <- x.precision
	
	v <- YVarSemes2[1:9,time==s]; ## EXTRACT VOTES
	J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
	
	ife.data <- list ("J", "I", "v", "x.mean","x.tau")
	ife.inits <- function (){
		list (
			x=rnorm(J),
			signal=rnorm(I),
			difficulty=rnorm(I)
		)
	}
	ife.parameters <- c("x", "signal", "difficulty")
	
	print(cat("\tSession no.",s,"of",S,", with", I, "votes \n"))
	
	#full run
	start.time <- proc.time()
	results <- mclapply(1:2, function(x) {
		model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
								   model.file=model1Dj.irt, n.chains=1,
								   n.iter=10000, n.burnin=5000, n.thin=50
		))
		if(inherits(model.jags.re,"try-error")) {return()}
		return(model.jags.re)
	}, mc.cores=2 )
	time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
	print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)
	
	semester.results[length(semester.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
	
	x.location  <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
	x.precision <- rep (50, 9)
}



mult <- c(1,1,1,1,1,1,1,1,1,1,1,1,-1,1)
mult <- rep (1,14)
for (i in 1:S){
	chainsConv <- mcmc.list(list (as.mcmc (semester.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (semester.results[[i]][[1]]$BUGSoutput$sims.list$x)))
	print (gelman.diag (chainsConv))
}

ideal.points <- matrix (NA, nrow=S, ncol=9)
for (i in 1:S){
# 	ideal.points[i,] <- apply (rbind (semester.results[[i]][[1]]$BUGSoutput$sims.list$x, semester.results[[i]][[2]]$BUGSoutput$sims.list$x)*mult[i], 2, median)
		ideal.points[i,] <- apply (semester.results[[i]][[1]]$BUGSoutput$sims.list$x*mult[i], 2, median)
}

colors <- c("red","black","orange","blue","gold","green","gray","brown","purple")

plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-4,4), type="n", xlab="", ylab="Ideal points")
abline (h=summary (EstVarCases)[[1]][,1], lty=3, lwd=2, col=colors)
for (j in 1:9){
	lines (ideal.points[1:S,j], lwd=3, col=colors[j])
}


lines (varSemester/20)









## Version con casos diferentes en cada semestre
## JAGS VERSION
time <- c(rep(1,35), rep(2,35), rep(3,35), rep(4,35), rep(5,35), rep(6,35), rep(7,35), rep(8,35), rep(9,35), rep(10,35), rep(11,35), rep(12,35), rep(13,35), rep(14,35))
semester.results <- list () ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
x.location <- c(-2,rep(0,7),2)
x.precision  <- c(4,rep(1,7),4)

S <- 14
for (s in 1:S){        # <= BIG FUNCTION STARTS
	x.mean <- x.location
	x.tau  <- x.precision
	
	v <- YVarCases2[1:9,time==s]; ## EXTRACT VOTES
	J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
	
	ife.data <- list ("J", "I", "v", "x.mean","x.tau")
	ife.inits <- function (){
		list (
			x=rnorm(J),
			signal=rnorm(I),
			difficulty=rnorm(I)
		)
	}
	ife.parameters <- c("x", "signal", "difficulty")
	
	print(cat("\tSession no.",s,"of",S,", with", I, "votes \n"))
	
	#full run
	start.time <- proc.time()
	results <- mclapply(1:2, function(x) {
		model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
								   model.file=model1Dj.irt, n.chains=1,
								   n.iter=10000, n.burnin=5000, n.thin=50
		))
		if(inherits(model.jags.re,"try-error")) {return()}
		return(model.jags.re)
	}, mc.cores=2 )
	time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
	print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)
	
	semester.results[length(semester.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
	
	x.location  <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
	x.precision <- rep (50, 9)
}

for (i in 1:S){
	chainsConv <- mcmc.list(list (as.mcmc (semester.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (semester.results[[i]][[1]]$BUGSoutput$sims.list$x)))
	print (gelman.diag (chainsConv))
}

ideal.points.1 <- matrix (NA, nrow=S, ncol=9)
for (i in 1:S){
	ideal.points.1[i,] <- apply (rbind (semester.results[[i]][[1]]$BUGSoutput$sims.list$x, semester.results[[i]][[2]]$BUGSoutput$sims.list$x), 2, median)
}

colors <- c("red","black","orange","blue","gold","green","gray","brown","purple")

plot(c(1:S), ideal.points.1[1:S,1], main="", ylim=c(-4,4), type="n", xlab="", ylab="Ideal points")
abline (h=summary (EstVarCases)[[1]][,1], lty=3, lwd=2, col=colors)
for (j in 1:9){
	lines(smooth.spline(c(1:S), ideal.points.1[1:S,j], df=10), lwd=3, col=colors[j])
}






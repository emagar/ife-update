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


rm(list = ls())
workdir <- c("/home/eric/Dropbox/data/rollcall/ife_cg/ife-update/data/")
setwd(workdir)

# Define colors and plotting names
# OJO: en tenure term==10 es 0, term==11 es 1 etc. Si se juntaran más de 13 terms, habría que replantear la codificación... 
ids <- matrix(c("Ugalde",           "ugalde",      "PRI",  4,
                "Albo",             "albo",        "PAN",  456,
                "Andrade",          "andrade",     "PRI",  4567, 
                "Gmz. Alcántar",    "alcantar",    "PVEM", 4567,
                "Glez. Luna",       "glezluna",    "PAN",  456,
                "Latapí",           "latapi",      "PRI",  45,
                "López Flores",     "lopezflores", "PRI",  456,
                "Morales",          "morales",     "PAN",  45,
                "Sánchez",          "sanchez",     "PAN",  4567,
                "Valdés",           "valdes",      "PRD",    67890,
                "Baños",            "banos",       "PRI",    678901,
                "Nacif",            "nacif",       "PAN",    678901,
                "Elizondo",         "elizondo",    "PAN",     7890,
                "Figueroa",         "figueroa",    "PRD",     7890,
                "Guerrero",         "guerrero",    "PRI",     7890,
                "Marván",           "marvan",      "PAN",       901,
                "Córdova",          "cordova",     "PRD",       901,
                "García Rmz.",      "garcia",      "PRI",       9  ),
              ncol = 4,
              byrow = TRUE)
#
ids <- as.data.frame(ids, stringsAsFactors = FALSE)
colnames(ids) <- c("name", "column", "pty", "tenure")                                           
ids$tenure <- as.numeric(ids$tenure)
ids <- within(ids, party <- ifelse (pty=="PRI", 1, ifelse (pty=="PAN", 2, ifelse (pty=="PRD", 3, 4))))
ids <- within(ids, color <- ifelse (pty=="PRI", "red", ifelse (pty=="PAN", "blue", ifelse (pty=="PRD", "gold", "green"))))
#str(ids)
# terms 4-8
sel    <- grep(pattern = "[45678]", ids$tenure)
name   <- ids$name[sel]
party  <- ids$party[sel]
color  <- ids$color[sel]
column <- ids$column[sel]

# ... or terms 4-11
sel    <- grep(pattern = "[45678901]", ids$tenure)
name   <- ids$name[sel]
party  <- ids$party[sel]
color  <- ids$color[sel]
column <- ids$column[sel]

####################################################################################
## Read votes (includes only informative votes only, exported by code/data-prep.r ##
####################################################################################
# subset votes to given terms (and members in those terms only)
vot <-read.csv("v45678901.csv",  header=TRUE)
sel.r <- which(vot$term %in% 4:8)
drop.c <- ids$column[grep(pattern = "[45678]", ids$tenure)] # column names not in terms 4-8
drop.c <- setdiff(ids$column, drop.c)
drop.c <- which(colnames(vot) %in% drop.c)
vot <- vot[sel.r, -drop.c]
colnames(vot)
# total members
J <- length(name)

# ... or use all periods' votes 4-11
vot <-read.csv("v45678901.csv",  header=TRUE)
J <- length(name)

########################
## recode vote values ##
########################
vs <- vot[,1:J]
#table(v$albo, useNA = "always")
vs[vs==0] <- NA    ## Version probit requiere 0s y 1s
vs[vs>2] <- NA
vs[vs==2] <- 0

# format dates
vot$date <- ymd(vot$date)
# summarize then drop uncontested votes
table(factor(vot$dunan, labels = c("contested","not")), vot$term, useNA = "ifany")
table(factor(vot$dunan, labels = c("contested","not")), useNA = "ifany")
sel <- which(vot$dunan==1)
vot <- vot[-sel,] # drop uncontested votes
vs  <- vs [-sel,] # drop uncontested votes

# Make sure there are no sequences of "all missing" votes (there was one, including 20 votes on 20080111, 20080118, and 20080128, that we need to get rid off)
# There is a sequence of votes that are missing
## v.true <- apply(v, 1, invalid) # Preg. a Memo (15feb2021): dónde definió esta función invalid?
## dim (vot)
## vot <- vot[v.true==FALSE,]
## dim (vot)
# eric's version 15feb2021
nas <- function(x) ifelse(length(which(is.na(x)))==0, TRUE, FALSE)
v.true <- apply(vs, 1, nas) 
dim (vot)
vot <- vot[v.true==FALSE,]
dim (vot)
# if some deteted, they'd have to be dropped from v too
vs <- vs[v.true==FALSE,]


#############################
###     UGALDE ET AL      ###
#############################

## MODEL
model1Dj.irt <- function() {
	for (j in 1:J){                ## loop over respondents
		for (i in 1:I){              ## loop over items
			v[j,i] ~ dbern(p[j,i]);                                 ## voting rule
			probit(p[j,i]) <- mu[j,i];                              ## sets 0<p<1 as function of mu
			mu[j,i] <- signal[i]*x[j] - difficulty[i];              ## utility differential
		}
	}
	## priors ################
	for (j in 1:J){
		x[j] ~ dnorm (x.mean[j], x.tau[j]);
	}
	for (i in 1:I){
		signal[i] ~ dnorm(0, 0.1);
		difficulty[i] ~ dnorm(0, 0.25);
	}
	for (p in 1:4){
		partyPos[p] <- mean (x[party[p]]);
	}
}
#end model##############

# Establish moving windows of 30 votes each (OLD WAY)
# inicio <- c ( 1:1081 )
# final  <- c ( 30:1110 )

# Alternative: center on vote (for date), extend windows to both sides
I <- nrow (vot)
#J <- 15;
item <- 1:I  # Need to define I before
inicio <- item-15; inicio[inicio<0] <- 1
final  <- item+15; final[final>I] <- I
S <- length(inicio)
item.date <- vot$date #ymd(vot$yr*10000+vot$mo*100+vot$dy)

# Added March 19: We need a matrix showing whether each councilor is actually in IFE the moment the vote takes place
IsCouncilor <- matrix (1, ncol=J, nrow=max(final))
## IsCouncilor[ item.date > ymd("20071217"),1 ] <- NA # OLD, MISSES LATE ENTRANTS
## IsCouncilor[ item.date > ymd("20080814"),2 ] <- NA
## IsCouncilor[ item.date > ymd("20101027"),3 ] <- NA
## IsCouncilor[ item.date > ymd("20101027"),4 ] <- NA
## IsCouncilor[ item.date > ymd("20080814"),5 ] <- NA
## IsCouncilor[ item.date > ymd("20071217"),6 ] <- NA
## IsCouncilor[ item.date > ymd("20080814"),7 ] <- NA
## IsCouncilor[ item.date > ymd("20071217"),8 ] <- NA
## IsCouncilor[ item.date > ymd("20101027"),9 ] <- NA
## IsCouncilor[ item.date < ymd("20080215"),10] <- NA
## IsCouncilor[ item.date < ymd("20080215"),11] <- NA
## IsCouncilor[ item.date < ymd("20080215"),12] <- NA
## IsCouncilor[ item.date < ymd("20080829"),13] <- NA
## IsCouncilor[ item.date < ymd("20080829"),14] <- NA
## IsCouncilor[ item.date < ymd("20080829"),15] <- NA
## #IsCouncilor[ item.date < ymd("20111215"),16] <- NA
## #IsCouncilor[ item.date < ymd("20111215"),17] <- NA
## #IsCouncilor[ item.date < ymd("20111215"),18] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  4,1 ] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  6,2 ] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  7,3 ] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  7,4 ] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  6,5 ] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  5,6 ] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  6,7 ] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  5,8 ] <- NA
IsCouncilor[ vot$term < 4 & vot$term >  7,9 ] <- NA
IsCouncilor[ vot$term < 6 & vot$term > 10,10] <- NA
IsCouncilor[ vot$term < 6 & vot$term > 11,11] <- NA
IsCouncilor[ vot$term < 6 & vot$term > 11,12] <- NA
IsCouncilor[ vot$term < 7 & vot$term > 10,13] <- NA
IsCouncilor[ vot$term < 7 & vot$term > 10,14] <- NA
IsCouncilor[ vot$term < 7 & vot$term > 10,15] <- NA
#IsCouncilor[ vot$term < 9 & vot$term > 11,16] <- NA
#IsCouncilor[ vot$term < 9 & vot$term > 11,17] <- NA
#IsCouncilor[ vot$term < 9 & vot$term >  9,18] <- NA

#Da la impresión de que alrededor del voto 900 se invierte la polaridad del espacio. Para entonces los priors semi-informativos que anclaron el norte y el sur han quedado muy atrás. Quizás esto pueda arreglarse dándole a córdova un prior centrado en -2. O quizás sea posible recentrar a Baños (supongo qu es quien sube cerca del 800 y baja abruptamente) en +2 o a Figueroa (el extremo sur que se vuelve norte) en -2 poco después de la entrada de Córdova, García Ramírez y Marván.

#En la versión trimestral, anclar a Figueroa y a Córdova ambos en dnorm(-2,4) permitió producir estimaciones que aparecen en la gráfica que he guardado en el directorio correspondiente. Si fuera necesario, trate de poner a García Ramírez en dnorm(2,4).

#No agregué nuevos priors para los nuevos consejeros.  Siguen comenzando con el prior del partido que los postuló, con dos adendos: 1) La posición del partido es la mediana de sus integrantes, no la media, con el objeto de descontar extremistas.  2) La precisión del prior para los nuevos consejeros es un poco más alta: 10, en lugar de 4.

## # ESTO VIENE DE ANTES Y TIENE IMPRECISIONES
## which(vot$date==ymd("20080215"))
## [1] 209 210 211 212 213 214 215 216 217
## Salen Ugalde (PRI), Latapi (PAN), y Morales (PRI)
## Entran Valdes (PRD), Baños (PRI), Nacif (PAN)
## which (vot$date==ymd("20080829"))
## [1] 262 263 264 265
## Salen Albo (PAN), Glez. Luna (PAN), Lopez Flores (PRI)
## Entran Elizondo (PAN), Figueroa (PRD), Guerrero (PRI)
## > which (vot$date==20101022)
## [1] 721 722 723 724 725 726 727 728 729 730 731
## Salen Andrade (PRI), Alcantar (VERDE), Sanchez (PAN)
## Entran Marvan (PAN), Cordova (PRD), Garcia Ramirez (PRI)

# Initial ideal points to anchor ideological space
#                 u  a  a  a  g  l  l  m  s  v  b  n  e  f  g  m  c  g
#                 g  l  n  l  l  a  p  o  a  a  a  a  l  i  u  a  o  a
#                 a  b  d  c  z  t  z  r  n  l  ñ  c  i  g  e  r  r  r
x.location <-   c(1, 0, 0, 2,-2, 0, 0, 2,-2, 0, 0, 0, 0, 0, 0, 0, 0, 0)
x.precision  <- c(4, 1, 1, 4, 4, 1, 1, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0)
window.results <- list () ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
partyPlacement <- rep (NA,J)
x.mean <- numeric ()
x.tau  <- numeric ()

item.date[s+14]
ids
s <- 174 ## LA PRIMERA VENTANA EN QUE ENTRAN VALDÉS NACIF Y BAÑOS... SE ROMPE EL CÓDIGO. COMPARARLO CON EL DE WOLDENBERG BONICA QUE SI FUNCIONA
for (s in 174:S){        # <= BIG FUNCTION STARTS (loop over 1081 windows)

	# Added March 19: We include councilors (and their party IDs) only if they were actual councilors for at least one vote
	# This means that the length of estimated ideal points is either
	# 9 (for most votes) or 11 (when there is some overlap: two councilors are leaving , two are coming in)
	councilor.in <- apply (IsCouncilor[inicio[s]:final[s],], 2, invalid)
	councilors   <- name[councilor.in==FALSE]
	parties      <- party[councilor.in==FALSE]

	for (c in 1:15){
		x.mean[c] <- ifelse (councilor.in[c]==TRUE, NA, ifelse (!is.na(x.location[c]), x.location[c], ifelse (parties[c]==1, 2, ifelse (parties[c]==3, -2, -1))))
		x.tau[c]  <- ifelse (councilor.in[c]==TRUE, NA, ifelse (!is.na(x.precision[c]), x.precision[c], 4))
	}

	x.mean <- as.numeric (na.omit (x.mean))
	x.tau  <- as.numeric (na.omit (x.tau))

	v <- vs[inicio[s]:final[s],1:15][,councilor.in==FALSE]; ## EXTRACT 30 VOTES EACH TIME
	v <- t(v)                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
	J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS

	ife.data <- list ("J","I","v","x.mean","x.tau","party")
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
	results <-
#            mclapply(1:2, function(x) {
#		model.jags.re <- try(
                                 jags (data=ife.data, inits=ife.inits, ife.parameters,
								   model.file=model1Dj.irt, n.chains=1,
 								   n.iter=600, n.burnin=300, n.thin=3)
#								   n.iter=60000, n.burnin=30000, n.thin=300)
#		)
#		if(inherits(model.jags.re,"try-error")) {return()}
#		return(model.jags.re)
#	}, mc.cores=2 )
	time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
	print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)

	# Quick check on convergence of ideal point chains
#	GHconv <- gelman.diag(mcmc.list(list (as.mcmc (results[[2]]$BUGSoutput$sims.list$x), as.mcmc (results[[1]]$BUGSoutput$sims.list$x))))[[2]]
#	print (cat ("Gelman-Rubin R-hat:", GHconv, "\n"))

	results[[length(results)+1]] <- councilors;
#        results[[4]] <- GHconv; rm (GHconv)
 	window.results[length(window.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS

	# Update location of ideal point at time s, to be used as location prior at time s+1
	x.location  <- rep (NA, J)
	x.precision <- rep (100, J)
#	locs <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
	locs <- apply( results$BUGSoutput$sims.list$x, 2, median)
#	partyPlacement <- apply( rbind (results[[1]]$BUGSoutput$sims.list$partyPos, results[[2]]$BUGSoutput$sims.list$partyPos), 2, median)
	partyPlacement <- apply( results$BUGSoutput$sims.list$partyPos, 2, median)
	for (n in 1:15){
		if (length (which (councilors==names45678901[n]))==0) {
			x.location[n] <- NA
			x.precision[n] <- NA
		}
		else { x.location[n] <-  locs[which (councilors==names45678901[n])] }
	}
	# Precision prior is always constant at 100, implying standard deviation = sqrt (1/100) = 0.1
}  # <---   END OF LOOP OVER WINDOWS


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







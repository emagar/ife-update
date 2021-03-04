##################################################################################
# Woldenberg a la Bonica
# This code runs a Bonica-like algorithm to provide a dynamic view of Woldenberg's
# IFE.  The first run has already been saved ("posterior-samples/wold23-window-results-compress.RData")
# March 19, 2013: Add party-based priors for councilors coming in from outside
##################################################################################





library (arm)
library (MCMCpack)
library (foreign)
library (car)
library (gtools)
#library (multicore)
library (R2jags)
library (mcmcplots)
library (sm)
library (lubridate)


rm(list = ls())
workdir <- c("/home/eric/Dropbox/data/rollcall/ife_cg/ife-update/data/")
setwd(workdir)


# Define colors and plotting names
ids <- matrix(c("Woldenberg", "woldenberg", "PRI", 23,
                "Barragán",   "barragan",   "PRD", 23,
                "Cantú",      "cantu",      "PRD", 23,
                "Cárdenas",   "cardenas",   "PRD", 23,
                "Lujambio",   "lujambio",   "PAN", 23,
                "Merino",     "merino",     "PRI", 23,
                "Molinar",    "molinar",    "PAN", 2 ,
                "Peschard",   "peschard",   "PRI", 23,
                "Zebadúa",    "zebadua",    "PRD", 2 ,
                "Rivera",     "rivera",     "PRI",  3,
                "Luken",      "luken",      "PAN",  3),
              ncol = 4,
              byrow = TRUE)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
ids <- as.data.frame(ids, stringsAsFactors = FALSE)
colnames(ids) <- c("name", "column", "pty", "tenure")                                           
ids$tenure <- as.numeric(ids$tenure)
ids <- within(ids, party <- ifelse (pty=="PRI", 1,
                            ifelse (pty=="PAN", 2,
                            ifelse (pty=="PRD", 3, 
                            ifelse(pty=="PVEM", 4, 5)))))
ids <- within(ids, color <- ifelse (pty=="PRI", "red",
                            ifelse (pty=="PAN", "blue",
                            ifelse (pty=="PRD", "gold",
                            ifelse(pty=="PVEM", "green", "orangered4")))))

# select term 2, 3 or both
sel    <- grep(pattern = "[23]", ids$tenure)
name   <- ids$name[sel]
party  <- ids$party[sel]
color  <- ids$color[sel]
column <- ids$column[sel]

## rgb.23 <- c(length=11)
## rgb.23[c(1,6,8,10)] <- rgb(1,       0, 0, 0.6) #red
## rgb.23[c(2:4,9)]    <- rgb(1, 215/255, 0, 0.6) #gold
## rgb.23[c(5,7,11)]   <- rgb(0,       0, 1, 0.6) #blue

###############################################################################
## Read votes (includes informative votes only, exported by code/data-prep.r ##
###############################################################################
vot <-read.csv("v23.csv",  header=TRUE)
#
# subset to chosen periods
sel.r <- which(vot$term %in% 2:3)
drop.c <- ids$column[grep(pattern = "[23]", ids$tenure)] # column names not in terms 2-3
drop.c <- setdiff(ids$column, drop.c)
drop.c <- which(colnames(vot) %in% drop.c)
if (length(drop.c)>0) vot <- vot[sel.r, -drop.c]
colnames(vot)
# total members
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

###########################
###     WOLDENBERG      ###
###########################

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
	for (p in 1:3){
		partyPos[p] <- mean (x[party[p]]);
	}
}
#end model##############

# Center on vote (for date), extend windows to both sides
I <- nrow(vot)
item <- 1:I
inicio <- item-15; inicio[inicio<0] <- 0
final  <- item+15; final[final>I] <- I
item.date <- vot$date
S <- length(inicio)

# Added March 19: We need a matrix showing whether each councilor is actually in IFE the moment the vote takes place
IsCouncilor <- matrix (1, ncol=J, nrow=S)
IsCouncilor[item.date > ymd(20001114), c(7,9)] <- NA # Last Molinar, Zebadua vote
IsCouncilor[item.date < ymd(20010130), c(10,11)] <- NA # First Rivera, Luken vote

# Initial ideal points to anchor ideological space
x.location <- c(1,2,0,-2,rep(0,7))
x.precision  <- c(4,4,1,4,rep(1,7))
window.results <- list () ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS
partyPlacement <- rep (NA,11)
x.mean <- numeric ()
x.tau  <- numeric ()

s <- 1
for (s in 1:S){        # <= BIG FUNCTION STARTS (loop over 552 windows)

	# Added March 19: We include councilors (and their party IDs) only if they were actual councilors for at least one vote
	# This means that the length of estimated ideal points is either
	# 9 (for most votes) or 11 (when there is some overlap: two councilors are leaving , two are coming in)
	councilor.in <- apply (IsCouncilor[inicio[s]:final[s],], 2, invalid)
	councilors   <- name [councilor.in==FALSE]
	sponsors     <- party[councilor.in==FALSE]
	
	for (c in 1:11){
		x.mean[c] <- ifelse (!is.na(x.location[c]), x.location[c], partyPlacement[sponsors[c]])
		x.tau[c]  <- ifelse (!is.na(x.precision[c]), x.precision[c], 4)
	}

	v <- vs[inicio[s]:final[s], 1:11][, councilor.in==FALSE]; ## EXTRACT 30 VOTES EACH TIME
#21-02#	v[v==0] <- NA; v[v==-1] <- 0    ## Version probit requiere 0s y 1s
	v <- t(v)                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
	J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS

	ife.data <- list ("J", "I", "v", "x.mean", "x.tau", "party")
	ife.inits <- function (){
		list (
			x=rnorm(J),
			signal=rnorm(I),
			difficulty=rnorm(I)
		)
	}
	ife.parameters <- c("x", "signal", "difficulty", "partyPos")

	print(cat("Session no.", s, "of", S, ", with", I, "votes \n"))

	#full JAGS run
	start.time <- proc.time()

	# Use dual core capabilities
	results <-
#            mclapply(1:2, function(x) {
#		model.jags.re <- try(
                                 jags (data=ife.data, inits=ife.inits, ife.parameters,
#								   model.file=model1Dj.irt, n.chains=1,
								   model.file=model1Dj.irt, n.chains=2,
#								   n.iter=600, n.burnin=300, n.thin=30)
								   n.iter=50000, n.burnin=30000, n.thin=200)
#		)
#		if(inherits(model.jags.re,"try-error")) {return()}
#		return(model.jags.re)
#	}, mc.cores=2 )
	time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
	print(cat("\tTime elapsed in estimation:", time.elapsed, "minutes", "\n")); rm(time.elapsed)

	# ADD COUNCILOR NAMES TO RESULTS OBJECT
        results <- c(results, councilors=list(councilors)); # should be faster than results[[length(results)+1]] <- councilors;
        results <- c(results, folio.date=list(vot[s,c("folio","dy","mo","yr")])); # add vote on which window is centered
        window.results <- c(window.results, list(results)); # should be faster than window.results[length(window.results)+1] <- list(results) ## ADD SESSION'S RESULTS TO OBJECT HOLDING ALL RESULTS

	# Update location of ideal point at time s, to be used as location prior at time s+1
	x.location  <- rep (NA, 11)
	x.precision <- rep (100, 11)
#	locs <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
#	partyPlacement <- apply( rbind (results[[1]]$BUGSoutput$sims.list$partyPos, results[[2]]$BUGSoutput$sims.list$partyPos), 2, median)
	locs <- apply( results$BUGSoutput$sims.list$x, 2, median)
	partyPlacement <- apply( results$BUGSoutput$sims.list$partyPos, 2, median)
	for (n in 1:11){
		if (length (which (councilors==name[n]))==0) {
			x.location[n] <- NA
			x.precision[n] <- NA
		}
		else { x.location[n] <-  locs[which (councilors==name[n])] }
	}
	# Precision prior is always constant at 100, implying standard deviation = sqrt (1/100) = 0.1
}  # <---   END OF LOOP OVER WINDOWS

# rename object with posterior sims
window.results.23 <- window.results
ls()
rm(window.results)

# clean
ls()
rm(c, s, n, i, v, sel, ife.inits, ife.parameters, ife.data)
rm(councilors, sponsors, inicio, final, councilor.in)
rm(x.location, x.mean, x.precision, x.tau, item, results, item.date)
rm(color, column, locs, name, party, partyPlacement)


# save
summary(window.results)
summary(window.results[[231]]) # 9 members
summary(window.results[[232]]) # 11 members, overlap
save.image(file = "posterior-samples/wold23-window-results-compress.RData", compress = "xz")
#save(window.results.23, file = "posterior-samples/wold23-window-results-compress.RData")
x




# Save semester.results, containing all chains from all runs
# save (semester.results, file="DynWoldenbergBonicaMarch19.RData")
# save (semester.results, file="DynWoldenbergBonica.RData")

# RData file with runs carried out in Mexico, early March
# load ("DynWoldenbergBonica.RData")

# RData file with runs carried out in Wash U, March 19
# These runs omit non-sitting Councilors and party precisions for new Councilors
load ("DynWoldenbergBonicaMarch19.RData")

S <- length(semester.results)

multiGelman.hat <- numeric ()
for (i in 1:S){
	chainsConv <- mcmc.list(list (as.mcmc (semester.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (semester.results[[i]][[1]]$BUGSoutput$sims.list$x)))
	tmp <- gelman.diag (chainsConv)[[2]]
	multiGelman.hat <- c(multiGelman.hat, tmp)
}
summary (multiGelman.hat)
rm (tmp, chainsConv)

CouncilorIn <- matrix (1, nrow=11, ncol=S)
#CouncilorIn[c(7,9),  all23$date[-c(1:29)] > 20001114] <- NA # Last Molinar, Zebadua vote
#CouncilorIn[c(10,11),all23$date[-c(1:29)] < 20010130] <- NA # First Rivera, Luken vote
CouncilorIn[c(7,9),  item.date > ymd(20001114)] <- NA # Last Molinar, Zebadua vote
CouncilorIn[c(10,11), item.date < ymd(20010130)] <- NA # First Rivera, Luken vote

# If using DynWoldenbergBonica.RData, use the following code to extract ideal points
ideal.points <- matrix (NA, nrow=S, ncol=11)
for (i in 1:S){
	ideal.points[i,] <- apply (rbind (semester.results[[i]][[1]]$BUGSoutput$sims.list$x, semester.results[[i]][[2]]$BUGSoutput$sims.list$x), 2, median)
}

# If using DynWodenbergBonicaMarch19.RData, use the following code to extract ideal points
ideal.points <- matrix (NA, nrow=S, ncol=11)
ideal.points.var <- matrix (NA, nrow=S, ncol=11)
for (i in 1:S){
 	for (j in 1:11){
 		councilor <- name[j]
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

# Get SDs of estimates, the width should be useful to plot thickness of data points
ideal.points.var <- sqrt (ideal.points.var)

# Non-smoothed ideal point time-paths
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:11){
	lines(CouncilorIn[j,1:S] * ideal.points[1:S,j], lwd=4, col=color.23[j])
}

# Item indices closest to federal elections
fedEls.items <- c(
min(which(abs(item.date-ymd(19970706))==min(abs(item.date-ymd(19970706))))),
min(which(abs(item.date-ymd(20000702))==min(abs(item.date-ymd(20000702))))),
min(which(abs(item.date-ymd(20030706))==min(abs(item.date-ymd(20030706)))))
#min(which(abs(item.date-ymd(20060702))==min(abs(item.date-ymd(20060702))))),
#min(which(abs(item.date-ymd(20090705))==min(abs(item.date-ymd(20090705))))),
#min(which(abs(item.date-ymd(20120701))==min(abs(item.date-ymd(20120701)))))
    )

# Item indices closest to New Years
newYear.items <- c(
min(which(abs(item.date-ymd(19970101))==min(abs(item.date-ymd(19970101))))),
min(which(abs(item.date-ymd(19980101))==min(abs(item.date-ymd(19980101))))),
min(which(abs(item.date-ymd(19990101))==min(abs(item.date-ymd(19990101))))),
min(which(abs(item.date-ymd(20000101))==min(abs(item.date-ymd(20000101))))),
min(which(abs(item.date-ymd(20010101))==min(abs(item.date-ymd(20010101))))),
min(which(abs(item.date-ymd(20020101))==min(abs(item.date-ymd(20020101))))),
min(which(abs(item.date-ymd(20030101))==min(abs(item.date-ymd(20030101)))))
## min(which(abs(item.date-ymd(20040101))==min(abs(item.date-ymd(20040101))))),
## min(which(abs(item.date-ymd(20050101))==min(abs(item.date-ymd(20050101))))),
## min(which(abs(item.date-ymd(20060101))==min(abs(item.date-ymd(20060101))))),
## min(which(abs(item.date-ymd(20070101))==min(abs(item.date-ymd(20070101))))),
## min(which(abs(item.date-ymd(20080101))==min(abs(item.date-ymd(20080101))))),
## min(which(abs(item.date-ymd(20090101))==min(abs(item.date-ymd(20090101))))),
## min(which(abs(item.date-ymd(20100101))==min(abs(item.date-ymd(20100101))))),
## min(which(abs(item.date-ymd(20110101))==min(abs(item.date-ymd(20110101))))),
## min(which(abs(item.date-ymd(20120101))==min(abs(item.date-ymd(20120101)))))
    )

# Item indices for entry/exit from council
inNout <- matrix (NA, nrow=11, ncol=S)
for (j in 1:11){
  inNout[j,min(which(CouncilorIn[j,]==1))] <- min(which(CouncilorIn[j,]==1))
  inNout[j,max(which(CouncilorIn[j,]==1))] <- max(which(CouncilorIn[j,]==1))
}

# Function captures smooth.spline ideal point coordinates: Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s] give vote s's
Smooth <- list ()
for (j in 1:11){
#	Smooth[[j]] <- smooth.spline(c(1:S), ideal.points[,j], df=10)
        Smooth[[j]] <- smooth.spline(c(1:S)[!is.na(CouncilorIn[j,1:S])], ideal.points[!is.na(CouncilorIn[j,1:S]),j], df=10)
}

for (j in 1:11){
	Smooth[[j]]$y[is.na(CouncilorIn[j,1:S])] <- NA
}

# Smoothed ideal point time-paths
err <- ideal.points.var*2/max(ideal.points.var) ## estimate error
spaghetti.graph <- function(progress=S){
#pdf(paste(graphdir,"woldBonicaSmoothError.pdf",sep=""), width=7, height=7)
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="Divided vote", ylab="Ideal points")
axis(3, at=newYear.items, labels = 1997:2003, cex.axis=.6)
abline(v=fedEls.items, lty=3, col="grey50")
text(rep(fedEls.items, times=2), c(rep(3.1,times=3),rep(2.925,times=3)), c("midterm","presidential","midterm",rep("election",3)), adj=0, cex=.65, pos=1, col="grey30")
for (j in 1:11){
       points(1:progress, Smooth[[j]]$y[1:progress], cex=.3, col=color.23[j])
#       points(1:progress, Smooth[[j]]$y[1:progress], cex=err[,j], col=color.23[j])
#	lines(1:progress, Smooth[[j]]$y[1:progress], lwd=3, col=color.23[j])
#        points(inNout[j,], Smooth[[j]]$y, pch=19, col=color.23[j])
}
text(-15,Smooth[[2]]$y[1]+.15,c("Barragán"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[1]]$y[1]+.4,c("Woldenberg"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[1]]$y[1]+.25,c("Peschard"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[1]]$y[1]+.1,c("Merino"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[7]]$y[30],c("Molinar"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[9]]$y[15],c("Zebadúa"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[5]]$y[1]-.25,c("Lujambio"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[5]]$y[1]-.4,c("Cantú"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[4]]$y[70],c("Cárdenas"), pos=4, adj=0, cex=.75, col="black")
text(233-15,Smooth[[10]]$y[233]-.125,c("Rivera"), pos=4, adj=0, cex=.75, col="black")
text(233-15,Smooth[[11]]$y[233]+.075,c("Luken"), pos=4, adj=0, cex=.75, col="black")
#dev.off()
}

spaghetti.graph()

# Smoothed ideal point time-paths, daily change movie
snapshot <- seq (3, 553, by=5)
for (s in snapshot){
	which.s <- which (snapshot==s)
	if (which.s < 10) {name = paste('Wolden00', which.s,'plot.png',sep='')}
	if (which.s >= 10 && which.s < 100) {name = paste('Wolden0', which.s,'plot.png', sep='')}
	if (which.s >= 100) {name = paste('Wolden', which.s,'plot.png', sep='')}

	jpeg (paste(graphdir, "animBits/", name, sep=""), quality=100, height=500, width=500)

        spaghetti.graph(s)

	## plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
	## for (j in 1:11){
	## 	lines ( Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s], lwd=6, col=color.23[j])
	## }
	legend ("bottomright", bty="n", xjust=0, legend=paste ("vote date:", item.date[s], sep=" ")) #Change the legend for the date of the vote
	dev.off()
}

# Make a short film
# The number after loop controls the number of automatic replays (0 stands for infinite loop)
# The number after delay determines length of transition between slides)
setwd(paste(graphdir,"animBits/", sep=""))
system ("convert -loop 1 -delay 20 *.png IFEwoldenTheMovie.gif")
setwd(workdir)

###########################
#  Regression analysis
# Identify first Sunday in July for election years
election.dates <- c(19970706,20000702,20030706,20060702,20090705,20120701)
ymd (20130308) - ymd (20130307)

Dates <- matrix (NA, nrow=length(all23$date), ncol=length(election.dates))
for (i in 1:6){
	for (j in 1:length(all23$date)){
		Dates[j,i] <- ymd (election.dates[i]) - ymd (all23$date[j])
	}
}

Date2NextElection <- numeric ()
for (i in 1:length(all23$date)){
	Date2NextElection[i] <- min (Dates[i,][Dates[i,]>0])
}
rm (Dates)
Date2NextElection <- Date2NextElection[-c(1:29)]

Ideal.Points <- matrix (NA, ncol=ncol(ideal.points), nrow=nrow(ideal.points))
for (j in 1:ncol(ideal.points)){
	Ideal.Points[,j] <- CouncilorIn[j,] * ideal.points[,j]
}

WD.PRI <- WD.PRD <- WD.PAN <- numeric ()
for (i in 1:nrow(Ideal.Points)){
	WD.PRI[i]  <- max(Ideal.Points[i,color.23=="red"], na.rm=T) - min(Ideal.Points[i,color.23=="red"], na.rm=T)
	WD.PRD[i]  <- max(Ideal.Points[i,color.23=="gold"], na.rm=T) - min(Ideal.Points[i,color.23=="gold"], na.rm=T)
	WD.PAN[i]  <- max(Ideal.Points[i,color.23=="blue"], na.rm=T) - min(Ideal.Points[i,color.23=="blue"], na.rm=T)
}


AV.PRI <- AV.PRD <- AV.PAN <- numeric ()
for (i in 1:nrow(Ideal.Points)){
	AV.PRI[i] <- mean(Ideal.Points[i,color.23=="red"], na.rm=T)
	AV.PRD[i] <- mean(Ideal.Points[i,color.23=="gold"], na.rm=T)
	AV.PAN[i] <- mean(Ideal.Points[i,color.23=="blue"], na.rm=T)
}

PAN.PRD <- PRD.PRI <- PRI.PAN <- numeric ()
for (i in 1:nrow(Ideal.Points)){
	PAN.PRD[i] <- AV.PAN[i] - AV.PRD[i]
	PRD.PRI[i] <- AV.PRD[i] - AV.PRI[i]
	PRI.PAN[i] <- AV.PRI[i] - AV.PAN[i]
}

mod <- lm (PRI.PAN ~ Date2NextElection + I(Date2NextElection^2))
plot (PRI.PAN, type="l", lwd=2)
abline (v=c(163,493))

mod <- lm (PAN.PRD ~ Date2NextElection + I(Date2NextElection^2))
plot (PAN.PRD, type="l", lwd=2)
abline (v=c(163,493))

mod <- lm (PRD.PRI ~ Date2NextElection + I(Date2NextElection^2))
plot (PRD.PRI, type="l", lwd=2)
abline (v=c(163,493))


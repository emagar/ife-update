#########################################################################
####  GRAFICA DE VOTOS TOTALES Y DIVIDIDOS POR TRIMESTRE 1997--2012  ####
#########################################################################
rm(list = ls())
workdir <- c("~/Dropbox/data/rollcall/ife_cg")
graphdir <- paste(workdir, "graphs", sep="/")
#
setwd(paste(workdir, "graphs/", sep="/"))
#pdf(file="all+divVotsQuarter.pdf",width=7, height=4)
par(mar=c(5,4,0,1)+0.1)  ## USA EL ESPACIO DEL TITULO INEXISTENTE
#
yrqr <- paste(as.integer( seq(from=1997, to=2012.75, by=.25)), c("q1","q2","q3","q4"), sep="")
yrsm <- paste(as.integer( seq(from=1997, to=2012.75, by=.5)),  c("s1","s2"), sep="")
#
## OLD COUNT vot <- c(95, 59, 37, 47, 40, 90, 123, 155, 51, 59, 55, 103, 125,
##         147, 59,  54, 48, 98, 116, 145, 178, 323)
## OLD COUNT div <- c(15,7,19,20,23,35,38,89,31,36,36,57,85,91,23,16,9,17,20,62,
##         18,23)
votsm <- c(95, 59, 37, 47, 40, 90, 123, 116, 90, 59, 55, 103, 125, 147, 59, 54, 48, 98, 116, 145, 178, 182, 231, 547, 718, 485, 280, 368, 192, 353, 517, 544)
divsm <- c(15 , 7 , 19 , 20 , 23 , 35 , 38 , 60 , 60 , 36 , 36 , 57 , 85 , 91 , 23 , 16 , 9 , 17 , 20 , 62 , 18 , 19 , 43 , 63 , 91 , 137 , 57 , 156 , 35 , 88 , 146 , 110)
votqr <- c(58 , 37 , 52 , 7 , 20 , 17 , 22 , 25 , 22 , 18 , 29 , 61 , 47 , 76 , 87 , 29 , 40 , 50 , 9 , 50 , 24 , 31 , 39 , 64 , 62 , 63 , 64 , 83 , 18 , 41 , 32 , 22 , 11 , 37 , 40 , 58 , 59 , 57 , 137 , 8 , 66 , 112 , 128 , 54 , 153 , 78 , 331 , 216 , 527 , 191 , 268 , 217 , 153 , 127 , 239 , 129 , 86 , 106 , 202 , 151 , 192 , 325 , 327 , 217)
divqr <- c(11 , 4 , 7 , 0, 15 , 4 , 4 , 16 , 13 , 10 , 9 , 26 , 16 , 22 , 37 , 23 , 29 , 31 , 7 , 29 , 19 , 17 , 24 , 33 , 46 , 39 , 38 , 53 , 1 , 22 , 9 , 7 , 2 , 7 , 3 , 14 , 11 , 9 , 60 , 2 , 1 , 17 , 2 , 17 , 16 , 27 , 27 , 36 , 26 , 65 , 88 , 49 , 28 , 29 , 114 , 42 , 16 , 19 , 69 , 19 , 48 , 98 , 63 , 47)
#
vot <- votqr; div <- divqr ## choose appropriate
#vot[49] <- 375 ## max(vot) way off, break column
plot(1:length(vot),c(rep(0,(length(vot)-1)),max(vot)),type="n",xlab="quarter",
    ylab="votes", main=NULL, axes=FALSE)
#mtext("votes           ",4,col="grey55")
#pct.div <- divqr*100/votqr
#redux <- 13
#vot.mod <- vot/redux - 25/redux # vot/4.5 -25/4.5
lines(c(17,17),  c(0,530),lty=3)     ## quarter periods
lines(c(28.5,28.5),  c(0,530),lty=3) ## quarter periods
lines(c(46,46),  c(0,530),lty=3)     ## quarter periods
lines(c(48,48),  c(0,530),lty=3)     ## quarter periods
lines(c(56.5,56.5),  c(0,530),lty=3)     ## quarter periods
lines(c(61,61),  c(0,530),lty=3)     ## quarter periods
text(17/2,    540,"I")               ## quarter periods
text((17+29)/2, 540,"II")            ## quarter periods
text((29+46)/2, 540,"III")           ## quarter periods
text((46+48)/2, 540,"IV")            ## quarter periods
text((48+56)/2, 540,"V")             ## quarter periods
text((56+61)/2, 540,"VI")            ## quarter periods
text((61+64)/2, 540,"VII")            ## quarter periods
## IN AND OUT SUMMARY QUARTERS
#        event         date       qtr (progress)  qr_count
# I      Molinar out   5/12/2000  2001.1 (35%)    17
# II     Luken in     11/12/2000  2001.1 (40%)    17
#        All Wold out 31/10/2003  2003.4 (100%)   28
# III    All Ugalde in 3/11/2003  2004.1 (0%)     29 or 1
#        Ugalde out   14/12/2007  2008.1 (50%)    45 or 17
#        Latapí out     7/2/2008  2008.2 (10%)    46 or 18
#        Morales out    7/2/2008  2008.1 (50%)    46 or 18
# IV     Valdés in      8/2/2008  2008.2 (10%)    46 or 18
#        Nacif in       8/2/2008  2008.2 (10%)    46 or 18
#        Baños in       8/2/2008  2008.2 (10%)    46 or 18
#        Albo out      14/8/2008  2008.4 (20%)    48 or 20
#        GlezLuna out  14/8/2008  2008.4 (20%)    48 or 20
#        LpzFlores out 14/8/2008  2008.4 (20%)    48 or 20
# V      Figueroa in   15/8/2008  2008.4 (20%)    48 or 20
#        Guerrero in   15/8/2008  2008.4 (20%)    48 or 20
#        Elizondo in   15/8/2008  2008.4 (20%)    48 or 20
# VI     Sánchez out  31/10/2010  2010.4 (100%)   56 or 28
#        Andrade out  31/10/2010  2010.4 (100%)   56 or 28
#        Alcántar out 31/10/2010  2010.4 (100%)   56 or 28
# VII    Córdova in   15/11/2011  2012.1 (20%)    61 or 33
#        Marván in    15/11/2011  2012.1 (20%)    61 or 33
#        GarcíaRmz in 15/11/2011  2012.1 (20%)    61 or 33
# VIII   G. Rmz out     8/2/2013  2013.2 (10%)    65 or 37
axis(1, at=c(1:64), labels = FALSE)
axis(1, tick=FALSE, cex.axis=.35, at=c(1:64), labels = c(4,rep(1:4,15),1,2,3), line=-0.8) #axis(1, tick=FALSE, cex.axis=.35, at=c(1:64), labels = rep(1:4,16), line=-0.8)
axis(1, tick= FALSE,cex.axis=.675, at=seq(from=3.5, to=65.5, by=4),
     labels = c("1997","'98","'99","2000","'01","'02","'03","'04","'05","'06","'07","'08","'09","'10","'11", "2012"))
axis(2, at=25*(0:21), labels = FALSE)
axis(2, tick= FALSE,cex.axis=.75, at=100*(0:5))
abline(h=25*(0:21), col="grey")  #abline(h=25*(0:14), col="grey")
wi <- .3
for (i in 1:64){
  polygon(c(rep(i-wi,2),rep(i+wi,2)), c(0,rep(vot[i],2),0), col = "grey", border = "grey")
  polygon(c(rep(i-wi,2),rep(i+wi,2)), c(0,rep(div[i],2),0), col = "black", border = "black")
}
#axis(4, at=c(25/redux,50*(1:18)/redux)-25/redux, labels = FALSE, line=-2.5,col="grey55")
#axis(4, tick= FALSE,cex.axis=.75, at=c(25/redux,100*(1:9)/redux)-25/redux,labels=c(25,100*(1:9)),line=-3,col.axis="grey55")
#lines(1:28,vot.mod,col="grey55",lwd=2)
#lines(1:28,pct.div,lwd=2)
text(c(3,15,27,39,51,63), rep(-10,6), c("*"))
#dev.off()
setwd(workdir)

########################################################################
####  GRAFICA DE VOTOS TOTALES Y DIVIDIDOS POR SEMESTRE 1997--2012  ####
########################################################################
setwd(paste(workdir, "graphs/", sep="/"))
pdf(file="all+divVotsSemester.pdf",width=7, height=4)
par(mar=c(5,4,0,1)+0.1)  ## USA EL ESPACIO DEL TITULO INEXISTENTE
#
yrqr <- paste(as.integer( seq(from=1997, to=2012.75, by=.25)), c("q1","q2","q3","q4"), sep="")
yrsm <- paste(as.integer( seq(from=1997, to=2012.75, by=.5)),  c("s1","s2"), sep="")
#
## OLD COUNT vot <- c(95, 59, 37, 47, 40, 90, 123, 155, 51, 59, 55, 103, 125,
##         147, 59,  54, 48, 98, 116, 145, 178, 323)
## OLD COUNT div <- c(15,7,19,20,23,35,38,89,31,36,36,57,85,91,23,16,9,17,20,62,
##         18,23)
votsm <- c(95, 59, 37, 47, 40, 90, 123, 116, 90, 59, 55, 103, 125, 147, 59, 54, 48, 98, 116, 145, 178, 182, 231, 547, 718, 485, 280, 368, 192, 353, 517, 544)
divsm <- c(15 , 7 , 19 , 20 , 23 , 35 , 38 , 60 , 60 , 36 , 36 , 57 , 85 , 91 , 23 , 16 , 9 , 17 , 20 , 62 , 18 , 19 , 43 , 63 , 91 , 137 , 57 , 156 , 35 , 88 , 146 , 110)
votqr <- c(58 , 37 , 52 , 7 , 20 , 17 , 22 , 25 , 22 , 18 , 29 , 61 , 47 , 76 , 87 , 29 , 40 , 50 , 9 , 50 , 24 , 31 , 39 , 64 , 62 , 63 , 64 , 83 , 18 , 41 , 32 , 22 , 11 , 37 , 40 , 58 , 59 , 57 , 137 , 8 , 66 , 112 , 128 , 54 , 153 , 78 , 331 , 216 , 527 , 191 , 268 , 217 , 153 , 127 , 239 , 129 , 86 , 106 , 202 , 151 , 192 , 325 , 327 , 217)
divqr <- c(11 , 4 , 7 , 0, 15 , 4 , 4 , 16 , 13 , 10 , 9 , 26 , 16 , 22 , 37 , 23 , 29 , 31 , 7 , 29 , 19 , 17 , 24 , 33 , 46 , 39 , 38 , 53 , 1 , 22 , 9 , 7 , 2 , 7 , 3 , 14 , 11 , 9 , 60 , 2 , 1 , 17 , 2 , 17 , 16 , 27 , 27 , 36 , 26 , 65 , 88 , 49 , 28 , 29 , 114 , 42 , 16 , 19 , 69 , 19 , 48 , 98 , 63 , 47)
#
vot <- votsm; div <- divsm ## choose appropriate
plot(1:length(vot),c(rep(0,(length(vot)-1)),max(vot)+25),type="n",xlab="semester",
    ylab="votes", main=NULL, axes=FALSE)
#mtext("votes           ",4,col="grey55")
#pct.div <- divqr*100/votqr
#redux <- 13
#vot.mod <- vot/redux - 25/redux # vot/4.5 -25/4.5
lines(c(9,9),  c(0,725),lty=3) ## semester periods
lines(c(14.5,14.5),c(0,725),lty=3) ## semester periods
lines(c(23,23),c(0,725),lty=3)     ## semester periods
lines(c(24,24),c(0,725),lty=3)     ## semester periods
lines(c(28.5,28.5),c(0,725),lty=3) ## semester periods
lines(c(31,31),c(0,725),lty=3)     ## semester periods
text(9/2,      745, "I")           ## semester periods
text((9+14)/2, 745, "II")          ## semester periods
text((14+23)/2, 745, "III")        ## semester periods
text((23+24)/2, 745, "IV")         ## semester periods
text((24+28.5)/2, 745, "V")        ## semester periods
text((28.5+31)/2, 745, "VI")       ## semester periods
text((31+32)/2, 745, "VII")        ## semester periods
#        event         date       sem (progress)  sm_count
# I      Molinar out   5/12/2000  2001.1 (15%)    9
#        Zebadúa out   5/12/2000  2001.1 (15%)    9
# II     Luken in     11/12/2000  2001.1 (16%)    9
#        Rivera in    11/12/2000  2001.1 (16%)    9
#        All Wold out 31/10/2003  2003.2 (100%)   14
# III    All Ugalde in 3/11/2003  2004.1 (0%)     15 or 1
#        Ugalde out   14/12/2007  2008.1 (17%)    23 or 9
#        Latapí out     7/2/2008  2008.1 (50%)    23 or 9
#        Morales out    7/2/2008  2008.1 (50%)    23 or 9
# IV     Valdés in      8/2/2008  2008.1 (50%)    23 or 9
#        Nacif in       8/2/2008  2008.1 (50%)    23 or 9
#        Baños in       8/2/2008  2008.1 (50%)    23 or 9
#        Albo out      14/8/2008  2008.2 (40%)    24 or 10
#        GlezLuna out  14/8/2008  2008.2 (40%)    24 or 10
#        LpzFlores out 14/8/2008  2008.2 (40%)    24 or 10
# V      Figueroa in   15/8/2008  2008.2 (40%)    24 or 10
#        Guerrero in   15/8/2008  2008.2 (40%)    24 or 10
#        Elizondo in   15/8/2008  2008.2 (40%)    24 or 10
# VI     Sánchez out  31/10/2010  2010.2 (100%)   28 or 14
#        Andrade out  31/10/2010  2010.2 (100%)   28 or 14
#        Alcántar out 31/10/2010  2010.2 (100%)   28 or 14
# VII    Córdova in   15/11/2011  2012.1 (10%)    31 or 17
#        Marván in    15/11/2011  2012.1 (10%)    31 or 17
#        GarcíaRmz in 15/11/2011  2012.1 (10%)    31 or 17
# VIII   GarcíaRmz out  8/2/2013  2013.1 (50%)    33 or 19
axis(1, at=c(1:32), labels = FALSE)
axis(1, tick=FALSE, cex.axis=.55, at=c(1:32), labels = rep(1:2,16), line=-0.8)
axis(1, tick= FALSE,cex.axis=.675, at=seq(from=1.5, to=31.5, by=2),
     labels = c("1997","'98","'99","2000","'01","'02","'03","'04","'05","'06","'07","'08","'09","'10","'11", "2012"))
axis(2, at=50*(0:14), labels = FALSE)
axis(2, tick= FALSE,cex.axis=.75, at=100*(0:7))
abline(h=50*(0:14), col="grey")  #abline(h=25*(0:14), col="grey")
wi <- .3
for (i in 1:32){
  polygon(c(rep(i-wi,2),rep(i+wi,2)), c(0,rep(vot[i],2),0), col = "grey", border = "grey")
  polygon(c(rep(i-wi,2),rep(i+wi,2)), c(0,rep(div[i],2),0), col = "black", border = "black")
}
#axis(4, at=c(25/redux,50*(1:18)/redux)-25/redux, labels = FALSE, line=-2.5,col="grey55")
#axis(4, tick= FALSE,cex.axis=.75, at=c(25/redux,100*(1:9)/redux)-25/redux,labels=c(25,100*(1:9)),line=-3,col.axis="grey55")
#lines(1:28,vot.mod,col="grey55",lwd=2)
#lines(1:28,pct.div,lwd=2)
text(c(3,15,27,39,51,63), rep(-15,6), c("*"))
dev.off()
setwd(workdir)



####################################################
####################################################
##############  SPAGHETTI  PLOTS  ##################
####################################################
####################################################


###############################################################
##   Woldenberg trimestral 1996-2003 (viene de dynGraphs.r)  ##
###############################################################
rm(list = ls())
workdir <- c("~/Dropbox/ifeSharedGE/data")
graphdir <- c("~/Dropbox/ifeSharedGE/graphs")
setwd(workdir)
load("DynWmartinQuinnTrimestresLucia.RData")
#
wo.results <- ife.23; rm(ife.23)
S <- wo.results$n.sims # N OF SAMPLED SIMULATIONS
I <- nrow(all23); J <- 11; T <- 28
all23 <- read.csv("tmp23.csv")
#
# define name and color vectors for graphs
names.23 <- c("Woldenberg", "Barragán", "Cantú", "Cárdenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebadúa", "Rivera", "Luken")
#names.23 <- c("Woldenberg", "Barragan", "Cantu", "Cardenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebadua", "Rivera", "Luken")
color.23 <- c("red", "gold", "orange", "gold",  "blue", "red", "blue", "red", "gold", "red", "blue")
#color.23 <- c("red", "gold", "gold"  , "gold",  "blue", "red", "blue", "red", "gold", "red", "blue")
## All posterior ideal point simulations by quarter
xx <- array (NA, dim=c(S, J, T))
xx[,,1] <-  wo.results$sims.list$x1
xx[,,2] <-  wo.results$sims.list$x2 
xx[,,3] <-  wo.results$sims.list$x3 
xx[,,4] <-  wo.results$sims.list$x4 
xx[,,5] <-  wo.results$sims.list$x5 
xx[,,6] <-  wo.results$sims.list$x6 
xx[,,7] <-  wo.results$sims.list$x7 
xx[,,8] <-  wo.results$sims.list$x8 
xx[,,9] <-  wo.results$sims.list$x9 
xx[,,10] <- wo.results$sims.list$x10
xx[,,11] <- wo.results$sims.list$x11
xx[,,12] <- wo.results$sims.list$x12
xx[,,13] <- wo.results$sims.list$x13
xx[,,14] <- wo.results$sims.list$x14
xx[,,15] <- wo.results$sims.list$x15
xx[,,16] <- wo.results$sims.list$x16
xx[,,17] <- wo.results$sims.list$x17
xx[,,18] <- wo.results$sims.list$x18
xx[,,19] <- wo.results$sims.list$x19
xx[,,20] <- wo.results$sims.list$x20
xx[,,21] <- wo.results$sims.list$x21
xx[,,22] <- wo.results$sims.list$x22
xx[,,23] <- wo.results$sims.list$x23
xx[,,24] <- wo.results$sims.list$x24
xx[,,25] <- wo.results$sims.list$x25
xx[,,26] <- wo.results$sims.list$x26
xx[,,27] <- wo.results$sims.list$x27
xx[,,28] <- wo.results$sims.list$x28
# Thin Lucía's results further
xx <- xx[(1:300)*10,,]
## TRIMESTRES CON CONSEJEROS NUEVOS Y VIEJOS PRESENTES EN t=17
qs <- c(.025,.5,.975) ## quantiles you wish to report
ideal.points <- array(NA, dim = c(T,3,J)); dimnames(ideal.points) <- list(NULL, qs, NULL);
for (t in 1:T){
 	for (j in 1:J){
          ideal.points[t,,j] <- quantile( xx[,j,t], probs = qs, names = FALSE )
        }
      }
ideal.points[18:T,,c(7,9)] <- rep(NA,3)
ideal.points[1:16,,c(10,11)] <- rep(NA,3)
#
# EXPECTED MEDIAN
xx[,c(7,9),18:T] <- NA; xx[,c(10,11),1:16] <- NA
exp.med <- rep(NA, T)
for (t in 1:T){
  exp.med[t] <- median( xx[,,t][!is.na(xx[,,t])] )
}
rm(xx)
#
#TODOS LOS CONSEJEROS EN COLOR
#pdf(file=paste(graphdir, "allWdynQuarter.pdf", sep="/"), width = 7, height = 7)
par(mar=c(5,4,0,1)+0.1)  ## USA EL ESPACIO DEL TITULO INEXISTENTE
plot(1:T,c(rep(-1,times=(T-1)),1.1),type="n",xlab="quarter",
    ylab="ideal point       ", axes="FALSE")#, main="Woldenberg Council")
#    xaxp=c(1,14,13), yaxp=c(-.6,.6,6), axes="FALSE", main="Woldenberg Council")
axis(1, at=c(1:T), labels = FALSE)
axis(1, tick= FALSE, cex.axis=.55, at=c(1:T), labels = c(4, rep(1:4,6), 1,2,3), line=-0.8)
axis(1, tick= FALSE, cex.axis=.9, at=seq(from=3.5,to=27.5,by=4), labels = c("1997","'98","'99","2000","'01","'02","'03") )
axis(2, at=c(-1,-.5,0,.5,1), labels = FALSE)
axis(2, tick= FALSE, cex.axis=.9, at=c(-1,0,1), labels = TRUE )
#lines(c(2,2),c(.9,-.9),lwd=1,lty=3, col="grey50")
#lines(c(8,8),c(.9,-.9),lwd=1,lty=3, col="grey50")
#lines(c(14,14),c(.9,-.9),lwd=1,lty=3, col="grey50")
lines(c(17,17),c(1.1,-2),lwd=1,lty=3, col="grey50")
text(c(17,17),c(1,.95),c("New ","members "), adj=1, cex=.65, col="grey50")
#
inNout <- matrix(1, nrow=11, ncol=2); inNout[,2] <- 28; inNout[c(7,9),2] <- 17; inNout[c(10,11),1] <- 17
for (j in 1:11){
  lines(spline(1:T, ideal.points[,2,j]), lwd=3, col = color.23[j])
  points(spline((1:T)[inNout[j,]], ideal.points[,2,j][inNout[j,]], n=2), pch=19, cex=1, col = color.23[j])
}
text(.25, ideal.points[1,2,2]+.05, c("Barragán"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,1]+.15, c("Woldenberg"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,1]+.1, c("Peschard"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,1]+.05, c("Merino"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,9]+.15, c("Zebadúa"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,9]+.1, c("Lujambio"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,9]+.05, c("Molinar"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,3]-.06, c("Cantú"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,4]-.06, c("Cárdenas"), pos=4, adj=0, cex=.8, col="black")
text(16.25, ideal.points[17,2,10]+.12, c("Rivera"), pos=4, adj=0, cex=.8, col="black")
text(16.25, ideal.points[17,2,10]+.07, c("Luken"), pos=4, adj=0, cex=.8, col="black")
for (t in 1:T){
  points(t, exp.med[t], pch=17, cex=.5)
}
text(3,-1.05,c("*"))
text(15,-1.05,c("*"))
text(27,-1.05,c("*"))
text(9,1.15,c("I"))
text(23,1.15,c("II"))
#dev.off()



##########################################################################
##   Ugalde Albo Valdés trimestral 2004--2012  (viene de dynGraphs.r)   ##
##########################################################################
rm(list=ls())
workdir <-  "~/Dropbox/ifeSharedGE/data"
graphdir <- "~/Dropbox/ifeSharedGE/graphs"
#
# define name and color vectors for graphs
names.456789 <- c("Ugalde", "Albo", "Andrade", "Gmz.Alcántar", "Glez.Luna", "Latapí", "Lpz.Flores", "Morales", "Sánchez", "Valdés", "Baños", "Nacif", "Elizondo", "Figueroa", "Guerrero", "Marván", "Córdova", "García.Rmz")
color.456789 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red",  "blue", "gold", "red")
#
setwd(workdir)
load ("DynUAVmartinQuinnTrimestres2003-2011lucia.Rdata")
#
ug.results <- ife.45678; rm(ife.45678)
all45678 <- read.csv("tmp456789.csv")
all45678 <- all45678[all45678$term<9,] ## Drop term==9 which Lucía did not include
#
S <- ug.results$n.sims # N OF SAMPLED SIMULATIONS
I <- nrow(all45678); J <- 15; T <- 33
#
## All posterior ideal point simulations by quarter
xx <- array (NA, dim=c(S, J, T))
xx[,,1] <-  ug.results$sims.list$x1
xx[,,2] <-  ug.results$sims.list$x2 
xx[,,3] <-  ug.results$sims.list$x3 
xx[,,4] <-  ug.results$sims.list$x4 
xx[,,5] <-  ug.results$sims.list$x5 
xx[,,6] <-  ug.results$sims.list$x6 
xx[,,7] <-  ug.results$sims.list$x7 
xx[,,8] <-  ug.results$sims.list$x8 
xx[,,9] <-  ug.results$sims.list$x9 
xx[,,10] <- ug.results$sims.list$x10
xx[,,11] <- ug.results$sims.list$x11
xx[,,12] <- ug.results$sims.list$x12
xx[,,13] <- ug.results$sims.list$x13
xx[,,14] <- ug.results$sims.list$x14
xx[,,15] <- ug.results$sims.list$x15
xx[,,16] <- ug.results$sims.list$x16
xx[,,17] <- ug.results$sims.list$x17
xx[,,18] <- ug.results$sims.list$x18
xx[,,19] <- ug.results$sims.list$x19
xx[,,20] <- ug.results$sims.list$x20
xx[,,21] <- ug.results$sims.list$x21
xx[,,22] <- ug.results$sims.list$x22
xx[,,23] <- ug.results$sims.list$x23
xx[,,24] <- ug.results$sims.list$x24
xx[,,25] <- ug.results$sims.list$x25
xx[,,26] <- ug.results$sims.list$x26
xx[,,27] <- ug.results$sims.list$x27
xx[,,28] <- ug.results$sims.list$x28
xx[,,29] <- ug.results$sims.list$x29
xx[,,30] <- ug.results$sims.list$x30
xx[,,31] <- ug.results$sims.list$x31
xx[,,32] <- ug.results$sims.list$x32
xx[,,33] <- ug.results$sims.list$x33
# Thin Lucía's results further
xx <- xx[(1:300)*10,,]
## TRIMESTRES CON CONSEJEROS NUEVOS Y VIEJOS PRESENTES EN t=17
qs <- c(.025,.5,.975) ## quantiles you wish to report
ideal.points <- array(NA, dim = c(T,3,J)); dimnames(ideal.points) <- list(NULL, qs, NULL);
for (t in 1:T){
 	for (j in 1:J){
          ideal.points[t,,j] <- quantile( xx[,j,t], probs = qs, names = FALSE )
        }
      }
ideal.points[18:T,,1] <- rep(NA,3)
ideal.points[19:T,,c(6,8)] <- rep(NA,3)
ideal.points[1:17,,10:12] <- rep(NA,3)
ideal.points[21:T,,c(2,5,7)] <- rep(NA,3)
ideal.points[1:19,,13:15] <- rep(NA,3)
ideal.points[29:T,,c(3,4,9)] <- rep(NA,3)
#
# EXPECTED MEDIAN
xx[,1,18:T] <- NA; xx[,c(6,8),19:T] <- NA; xx[,10:12,1:17] <- NA; xx[,c(2,5,7),21:T] <- NA; xx[,13:15,1:19] <- NA; xx[,c(3,4,9),29:T] <- NA
exp.med <- rep(NA, T)
for (t in 1:T){
  exp.med[t] <- median( xx[,,t][!is.na(xx[,,t])] )
}
rm(xx)
#

#TODOS LOS CONSEJEROS EN COLOR
#pdf(file=paste(graphdir, "allUAVdynQuarter.pdf", sep="/"), width = 7, height = 7)
par(mar=c(5,4,0,1)+0.1)  ## USA EL ESPACIO DEL TITULO INEXISTENTE
plot(1:T,c(rep(-1,times=(T-1)),1.1),type="n",xlab="quarter",
    ylab="ideal point       ", axes="FALSE")
axis(1, at=c(1:T), labels = FALSE)
axis(1, tick= FALSE, cex.axis=.55, at=c(1:T), labels = c(4, rep(1:4,8)), line=-0.8)
axis(1, tick= FALSE, cex.axis=.9, at=seq(from=3.5,to=31.5,by=4), labels = c("2004","'05","'06","'07","'08","'09","'10","'11") )
axis(2, at=c(-1,-.5,0,.5,1), labels = FALSE)
axis(2, tick= FALSE, cex.axis=.9, at=c(-1,0,1), labels = TRUE )
lines(c(18,18),c(1.1,-2),lwd=1,lty=3, col="grey50")
text(c(18,18),c(1,.95),c("New ","members "), adj=1, cex=.65, col="grey50")
lines(c(20,20),c(1.1,-2),lwd=1,lty=3, col="grey50")
text(c(20,20)-.25,c(1,.95),c("New ","members "), pos=4, cex=.65, col="grey50")
lines(c(28,28),c(1.1,-2),lwd=1,lty=3, col="grey50")
text(c(28,28),c(1,.95),c("New ","members "), adj=1, cex=.65, col="grey50")
#lines(c(33,33),c(1.1,-2),lwd=1,lty=3, col="grey50")
#text(c(33,33),c(1,.95),c("New ","members "), adj=1, cex=.65, col="grey50")
#
inNout <- matrix(1, nrow=15, ncol=2); inNout[,2] <- 100; inNout[1,2] <- 17; inNout[c(6,8),2] <- 18; inNout[10:12,1] <- 18; inNout[c(2,5,7),2] <- 20; inNout[13:15,1] <- 20; inNout[c(3,4,9),2] <- 28; #inNout[16:18,1] <- 33
for (j in 1:J){
  lines(spline(1:T, ideal.points[,2,j]), lwd=3, col = color.456789[j])
  points(spline((1:T)[inNout[j,]], ideal.points[,2,j][inNout[j,]], n=2), pch=19, cex=1, col = color.456789[j])
}
text(.25, ideal.points[1,2,4]+.05, c("Gmz.Alcántar"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,8]+.09, c("Morales"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,3]-.08, c("Andrade"), pos=4, adj=0, cex=.8, col="black")
text(9.5, ideal.points[10,2,1]-.05, c("Ugalde"), pos=4, adj=0, cex=.8, col="black")
text(9.5, ideal.points[11,2,6]+.03, c("Latapí"), pos=4, adj=0, cex=.8, col="black")
text(8.25, ideal.points[9,2,2]+.05, c("Albo"), pos=4, adj=0, cex=.8, col="black")
text(2, ideal.points[5,2,7]+.04, c("Lpz.Flores"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,9]-.11, c("Sánchez"), pos=4, adj=0, cex=.8, col="black")
text(.25, ideal.points[1,2,5]-.06, c("Glez.Luna"), pos=4, adj=0, cex=.8, col="black")
text(15, ideal.points[18,2,10], c("Valdés"), pos=4, adj=0, cex=.8, col="black")
text(18.5, ideal.points[20,2,11]+.02, c("Baños"), pos=4, adj=0, cex=.8, col="black")
text(15.75, ideal.points[18,2,12]+.04, c("Nacif"), pos=4, adj=0, cex=.8, col="black")
text(19.5, ideal.points[20,2,13]+.06, c("Elizondo"), pos=4, adj=0, cex=.8, col="black")
text(19.25, ideal.points[20,2,14]-.07, c("Figueroa"), pos=4, adj=0, cex=.8, col="black")
text(19.75, ideal.points[20,2,15], c("Guerrero"), pos=4, adj=0, cex=.8, col="black")
for (t in 1:T){
  points(t, exp.med[t], pch=17, cex=.5)
}
text(11,-1.05,c("*")); text(23,-1.05,c("*")); text(35,-1.05,c("*"))
text(9,1.15,c("III"))
text(19,1.15,c("IV"))
text(24,1.15,c("V"))
text(31,1.15,c("VI"))
#text(35,1.15,c("VII"))
#dev.off()



############################################################
############################################################
##    Woldenberg and UGALDE a la Bonica Spaghetti plot    ##
############################################################
############################################################

#rm(list=ls())
library(lubridate)

workdir <-  "~/Dropbox/ifeSharedGE/data"
graphdir <- "~/Dropbox/ifeSharedGE/graphs"
#
setwd(workdir)
#load ("DynWoldenbergBonicaMarch19.RData")
#wo.results <- semester.results; rm(semester.results)
load ("DynWoldenbergBonicaApril1.RData")
wo.results <- window.results; rm(window.results)
load ("DynUgaldeBonicaMarch21.RData")
ug.results <- semester.results; rm(semester.results)
all23 <- read.csv("tmp23.csv")
all456789 <- read.csv("tmp456789.csv")
#names.23 <- c("Woldenberg", "Barragán", "Cantú", "Cárdenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebadúa", "Rivera", "Luken")
names.23 <- c("Woldenberg", "Barragan", "Cantu", "Cardenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebadua", "Rivera", "Luken")
color.23 <- c("red",        "gold",     "orange","gold",     "blue",     "red",    "blue",    "red",      "gold",    "red",    "blue")
party.23 <- c("PRI",        "PRD",      "PRD-PT","PRD",      "PAN",      "PRI",    "PAN",     "PRI",      "PRD",     "PRI",    "PAN")
#color.23 <- c("red",        "gold",     "gold",  "gold",     "blue",     "red",    "blue",    "red",      "gold",    "red",    "blue")
#party.23 <- c("PRI",        "PRD",      "PRD",   "PRD",      "PAN",      "PRI",    "PAN",     "PRI",      "PRD",     "PRI",    "PAN")
#names.456789 <- c("Ugalde", "Albo", "Andrade", "Alcántar", "Glez. Luna", "Latapí", "López Flores", "Morales", "Sánchez", "Valdés", "Baños", "Nacif", "Elizondo", "Figueroa", "Guerrero", "Marván", "Córdova", "García Ramírez")
names.456789 <- c("Ugalde", "Albo", "Andrade", "Alcantar", "Glez. Luna", "Latapi", "Lopez Flores", "Morales", "Sanchez", "Valdes", "Banos", "Nacif", "Elizondo", "Figueroa", "Guerrero", "Marvan", "Cordova", "Garcia Ramirez")
color.456789 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red", "blue", "gold", "red")

## multiGelman.hat <- numeric ()
## for (i in 1:S){
## 	chainsConv <- mcmc.list(list (as.mcmc (wo.results[[i]][[2]]$BUGSoutput$sims.list$x), as.mcmc (wo.results[[i]][[1]]$BUGSoutput$sims.list$x)))
## 	tmp <- gelman.diag (chainsConv)[[2]]
## 	multiGelman.hat <- c(multiGelman.hat, tmp)
## }
## summary (multiGelman.hat)
## rm (tmp, chainsConv)

###############
### Woldenberg Bonica spaghetti plot ###
###############

S <- length(wo.results)
I <- nrow(all23)
## # Establish moving windows of 30 votes each
## inicio <- c ( 1:553 )
## final  <- c ( 30:582 )
## # set window's date to middle
## tmp <- ymd(paste(all23$yr,all23$mo,all23$dy,sep="-"))
## item.date <- tmp[inicio+14]; rm(tmp)
### Alternative: center on vote (for date), extend windows to both sides
item <- 1:I
inicio <- item-15; inicio[inicio<0] <- 0
final  <- item+15; final[final>I] <- I
item.date <- ymd(paste(all23$yr,all23$mo,all23$dy,sep="-"))
S <- length(inicio)
#
CouncilorIn <- matrix (1, nrow=11, ncol=S)
#CouncilorIn[c(7,9),  all23$date[-c(1:29)] > 20001114] <- NA # Last Molinar, Zebadua vote
#CouncilorIn[c(10,11),all23$date[-c(1:29)] < 20010130] <- NA # First Rivera, Luken vote
CouncilorIn[c(7,9),  item.date > ymd(20001114)] <- NA # Last Molinar, Zebadua vote
CouncilorIn[c(10,11), item.date < ymd(20010130)] <- NA # First Rivera, Luken vote
#
## # If using DynWodenbergBonicaMarch19.RData, use the following code to extract ideal points
## ideal.points <- matrix (NA, nrow=S, ncol=11)
## ideal.points.var <- matrix (NA, nrow=S, ncol=11)
## for (i in 1:S){
##  	for (j in 1:11){
##  		councilor <- names.23[j]
##  		num <- which (wo.results[[i]][[3]]==councilor)
##  		if ( length (num)==0 ) {
##  			ideal.points[i,j] <- 1
## 			ideal.points.var[i,j]  <- 0
##  		} else {
##  			ideal.points[i,j] <- median (c (wo.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], wo.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]))
##  			ideal.points.var[i,j]  <- var (c (wo.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], wo.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]))
##  		}
##  	}
## }
#
# If using DynWodenbergBonicaApril1.RData, use the following code to extract ideal points
ideal.points <- matrix (NA, nrow=S, ncol=11)
ideal.points.var <- matrix (NA, nrow=S, ncol=11)
for (i in 1:S){
 	for (j in 1:11){
 		councilor <- names.23[j]
 		num <- which (wo.results[[i]][[7]]==councilor)
 		if ( length (num)==0 ) {
 			ideal.points[i,j] <- 1
			ideal.points.var[i,j]  <- 0
 		} else {
 			ideal.points[i,j] <- median ( wo.results[[i]]$BUGSoutput$sims.list$x[,num] )
 			ideal.points.var[i,j]  <- var ( wo.results[[i]]$BUGSoutput$sims.list$x[,num] )
 		}
 	}
}
#
# Get SDs of estimates, the width should be useful to plot thickness of data points
ideal.points.var <- sqrt (ideal.points.var)
#
# Expected Council median ## NOPARECE SALIR BIEN; REVISAR SI QUEREMOS INCLUIRLA EN LA GRAFICA
exp.med <- rep(NA,S)
for (i in 1:S){
  exp.med[i] <- mean (apply ( wo.results[[i]]$BUGSoutput$sims.list$x[,na.omit(CouncilorIn[,i])], 1, median ))
}
#
# Non-smoothed ideal point time-paths (to verify ideal point object looks right)
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:11){
	lines(CouncilorIn[j,1:S] * ideal.points[1:S,j], lwd=4, col=color.23[j])
}
#
# Item indices closest to federal elections
fedEls.items <- c(
min(which(abs(item.date-ymd(19970706))==min(abs(item.date-ymd(19970706))))),
min(which(abs(item.date-ymd(20000702))==min(abs(item.date-ymd(20000702))))),
min(which(abs(item.date-ymd(20030706))==min(abs(item.date-ymd(20030706)))))
    )
#
# Item indices closest to New Years
newYear.items <- c(
min(which(abs(item.date-ymd(19970101))==min(abs(item.date-ymd(19970101))))),
min(which(abs(item.date-ymd(19980101))==min(abs(item.date-ymd(19980101))))),
min(which(abs(item.date-ymd(19990101))==min(abs(item.date-ymd(19990101))))),
min(which(abs(item.date-ymd(20000101))==min(abs(item.date-ymd(20000101))))),
min(which(abs(item.date-ymd(20010101))==min(abs(item.date-ymd(20010101))))),
min(which(abs(item.date-ymd(20020101))==min(abs(item.date-ymd(20020101))))),
min(which(abs(item.date-ymd(20030101))==min(abs(item.date-ymd(20030101)))))
    )
#
# Item indices for entry/exit from council
inNout <- matrix (NA, nrow=11, ncol=S)
for (j in 1:11){
  inNout[j,min(which(CouncilorIn[j,]==1))] <- min(which(CouncilorIn[j,]==1)) 
  inNout[j,max(which(CouncilorIn[j,]==1))] <- max(which(CouncilorIn[j,]==1)) 
}
#
# Function captures smooth.spline ideal point coordinates: Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s] give vote s's 
Smooth <- list ()
for (j in 1:11){
	Smooth[[j]] <- smooth.spline(c(1:S), ideal.points[,j], df=10)
#        Smooth[[j]] <- smooth.spline(c(1:S)[!is.na(CouncilorIn[j,1:S])], ideal.points[!is.na(CouncilorIn[j,1:S]),j], df=10)
}
for (j in 1:11){
	Smooth[[j]]$x[is.na(CouncilorIn[j,1:S])] <- NA
	Smooth[[j]]$y[is.na(CouncilorIn[j,1:S])] <- NA
}
Smooth.exp.med <- smooth.spline(c(1:S), exp.med, df=10)
# Smoothed ideal point time-paths
err <- ideal.points.var*2/max(ideal.points.var) ## estimate error

spaghetti.graph <- function(progress=S){
#pdf(paste(graphdir,"woldBonicaSmoothLines.pdf",sep="/"), width=7, height=7)
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="Divided vote", ylab="Ideal points", axes=FALSE)
axis(1, at=c(1,(1:11)*50,S), labels = FALSE)
axis(1, at=c(1,(1:5)*100,S))
axis(2, at=c(-3:3))
axis(3, at=newYear.items, labels = 1997:2003, cex.axis=.75)
abline(v=fedEls.items, lty=3, col="grey50")
text(rep(fedEls.items, times=2), c(rep(3.1,times=3),rep(2.925,times=3)), c("midterm","presidential","midterm",rep("election",3)), adj=0, cex=.65, pos=1, col="grey30")
for (j in 1:11){
#       points(1:progress, Smooth[[j]]$y[1:progress], cex=.3, col=color.23[j])
#       points(1:progress, Smooth[[j]]$y[1:progress], cex=err[,j], col=color.23[j])
	lines(1:progress, Smooth[[j]]$y[1:progress], lwd=3, col=color.23[j])
        points(inNout[j,], Smooth[[j]]$y, pch=19, col=color.23[j])
}
#points(1:progress, Smooth.exp.med$y[1:progress], pch=17, cex=.25) ## Add expected median
text(-15,Smooth[[2]]$y[1]+.15,c("Barragán"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[1]]$y[1]+.4,c("Woldenberg"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[1]]$y[1]+.25,c("Peschard"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[1]]$y[1]+.1,c("Merino"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[7]]$y[45],c("Molinar"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[9]]$y[20],c("Zebadúa"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[5]]$y[1]-.25,c("Lujambio"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[5]]$y[1]-.4,c("Cantú"), pos=4, adj=0, cex=.75, col="black")
text(-15,Smooth[[4]]$y[70],c("Cárdenas"), pos=4, adj=0, cex=.75, col="black")
text(247-15,Smooth[[10]]$y[247]-.125,c("Rivera"), pos=4, adj=0, cex=.75, col="black")
text(247-15,Smooth[[11]]$y[247]+.075,c("Luken"), pos=4, adj=0, cex=.75, col="black")
#dev.off()
}
#
spaghetti.graph()

# Smoothed ideal point time-paths, daily change movie
snapshot <- seq (3, S, by=5)
for (s in snapshot){
	which.s <- which (snapshot==s)
	if (which.s < 10) {name = paste('Wolden00', which.s,'plot.png',sep='')}
	if (which.s >= 10 && which.s < 100) {name = paste('Wolden0', which.s,'plot.png', sep='')}
	if (which.s >= 100) {name = paste('Wolden', which.s,'plot.png', sep='')}
#
	jpeg (paste(graphdir, "animBits", name, sep="/"), quality=100, height=500, width=500)
#
        spaghetti.graph(s)
#
	## plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
	## for (j in 1:11){
	## 	lines ( Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s], lwd=6, col=color.23[j])
	## }
	legend ("bottomright", bty="n", xjust=0, legend=paste ("vote date:", item.date[s], sep=" ")) #Change the legend for the vote's date
	dev.off()
}

# Make a short film
# The number after loop controls the number of automatic replays (0 stands for infinite loop)
# The number after delay determines length of transition between slides)
setwd(paste(graphdir,"animBits", sep="/"))
system ("convert -loop 1 -delay 20 *.png IFEwoldenTheMovie.gif")
setwd(workdir)


##################
### Ugalde et al Bonica spaghetti plot ###
##################


# Establish moving windows of 30 votes each (OLD WAY)
# inicio <- c ( 1:1081 )
# final  <- c ( 30:1110 )
# Alternative: center on vote (for date), extend windows to both sides
I <- nrow(all456789)
item <- 1:I  # Need to define I before
inicio <- item-15; inicio[inicio<0] <- 0
final  <- item+15; final[final>I] <- I
S <- length(inicio)
J <- 18
item.date <- ymd(paste(all456789$yr, all456789$mo, all456789$dy, sep="-"))
#
CouncilorIn <- matrix (1, nrow=J, ncol=S)
CouncilorIn[1,  item.date >= ymd(20071214)] <- NA # Ugalde out
CouncilorIn[c(6,8), item.date >= ymd(20080207)] <- NA # Latapí, Morales out
CouncilorIn[10:12, item.date < ymd(20080207)] <- NA # Valdés, Baños, Nacif in
CouncilorIn[c(2,5,7), item.date >= ymd(20080814)] <- NA # Albo, GlezLuna, LpzFlores out
CouncilorIn[13:15, item.date < ymd(20080814)] <- NA # Elizondo, Figueroa, Guerrero in
CouncilorIn[c(3,4,9), item.date >= ymd(20101031)] <- NA # Andrade, Alcántar, Sánchez out
CouncilorIn[16:18, item.date < ymd(20111115)] <- NA # Córdova, Marván, GarcíaRmz in
#
# If using DynUgaldeBonicaMarch19.RData or DynUgaldeBonicaMarch21.RData, use the following code to extract ideal points
ideal.points <- matrix (NA, nrow=S, ncol=J)
ideal.points.var <- matrix (NA, nrow=S, ncol=J)
for (i in 1:S){
	for (j in 1:J){
		councilor <- names.456789[j]
		num <- which (ug.results[[i]][[3]]==councilor)
		if ( length (num)==0 ) {
			ideal.points[i,j] <- 1
			ideal.points.var[i,j]  <- 0
		} else {
				ideal.points[i,j] <- median (c (ug.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], ug.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]))
				ideal.points.var[i,j]  <- var (c (ug.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], ug.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]))
		}
	}
}
#
# Get SDs of estimates, the width should be useful to plot thickness of data points
ideal.points.var <- sqrt (ideal.points.var)
#
# Non-smoothed ideal point time-paths
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:J){
	lines(CouncilorIn[j,1:S] * ideal.points[1:S,j], lwd=4, col=color.456789[j])
}
#
# Item indices closest to federal elections
fedEls.items <- c(
min(which(abs(item.date-ymd(20060702))==min(abs(item.date-ymd(20060702))))),
min(which(abs(item.date-ymd(20090705))==min(abs(item.date-ymd(20090705))))),
min(which(abs(item.date-ymd(20120701))==min(abs(item.date-ymd(20120701)))))
    )
#
# Item indices closest to New Years
newYear.items <- c(
min(which(abs(item.date-ymd(20040101))==min(abs(item.date-ymd(20040101))))),
min(which(abs(item.date-ymd(20050101))==min(abs(item.date-ymd(20050101))))),
min(which(abs(item.date-ymd(20060101))==min(abs(item.date-ymd(20060101))))),
min(which(abs(item.date-ymd(20070101))==min(abs(item.date-ymd(20070101))))),
min(which(abs(item.date-ymd(20080101))==min(abs(item.date-ymd(20080101))))),
min(which(abs(item.date-ymd(20090101))==min(abs(item.date-ymd(20090101))))),
min(which(abs(item.date-ymd(20100101))==min(abs(item.date-ymd(20100101))))),
min(which(abs(item.date-ymd(20110101))==min(abs(item.date-ymd(20110101))))),
min(which(abs(item.date-ymd(20120101))==min(abs(item.date-ymd(20120101)))))
    )
#
# Item indices for entry/exit from council
inNout <- matrix (NA, nrow=J, ncol=S)
for (j in 1:J){
  inNout[j,min(which(CouncilorIn[j,]==1))] <- min(which(CouncilorIn[j,]==1)) 
  inNout[j,max(which(CouncilorIn[j,]==1))] <- max(which(CouncilorIn[j,]==1)) 
}
#
# Function captures smooth.spline ideal point coordinates: Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s] give vote s's 
Smooth <- list ()
for (j in 1:J){
#	Smooth[[j]] <- smooth.spline(c(1:S), ideal.points[,j], df=10)
        Smooth[[j]] <- smooth.spline(c(1:S)[!is.na(CouncilorIn[j,1:S])], ideal.points[!is.na(CouncilorIn[j,1:S]),j], df=10)
}
for (j in 1:J){
	Smooth[[j]]$y[is.na(CouncilorIn[j,1:S])] <- NA
}
#
# Smoothed ideal point time-paths
err <- ideal.points.var*2/max(ideal.points.var) ## estimate error

spaghetti.graph <- function(progress=S){
#pdf(paste(graphdir,"woldBonicaSmoothLines.pdf",sep="/"), width=7, height=7)
plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-2,3), type="n", xlab="Divided vote", ylab="Ideal points", axes=FALSE)
axis(1, at=c(1,(1:as.integer(S/100))*100,S), cex.axis=.9)
#axis(1, at=c(1,(1:7)*100,S))
axis(2, at=c(-2:3))
axis(3, at=newYear.items, labels = 2004:2012, cex.axis=.75)
abline(v=fedEls.items, lty=3, col="grey50")
#text(rep(fedEls.items, times=2), c(rep(3.1,times=3),rep(2.925,times=3)), c("midterm","presidential","midterm",rep("election",3)), adj=0, cex=.65, pos=1, col="grey30")
text(rep(fedEls.items, times=2), c(rep(3.1,times=3),rep(2.925,times=3)), c("presidential","midterm","presidential",rep("election",3)), adj=0, cex=.65, pos=1, col="grey30")
for (j in 1:J){
#       points(1:progress, Smooth[[j]]$y[1:progress], cex=.3, col=color.23[j])
#       points(1:progress, Smooth[[j]]$y[1:progress], cex=err[,j], col=color.23[j])
	lines(1:progress, Smooth[[j]]$y[1:progress], lwd=3, col=color.456789[j])
        points(inNout[j,], Smooth[[j]]$y, pch=19, col=color.456789[j])
}
## text(-15,Smooth[[2]]$y[1]+.15,c("Barragán"), pos=4, adj=0, cex=.75, col="black")
## text(-15,Smooth[[1]]$y[1]+.4,c("Woldenberg"), pos=4, adj=0, cex=.75, col="black")
## text(-15,Smooth[[1]]$y[1]+.25,c("Peschard"), pos=4, adj=0, cex=.75, col="black")
## text(-15,Smooth[[1]]$y[1]+.1,c("Merino"), pos=4, adj=0, cex=.75, col="black")
## text(-15,Smooth[[7]]$y[50],c("Molinar"), pos=4, adj=0, cex=.75, col="black")
## text(-15,Smooth[[9]]$y[35],c("Zebadúa"), pos=4, adj=0, cex=.75, col="black")
## text(-15,Smooth[[5]]$y[1]-.25,c("Lujambio"), pos=4, adj=0, cex=.75, col="black")
## text(-15,Smooth[[5]]$y[1]-.4,c("Cantú"), pos=4, adj=0, cex=.75, col="black")
## text(-15,Smooth[[4]]$y[70],c("Cárdenas"), pos=4, adj=0, cex=.75, col="black")
## text(233-15,Smooth[[10]]$y[233]-.125,c("Rivera"), pos=4, adj=0, cex=.75, col="black")
## text(233-15,Smooth[[11]]$y[233]+.075,c("Luken"), pos=4, adj=0, cex=.75, col="black")
#dev.off()
}
## Stop at term<9
## dim(all456789[all456789$term<9,])
## S <- 864
## Stop at term<8
## S <- 713
#
spaghetti.graph()

# Smoothed ideal point time-paths, daily change movie
snapshot <- seq (3, 553, by=5)
for (s in snapshot){
	which.s <- which (snapshot==s)
	if (which.s < 10) {name = paste('Wolden00', which.s,'plot.png',sep='')}
	if (which.s >= 10 && which.s < 100) {name = paste('Wolden0', which.s,'plot.png', sep='')}
	if (which.s >= 100) {name = paste('Wolden', which.s,'plot.png', sep='')}
#
	jpeg (paste(graphdir, "animBits", name, sep="/"), quality=100, height=500, width=500)
#
        spaghetti.graph(s)
#
	## plot(c(1:S), ideal.points[1:S,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
	## for (j in 1:11){
	## 	lines ( Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s], lwd=6, col=color.23[j])
	## }
	legend ("bottomright", bty="n", xjust=0, legend=paste ("vote date:", item.date[s], sep=" ")) #Change the legend for the vote's date
	dev.off()
}

# Make a short film
# The number after loop controls the number of automatic replays (0 stands for infinite loop)
# The number after delay determines length of transition between slides)
setwd(paste(graphdir,"animBits", sep="/"))
system ("convert -loop 1 -delay 20 *.png IFEwoldenTheMovie.gif")
setwd(workdir)






##############################################################################
##############################################################################
##                                                                          ##
##  POLIGONOS SEMI TRANSPARENTES WOLDENBERG I Y II (VIENE DE IFEBUGSDYN.R)  ##
##                                                                          ##
##############################################################################
##############################################################################

###############################################
# VERSION 1: CON ESTIMACION BONICA DynWodenbergBonicaMarch19.RData
#
library(lubridate)
#
rm(list=ls())
workdir <-  "~/Dropbox/ifeSharedGE/data"
graphdir <- "~/Dropbox/ifeSharedGE/graphs"
#
setwd(workdir)
## load ("DynWoldenbergBonicaMarch19.RData")
## wo.results <- semester.results; rm(semester.results)
load ("DynWoldenbergBonicaApril1.RData")
wo.results <- window.results; rm(window.results)
load ("DynUgaldeBonicaMarch21.RData")
ug.results <- semester.results; rm(semester.results)
all23 <- read.csv("tmp23.csv")
all456789 <- read.csv("tmp456789.csv")
#
S <- length(wo.results)
I <- nrow(all23)
## # Establish moving windows of 30 votes each
## inicio <- c ( 1:553 )
## final  <- c ( 30:582 )
## # set window's date to middle
## tmp <- ymd(paste(all23$yr,all23$mo,all23$dy,sep="-"))
## item.date <- tmp[inicio+14]; rm(tmp)
### Alternative: center on vote (for date), extend windows to both sides
item <- 1:I
inicio <- item-15; inicio[inicio<0] <- 0
final  <- item+15; final[final>I] <- I
item.date <- ymd(paste(all23$yr,all23$mo,all23$dy,sep="-"))
S <- length(inicio)
#
#names.23 <- c("Woldenberg", "Barragán", "Cantú", "Cárdenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebadúa", "Rivera", "Luken")
names.23 <- c("Woldenberg", "Barragan", "Cantu", "Cardenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebadua", "Rivera", "Luken")
color.23 <- c("red",        "gold",     "orange","gold",     "blue",     "red",    "blue",    "red",      "gold",    "red",    "blue")
party.23 <- c("PRI",        "PRD",      "PRD-PT","PRD",      "PAN",      "PRI",    "PAN",     "PRI",      "PRD",     "PRI",    "PAN")
#color.23 <- c("red",        "gold",     "gold",  "gold",     "blue",     "red",    "blue",    "red",      "gold",    "red",    "blue")
#party.23 <- c("PRI",        "PRD",      "PRD",   "PRD",      "PAN",      "PRI",    "PAN",     "PRI",      "PRD",     "PRI",    "PAN")
#names.456789 <- c("Ugalde", "Albo", "Andrade", "Alcántar", "Glez. Luna", "Latapí", "López Flores", "Morales", "Sánchez", "Valdés", "Baños", "Nacif", "Elizondo", "Figueroa", "Guerrero", "Marván", "Córdova", "García Ramírez")
names.456789 <- c("Ugalde", "Albo", "Andrade", "Alcantar", "Glez. Luna", "Latapi", "Lopez Flores", "Morales", "Sanchez", "Valdes", "Banos", "Nacif", "Elizondo", "Figueroa", "Guerrero", "Marvan", "Cordova", "Garcia Ramirez")
color.456789 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red", "blue", "gold", "red")
#
# Item indices closest to federal elections
fedEls.items <- c(
min(which(abs(item.date-ymd(19970706))==min(abs(item.date-ymd(19970706))))),
min(which(abs(item.date-ymd(20000702))==min(abs(item.date-ymd(20000702))))),
min(which(abs(item.date-ymd(20030706))==min(abs(item.date-ymd(20030706)))))
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
    )
#
S <- length(wo.results); J <- 11
yrs <- 1:S
#
CouncilorIn <- matrix (1, nrow=11, ncol=S)
#CouncilorIn[c(7,9),  all23$date[-c(1:29)] > 20001114] <- NA # Last Molinar, Zebadua vote
#CouncilorIn[c(10,11),all23$date[-c(1:29)] < 20010130] <- NA # First Rivera, Luken vote
CouncilorIn[c(7,9),  item.date > ymd(20001114)] <- NA # Last Molinar, Zebadua vote
CouncilorIn[c(10,11), item.date < ymd(20010130)] <- NA # First Rivera, Luken vote
#
## # If using DynWodenbergBonicaMarch19.RData, use the following code to extract ideal points and CIs
## qs <- c(.025,.5,.975) ## quantiles you wish to report
## ideal.points <- array(NA, dim = c(S,3,J)); dimnames(ideal.points) <- list(NULL, qs, NULL);
## for (i in 1:S){
##  	for (j in 1:J){
##  		councilor <- names.23[j]
##  		num <- which (wo.results[[i]][[3]]==councilor)
##  		if ( length (num)==0 ) {
##  			ideal.points[i,,j] <- c(1,1,1) ## USE ONEs SO THAT SPLINE FUNCTION IS DETERMINED (MAKE THEM NAs LATER)
##  		} else {
##  			ideal.points[i,,j] <- quantile (c (wo.results[[i]][[1]]$BUGSoutput$sims.list$x[,num], wo.results[[i]][[2]]$BUGSoutput$sims.list$x[,num]), probs = qs, names = FALSE)
##  		}
##  	}
## }
#
# If using DynWodenbergBonicaApril1.RData, use the following code to extract ideal points
qs <- c(.025,.5,.975) ## quantiles you wish to report
ideal.points <- array(NA, dim = c(S,3,J)); dimnames(ideal.points) <- list(NULL, qs, NULL);
for (i in 1:S){
 	for (j in 1:11){
 		councilor <- names.23[j]
 		num <- which (wo.results[[i]][[7]]==councilor)
 		if ( length (num)==0 ) {
 			ideal.points[i,,j] <- c(1,1,1)
 		} else {
 			ideal.points[i,,j] <- quantile ( wo.results[[i]]$BUGSoutput$sims.list$x[,num], probs = qs, names = FALSE )
 		}
 	}
}
# Non-smoothed ideal point time-paths to verify ideal point object complete
plot(c(1:S), ideal.points[1:S,2,1], main="", ylim=c(-3,3), type="n", xlab="", ylab="Ideal points")
for (j in 1:11){
	lines(CouncilorIn[j,1:S] * ideal.points[1:S,2,j], lwd=4, col=color.23[j])
#	lines( ideal.points[1:S,2,j], lwd=4, col=color.23[j])
}
#
# Function captures smooth.spline ideal point coordinates: Smooth[[j]]$x[1:s], Smooth[[j]]$y[1:s] give vote s's 
SmoothMed <- list ()
for (j in 1:11){
	SmoothMed[[j]] <- smooth.spline(c(1:S), ideal.points[,2,j], df=10)
#        SmoothMed[[j]] <- smooth.spline(c(1:S)[!is.na(CouncilorIn[j,2,1:S])], ideal.points[!is.na(CouncilorIn[j,2,1:S]),j], df=10)
}
for (j in 1:11){
	SmoothMed[[j]]$x[is.na(CouncilorIn[j,1:S])] <- NA
	SmoothMed[[j]]$y[is.na(CouncilorIn[j,1:S])] <- NA
}
SmoothLo <- list ()
for (j in 1:11){
	SmoothLo[[j]] <- smooth.spline(c(1:S), ideal.points[,1,j], df=10)
#        SmoothLo[[j]] <- smooth.spline(c(1:S)[!is.na(CouncilorIn[j,1,1:S])], ideal.points[!is.na(CouncilorIn[j,1,1:S]),j], df=10)
}
for (j in 1:11){
	SmoothLo[[j]]$x[is.na(CouncilorIn[j,1:S])] <- NA
	SmoothLo[[j]]$y[is.na(CouncilorIn[j,1:S])] <- NA
}
SmoothHi <- list ()
for (j in 1:11){
	SmoothHi[[j]] <- smooth.spline(c(1:S), ideal.points[,3,j], df=10)
#        SmoothHi[[j]] <- smooth.spline(c(1:S)[!is.na(CouncilorIn[j,3,1:S])], ideal.points[!is.na(CouncilorIn[j,3,1:S]),j], df=10)
}
for (j in 1:11){
	SmoothHi[[j]]$x[is.na(CouncilorIn[j,1:S])] <- NA
	SmoothHi[[j]]$y[is.na(CouncilorIn[j,1:S])] <- NA
}
#
p1 <- rbind(cbind(SmoothLo[[1]]$x, SmoothLo[[1]]$y), cbind(rev(SmoothHi[[1]]$x), rev(SmoothHi[[1]]$y)))
p2 <- rbind(cbind(SmoothLo[[2]]$x, SmoothLo[[2]]$y), cbind(rev(SmoothHi[[2]]$x), rev(SmoothHi[[2]]$y)))
p3 <- rbind(cbind(SmoothLo[[3]]$x, SmoothLo[[3]]$y), cbind(rev(SmoothHi[[3]]$x), rev(SmoothHi[[3]]$y)))
p4 <- rbind(cbind(SmoothLo[[4]]$x, SmoothLo[[4]]$y), cbind(rev(SmoothHi[[4]]$x), rev(SmoothHi[[4]]$y)))
p5 <- rbind(cbind(SmoothLo[[5]]$x, SmoothLo[[5]]$y), cbind(rev(SmoothHi[[5]]$x), rev(SmoothHi[[5]]$y)))
p6 <- rbind(cbind(SmoothLo[[6]]$x, SmoothLo[[6]]$y), cbind(rev(SmoothHi[[6]]$x), rev(SmoothHi[[6]]$y)))
p7 <- rbind(cbind(SmoothLo[[7]]$x[!is.na(CouncilorIn[7,1:S])], SmoothLo[[7]]$y[!is.na(CouncilorIn[7,1:S])]), cbind(rev(SmoothHi[[7]]$x[!is.na(CouncilorIn[7,1:S])]), rev(SmoothHi[[7]]$y[!is.na(CouncilorIn[7,1:S])])))
p8 <- rbind(cbind(SmoothLo[[8]]$x, SmoothLo[[8]]$y), cbind(rev(SmoothHi[[8]]$x), rev(SmoothHi[[8]]$y)))
p9 <- rbind(cbind(SmoothLo[[9]]$x[!is.na(CouncilorIn[9,1:S])], SmoothLo[[9]]$y[!is.na(CouncilorIn[9,1:S])]), cbind(rev(SmoothHi[[9]]$x[!is.na(CouncilorIn[9,1:S])]), rev(SmoothHi[[9]]$y[!is.na(CouncilorIn[9,1:S])])))
p10 <- rbind(cbind(SmoothLo[[10]]$x[!is.na(CouncilorIn[10,1:S])], SmoothLo[[10]]$y[!is.na(CouncilorIn[10,1:S])]), cbind(rev(SmoothHi[[10]]$x[!is.na(CouncilorIn[10,1:S])]), rev(SmoothHi[[10]]$y[!is.na(CouncilorIn[10,1:S])])))
p11 <- rbind(cbind(SmoothLo[[11]]$x[!is.na(CouncilorIn[11,1:S])], SmoothLo[[11]]$y[!is.na(CouncilorIn[11,1:S])]), cbind(rev(SmoothHi[[11]]$x[!is.na(CouncilorIn[11,1:S])]), rev(SmoothHi[[11]]$y[!is.na(CouncilorIn[11,1:S])])))
#
#
titleElem <- c("wold", "Pri") #ugvalStackPRImadPVEM
#pdf(file=paste(graphdir, "/", titleElem[1], "StackBonica", titleElem[2], ".pdf", sep=""), width = 7, height = 7)
plot(1:S,c(rep(-3,times=(S-1)),3),type="n",xlab="Divided vote",
    ylab="95% CIs for ideal points", axes="FALSE", main="")
axis(1, at=c(1, c(1:10)*50, S), cex.axis=.8, labels = TRUE)
## axis(1, tick= FALSE, cex.axis=.55, at=c(1:14), labels = rep(1:2,7), line=-0.8)
## axis(1, tick= FALSE, cex.axis=.8, at=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5), labels = 1997:2003 )
axis(2, at=c(-3,-1.5,0,1.5,3), labels = FALSE)
axis(2, tick= FALSE, cex.axis=.8, at=c(-3,0,3), labels = TRUE )
axis(3, at=newYear.items, labels = 1997:2003, cex.axis=.65)
abline(v=fedEls.items, lty=3, col="grey50")
#text(rep(fedEls.items, times=2), c(rep(3.1,times=3),rep(2.925,times=3)), c("midterm","presidential","midterm",rep("election",3)), adj=0, cex=.65, pos=1, col="grey30")
## lines(c(8.5,8.5),c(.9,-.9),lwd=1,lty=3, col="grey50")
##text(c(8.5,8.5),c(.9,.85),c("New ","members "), adj=1, cex=.5, col="grey50")
polygon(p1, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Woldenberg 
#polygon(p2, col=rgb(1,215/255,0,alpha=.25), lty=0)                  ## Barragán   
#polygon(p3, col=rgb(1,140/255,0,alpha=.25), lty=0)                  ## Cantú      
#polygon(p4, col=rgb(1,215/255,0,alpha=.25), lty=0)                  ## Cárdenas   
#polygon(p5, col=rgb(0,0,1,alpha=.25), lty=0)                        ## Lujambio   
polygon(p6, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Merino     
#polygon(p7, col=rgb(0,0,1,alpha=.25), lty=0)                        ## Molinar    
polygon(p8, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Peschard   
#polygon(p9, col=rgb(1,215/255,0,alpha=.25), lty=0)                  ## Zebadúa    
polygon(p10, col=rgb(1,0,0,alpha=.25), lty=0)                       ## Rivera     
#polygon(p11, col=rgb(0,0,1,alpha=.25), lty=0)                       ## Luken      
#lines(SmoothMed[[1]]$x, SmoothMed[[1]]$y)                                                               ## Woldenberg 
#lines(SmoothMed[[2]]$x, SmoothMed[[2]]$y)                                                               ## Barragán   
#lines(SmoothMed[[3]]$x, SmoothMed[[3]]$y)                                                               ## Cantú      
#lines(SmoothMed[[4]]$x, SmoothMed[[4]]$y)                                                               ## Cárdenas   
#lines(SmoothMed[[5]]$x, SmoothMed[[5]]$y)                                                               ## Lujambio   
#lines(SmoothMed[[6]]$x, SmoothMed[[6]]$y)                                                               ## Merino     
#lines(SmoothMed[[7]]$x[!is.na(CouncilorIn[7,1:S])], SmoothMed[[7]]$y[!is.na(CouncilorIn[7,1:S])])       ## Molinar    
#lines(SmoothMed[[8]]$x, SmoothMed[[8]]$y)                                                               ## Peschard   
#lines(SmoothMed[[9]]$x[!is.na(CouncilorIn[9,1:S])], SmoothMed[[9]]$y[!is.na(CouncilorIn[9,1:S])])       ## Zebadúa    
#lines(SmoothMed[[10]]$x[!is.na(CouncilorIn[10,1:S])], SmoothMed[[10]]$y[!is.na(CouncilorIn[10,1:S])])   ## Rivera     
#lines(SmoothMed[[11]]$x[!is.na(CouncilorIn[11,1:S])], SmoothMed[[11]]$y[!is.na(CouncilorIn[11,1:S])])   ## Luken      
#dev.off()
#

###############################################
# VERSION 2: CON ESTIMACION MartinQuinn Trimestral

rm(list=ls())
workdir <-  "~/Dropbox/ifeSharedGE/data"
graphdir <- "~/Dropbox/ifeSharedGE/graphs"
#
setwd(workdir)
load ("DynWmartinQuinnTrimestresLucia.RData")

wo.results <- ife.23; rm(ife.23)
S <- wo.results$n.sims # N OF SAMPLED SIMULATIONS
I <- nrow(all23); J <- 11; T <- 28
all23 <- read.csv("tmp23.csv")
#
## All posterior ideal point simulations by quarter
xx <- array (NA, dim=c(S, J, T))
xx[,,1] <-  wo.results$sims.list$x1
xx[,,2] <-  wo.results$sims.list$x2 
xx[,,3] <-  wo.results$sims.list$x3 
xx[,,4] <-  wo.results$sims.list$x4 
xx[,,5] <-  wo.results$sims.list$x5 
xx[,,6] <-  wo.results$sims.list$x6 
xx[,,7] <-  wo.results$sims.list$x7 
xx[,,8] <-  wo.results$sims.list$x8 
xx[,,9] <-  wo.results$sims.list$x9 
xx[,,10] <- wo.results$sims.list$x10
xx[,,11] <- wo.results$sims.list$x11
xx[,,12] <- wo.results$sims.list$x12
xx[,,13] <- wo.results$sims.list$x13
xx[,,14] <- wo.results$sims.list$x14
xx[,,15] <- wo.results$sims.list$x15
xx[,,16] <- wo.results$sims.list$x16
xx[,,17] <- wo.results$sims.list$x17
xx[,,18] <- wo.results$sims.list$x18
xx[,,19] <- wo.results$sims.list$x19
xx[,,20] <- wo.results$sims.list$x20
xx[,,21] <- wo.results$sims.list$x21
xx[,,22] <- wo.results$sims.list$x22
xx[,,23] <- wo.results$sims.list$x23
xx[,,24] <- wo.results$sims.list$x24
xx[,,25] <- wo.results$sims.list$x25
xx[,,26] <- wo.results$sims.list$x26
xx[,,27] <- wo.results$sims.list$x27
xx[,,28] <- wo.results$sims.list$x28
# Thin Lucía's results further
xx <- xx[(1:300)*10,,]
## TRIMESTRES CON CONSEJEROS NUEVOS Y VIEJOS PRESENTES EN t=17
qs <- c(.025,.5,.975) ## quantiles you wish to report
ideal.points <- array(NA, dim = c(T,3,J)); dimnames(ideal.points) <- list(NULL, qs, NULL);
for (t in 1:T){
 	for (j in 1:J){
          ideal.points[t,,j] <- quantile( xx[,j,t], probs = qs, names = FALSE )
        }
      }
ideal.points[18:T,,c(7,9)] <- rep(NA,3)
ideal.points[1:16,,c(10,11)] <- rep(NA,3)
#
p1 <- rbind(cbind(1:T, ideal.points[,1,1]), cbind(rev(1:T), rev(ideal.points[,3,1])))
p2 <- rbind(cbind(1:T, ideal.points[,1,2]), cbind(rev(1:T), rev(ideal.points[,3,2])))
p3 <- rbind(cbind(1:T, ideal.points[,1,3]), cbind(rev(1:T), rev(ideal.points[,3,3])))
p4 <- rbind(cbind(1:T, ideal.points[,1,4]), cbind(rev(1:T), rev(ideal.points[,3,4])))
p5 <- rbind(cbind(1:T, ideal.points[,1,5]), cbind(rev(1:T), rev(ideal.points[,3,5])))
p6 <- rbind(cbind(1:T, ideal.points[,1,6]), cbind(rev(1:T), rev(ideal.points[,3,6])))
p7 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,7])], ideal.points[,1,7][!is.na(ideal.points[,1,7])]), cbind(rev((1:T)[!is.na(ideal.points[,1,7])]), rev(ideal.points[,3,7][!is.na(ideal.points[,1,7])])))
p8 <- rbind(cbind(1:T, ideal.points[,1,8]), cbind(rev(1:T), rev(ideal.points[,3,8])))
p9 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,9])], ideal.points[,1,9][!is.na(ideal.points[,1,9])]), cbind(rev((1:T)[!is.na(ideal.points[,1,9])]), rev(ideal.points[,3,9][!is.na(ideal.points[,1,9])])))
#p9 <- rbind(cbind(1:T, ideal.points[,1,9]), cbind(rev(1:T), rev(ideal.points[,3,9])))
p10 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,10])], ideal.points[,1,10][!is.na(ideal.points[,1,10])]), cbind(rev((1:T)[!is.na(ideal.points[,1,10])]), rev(ideal.points[,3,10][!is.na(ideal.points[,1,10])])))
p11 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,11])], ideal.points[,1,11][!is.na(ideal.points[,1,11])]), cbind(rev((1:T)[!is.na(ideal.points[,1,11])]), rev(ideal.points[,3,11][!is.na(ideal.points[,1,11])])))
#p10 <- rbind(cbind(1:T, ideal.points[,1,10]), cbind(rev(1:T), rev(ideal.points[,3,10])))
#p11 <- rbind(cbind(1:T, ideal.points[,1,11]), cbind(rev(1:T), rev(ideal.points[,3,11])))
#
titleElem <- c("wold", "Left") #ugvalStackPRImadPVEM
pdf(file=paste(graphdir, "/", titleElem[1], "StackMartinQuinnQuarter", titleElem[2], ".pdf", sep=""), width = 7, height = 7)
par(mar=c(5,4,0,1)+0.1)  ## USA EL ESPACIO DEL TITULO INEXISTENTE
plot(1:T,c(rep(-1.5,times=(T-1)),1.5),type="n",xlab="quarter",
    ylab="95% CIs for ideal points", axes = FALSE)
axis(1, at=c(1:T), labels = FALSE)
axis(1, tick= FALSE, cex.axis=.55, at=c(1:28), labels = c(4,rep(1:4,6),1,2,3), line=-0.8)
axis(1, tick= FALSE, cex.axis=.8, at=seq(from = 3.5, to = 27.5, by = 4), labels = 1997:2003 )
axis(2, at=c(-1.5,-1,-.5,0,.5,1,1.5), labels = FALSE)
axis(2, tick= FALSE, cex.axis=.8, at=c(-1,0,1), labels = TRUE )
#lines(c(17,17),c(-3,3),lwd=1,lty=3, col="grey50")
lines(c(3,3),c(-3,3),lwd=1,lty=3, col="grey50"); 
text(c(3,3),c(1.5,1.4),c("Fed. ","election "), adj=1, cex=.65, col="grey50")
lines(c(15,15),c(-3,3),lwd=1,lty=3, col="grey50"); 
text(c(15,15),c(1.5,1.4),c("Fed. ","election "), adj=1, cex=.65, col="grey50")
lines(c(27,27),c(-3,3),lwd=1,lty=3, col="grey50"); 
text(c(27,27),c(1.5,1.4),c("Fed. ","election "), adj=1, cex=.65, col="grey50")
##text(c(8.5,8.5),c(.9,.85),c("New ","members "), adj=1, cex=.5, col="grey50")
#polygon(p1, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Woldenberg
polygon(p2, col=rgb(1,215/255,0,alpha=.25), lty=0)                  ## Barragán
polygon(p3, col=rgb(1,140/255,0,alpha=.25), lty=0)                  ## Cantú
polygon(p4, col=rgb(1,215/255,0,alpha=.25), lty=0)                  ## Cárdenas
#polygon(p5, col=rgb(0,0,1,alpha=.25), lty=0)                        ## Lujambio
#polygon(p6, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Merino
#polygon(p7, col=rgb(0,0,1,alpha=.25), lty=0)                        ## Molinar
#polygon(p8, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Peschard
polygon(p9, col=rgb(1,215/255,0,alpha=.25), lty=0)                  ## Zebadúa
#polygon(p10, col=rgb(1,0,0,alpha=.25), lty=0)                       ## Rivera
#polygon(p11, col=rgb(0,0,1,alpha=.25), lty=0)                       ## Luken
## lines(1:14, j1[,2])
## lines(1:14, j6[,2])
## lines(1:14, j8[,2])
## lines(1:14, j10[,2])
## text(2,-0.82,c("*"))
## text(8,-0.82,c("*"))
## text(14,-0.82,c("*"))
## text(4.5,.95,c("I"))
## text(11.5,.95,c("II"))
dev.off()
#






#########################################################################
#########################################################################
##                                                                     ##
##  POLIGONOS SEMI TRANSPARENTES UGALDE ET AL (VIENE DE IFEBUGSDYN.R)  ##
##                                                                     ##
#########################################################################
#########################################################################

###############################################
# VERSION 2: CON ESTIMACION MartinQuinn Trimestral

rm(list=ls())
workdir <-  "~/Dropbox/ifeSharedGE/data"
graphdir <- "~/Dropbox/ifeSharedGE/graphs"
#
setwd(workdir)
load ("DynUAVmartinQuinnTrimestres2003-2011lucia.Rdata")
#
ug.results <- ife.45678; rm(ife.45678)
all45678 <- read.csv("tmp456789.csv")
all45678 <- all45678[all45678$term<9,] ## Drop term==9 which Lucía did not include
#
S <- ug.results$n.sims # N OF SAMPLED SIMULATIONS
I <- nrow(all45678); J <- 15; T <- 33
#
## All posterior ideal point simulations by quarter
xx <- array (NA, dim=c(S, J, T))
xx[,,1] <-  ug.results$sims.list$x1
xx[,,2] <-  ug.results$sims.list$x2 
xx[,,3] <-  ug.results$sims.list$x3 
xx[,,4] <-  ug.results$sims.list$x4 
xx[,,5] <-  ug.results$sims.list$x5 
xx[,,6] <-  ug.results$sims.list$x6 
xx[,,7] <-  ug.results$sims.list$x7 
xx[,,8] <-  ug.results$sims.list$x8 
xx[,,9] <-  ug.results$sims.list$x9 
xx[,,10] <- ug.results$sims.list$x10
xx[,,11] <- ug.results$sims.list$x11
xx[,,12] <- ug.results$sims.list$x12
xx[,,13] <- ug.results$sims.list$x13
xx[,,14] <- ug.results$sims.list$x14
xx[,,15] <- ug.results$sims.list$x15
xx[,,16] <- ug.results$sims.list$x16
xx[,,17] <- ug.results$sims.list$x17
xx[,,18] <- ug.results$sims.list$x18
xx[,,19] <- ug.results$sims.list$x19
xx[,,20] <- ug.results$sims.list$x20
xx[,,21] <- ug.results$sims.list$x21
xx[,,22] <- ug.results$sims.list$x22
xx[,,23] <- ug.results$sims.list$x23
xx[,,24] <- ug.results$sims.list$x24
xx[,,25] <- ug.results$sims.list$x25
xx[,,26] <- ug.results$sims.list$x26
xx[,,27] <- ug.results$sims.list$x27
xx[,,28] <- ug.results$sims.list$x28
xx[,,29] <- ug.results$sims.list$x29
xx[,,30] <- ug.results$sims.list$x30
xx[,,31] <- ug.results$sims.list$x31
xx[,,32] <- ug.results$sims.list$x32
xx[,,33] <- ug.results$sims.list$x33
# Thin Lucía's results further
xx <- xx[(1:300)*10,,]
## TRIMESTRES CON CONSEJEROS NUEVOS Y VIEJOS PRESENTES EN t=17
qs <- c(.025,.5,.975) ## quantiles you wish to report
ideal.points <- array(NA, dim = c(T,3,J)); dimnames(ideal.points) <- list(NULL, qs, NULL);
for (t in 1:T){
 	for (j in 1:J){
          ideal.points[t,,j] <- quantile( xx[,j,t], probs = qs, names = FALSE )
        }
      }
ideal.points[18:T,,1] <- rep(NA,3)
ideal.points[19:T,,c(6,8)] <- rep(NA,3)
ideal.points[1:18,,10:12] <- rep(NA,3)
ideal.points[21:T,,c(2,5,7)] <- rep(NA,3)
ideal.points[1:20,,13:15] <- rep(NA,3)
ideal.points[29:T,,c(3,4,9)] <- rep(NA,3)
#
p1 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,1])], ideal.points[,1,1][!is.na(ideal.points[,1,1])]), cbind(rev((1:T)[!is.na(ideal.points[,1,1])]), rev(ideal.points[,3,1][!is.na(ideal.points[,1,1])])))
p2 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,2])], ideal.points[,1,2][!is.na(ideal.points[,1,2])]), cbind(rev((1:T)[!is.na(ideal.points[,1,2])]), rev(ideal.points[,3,2][!is.na(ideal.points[,1,2])])))
p3 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,3])], ideal.points[,1,3][!is.na(ideal.points[,1,3])]), cbind(rev((1:T)[!is.na(ideal.points[,1,3])]), rev(ideal.points[,3,3][!is.na(ideal.points[,1,3])])))
p4 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,4])], ideal.points[,1,4][!is.na(ideal.points[,1,4])]), cbind(rev((1:T)[!is.na(ideal.points[,1,4])]), rev(ideal.points[,3,4][!is.na(ideal.points[,1,4])])))
p5 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,5])], ideal.points[,1,5][!is.na(ideal.points[,1,5])]), cbind(rev((1:T)[!is.na(ideal.points[,1,5])]), rev(ideal.points[,3,5][!is.na(ideal.points[,1,5])])))
p6 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,6])], ideal.points[,1,6][!is.na(ideal.points[,1,6])]), cbind(rev((1:T)[!is.na(ideal.points[,1,6])]), rev(ideal.points[,3,6][!is.na(ideal.points[,1,6])])))
p7 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,7])], ideal.points[,1,7][!is.na(ideal.points[,1,7])]), cbind(rev((1:T)[!is.na(ideal.points[,1,7])]), rev(ideal.points[,3,7][!is.na(ideal.points[,1,7])])))
p8 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,8])], ideal.points[,1,8][!is.na(ideal.points[,1,8])]), cbind(rev((1:T)[!is.na(ideal.points[,1,8])]), rev(ideal.points[,3,8][!is.na(ideal.points[,1,8])])))
p9 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,9])], ideal.points[,1,9][!is.na(ideal.points[,1,9])]), cbind(rev((1:T)[!is.na(ideal.points[,1,9])]), rev(ideal.points[,3,9][!is.na(ideal.points[,1,9])])))
p10 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,10])], ideal.points[,1,10][!is.na(ideal.points[,1,10])]), cbind(rev((1:T)[!is.na(ideal.points[,1,10])]), rev(ideal.points[,3,10][!is.na(ideal.points[,1,10])])))
p11 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,11])], ideal.points[,1,11][!is.na(ideal.points[,1,11])]), cbind(rev((1:T)[!is.na(ideal.points[,1,11])]), rev(ideal.points[,3,11][!is.na(ideal.points[,1,11])])))
p12 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,12])], ideal.points[,1,12][!is.na(ideal.points[,1,11])]), cbind(rev((1:T)[!is.na(ideal.points[,1,12])]), rev(ideal.points[,3,12][!is.na(ideal.points[,1,12])])))
p13 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,13])], ideal.points[,1,13][!is.na(ideal.points[,1,13])]), cbind(rev((1:T)[!is.na(ideal.points[,1,13])]), rev(ideal.points[,3,13][!is.na(ideal.points[,1,13])])))
p14 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,14])], ideal.points[,1,14][!is.na(ideal.points[,1,14])]), cbind(rev((1:T)[!is.na(ideal.points[,1,14])]), rev(ideal.points[,3,14][!is.na(ideal.points[,1,14])])))
p15 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,15])], ideal.points[,1,15][!is.na(ideal.points[,1,15])]), cbind(rev((1:T)[!is.na(ideal.points[,1,15])]), rev(ideal.points[,3,15][!is.na(ideal.points[,1,15])])))
#p16 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,16])], ideal.points[,1,16][!is.na(ideal.points[,1,16])]), cbind(rev((1:T)[!is.na(ideal.points[,1,16])]), rev(ideal.points[,3,16][!is.na(ideal.points[,1,16])])))
#p17 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,17])], ideal.points[,1,17][!is.na(ideal.points[,1,17])]), cbind(rev((1:T)[!is.na(ideal.points[,1,17])]), rev(ideal.points[,3,17][!is.na(ideal.points[,1,17])])))
#p18 <- rbind(cbind((1:T)[!is.na(ideal.points[,1,18])], ideal.points[,1,18][!is.na(ideal.points[,1,18])]), cbind(rev((1:T)[!is.na(ideal.points[,1,18])]), rev(ideal.points[,3,18][!is.na(ideal.points[,1,18])])))
#

titleElem <- c("uav", "PriMadPvem") #ugvalStackPRImadPVEM
#pdf(file=paste(graphdir, "/", titleElem[1], "StackMartinQuinnQuarter", titleElem[2], ".pdf", sep=""), width = 7, height = 7)
par(mar=c(5,4,0,1)+0.1)  ## USA EL ESPACIO DEL TITULO INEXISTENTE
plot(1:T,c(rep(-1.5,times=(T-1)),1.5),type="n",xlab="quarter",
    ylab="95% CIs for ideal points", axes = FALSE)
axis(1, at=c(1:T), labels = FALSE)
axis(1, tick= FALSE, cex.axis=.55, at=c(1:T), labels = c(4,rep(1:4,8)), line=-0.8)
axis(1, tick= FALSE, cex.axis=.8, at=seq(from = 3.5, to = 31.5, by = 4), labels = c("2004","'05","'06","'07","'08","'09","'10","'11") )
axis(2, at=c(-1.5,-1,-.5,0,.5,1,1.5), labels = FALSE)
axis(2, tick= FALSE, cex.axis=.8, at=c(-1,0,1), labels = TRUE )
#lines(c(17,17),c(-3,3),lwd=1,lty=3, col="grey50")
lines(c(11,11),c(-3,3),lwd=1,lty=3, col="grey50"); 
text(c(11,11),c(1.5,1.4),c("Fed. ","election "), adj=1, cex=.65, col="grey50")
lines(c(23,23),c(-3,3),lwd=1,lty=3, col="grey50"); 
text(c(23,23),c(1.5,1.4),c("Fed. ","election "), adj=1, cex=.65, col="grey50")
#lines(c(35,35),c(-3,3),lwd=1,lty=3, col="grey50"); 
#text(c(35,35),c(1.5,1.4),c("Fed. ","election "), adj=1, cex=.65, col="grey50")
##text(c(8.5,8.5),c(.9,.85),c("New ","members "), adj=1, cex=.5, col="grey50")
#polygon(p1, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Ugalde
#polygon(p2, col=rgb(0,0,1,alpha=.25), lty=0)                        ## Albo
polygon(p3, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Andrade
polygon(p4, col=rgb(34/255,139/255,34/255,alpha=.25), lty=0)        ## Alcántar
#polygon(p5, col=rgb(0,0,1,alpha=.25), lty=0)                        ## Glez. Luna
#polygon(p6, col=rgb(1,0,0,alpha=.25), lty=0)                        ## Latapi
polygon(p7, col=rgb(1,0,0,alpha=.25), lty=0)                        ## López Flores
#polygon(p8, col=rgb(0,0,1,alpha=.25), lty=0)                        ## Morales
#polygon(p9, col=rgb(0,0,1,alpha=.25), lty=0)                        ## Sánchez
#polygon(p10, col=rgb(1,215/255,0,alpha=.25), lty=0)                 ## Valdés
polygon(p11, col=rgb(1,0,0,alpha=.25), lty=0)                       ## Baños
#polygon(p12, col=rgb(0,0,1,alpha=.25), lty=0)                       ## Nacif
#polygon(p13, col=rgb(0,0,1,alpha=.25), lty=0)                       ## Elizondo
#polygon(p14, col=rgb(1,215/255,0,alpha=.25), lty=0)                 ## Figueroa
polygon(p15, col=rgb(1,0,0,alpha=.25), lty=0)                       ## Guerrero
#polygon(p16, col=rgb(0,0,1,alpha=.25), lty=0)                       ## Marván
#polygon(p17, col=rgb(1,215/255,0,alpha=.25), lty=0)                 ## Córdova
polygon(p18, col=rgb(1,0,0,alpha=.25), lty=0)                       ## G. Ramírez
## lines(1:14, j1[,2])
## lines(1:14, j6[,2])
## lines(1:14, j8[,2])
## lines(1:14, j10[,2])
## text(2,-0.82,c("*"))
## text(8,-0.82,c("*"))
## text(14,-0.82,c("*"))
## text(4.5,.95,c("I"))
## text(11.5,.95,c("II"))
#dev.off()

######################################################
######################################################
##  PCT PARTY COMPLAINTS (datos salen de omar.do)   ## 
######################################################
######################################################
rm(list = ls())
graphdir <- c("~/Dropbox/ifeSharedGE/graphs")
setwd(graphdir)
#
qtr <- 1:45
shr.pty.complaints <- c(.0229885, .1818182, .2692308, .1333333, .1052632, .125, 0, .0571429, .1081081, .2222222, .0769231, .1168831, .2247191, .313253, .3827161, .78125, .2777778, .2285714, .0625, .3015873, .2439024, .25, .1875, .0675676, .1237113, .2948718, .3015873, .6, .3043478, .122449, .1764706, .1470588, .1612903, .1515152, .0392157, .0666667, .2261905, .3098592, .6206896, .0869565, .2272727, .5111111, .0555556, .1206897, .1395349)
#	
pdf(file="sharePtyComplaintsQuarter.pdf",width=7, height=4.5)
par(mar=c(5,4,.2,1)+0.1)  ## USA EL ESPACIO DEL TITULO INEXISTENTE
#
plot(1:45,c(rep(0,44),max(shr.pty.complaints)),type="n",xlab="quarter",
    ylab="Party complaints as share of all votes", main=NULL, axes=FALSE)
axis(1, at=c(1:45), labels = FALSE)
axis(1, tick=FALSE, cex.axis=.35, at=c(1:45), labels = c(4,rep(1:4,11)), line=-0.8)
axis(1, tick= FALSE,cex.axis=.675, at=seq(from=3.5, to=43.5, by=4),
     labels = c("1997","'98","'99","2000","'01","'02","'03","'04","'05","'06","2007"))
axis(2, at=(0:16)/20, labels = FALSE)
axis(2, tick= FALSE,cex.axis=.75, at=(0:8)/10)
#
color <- rep("grey", times=45)
color[c(2:4,14:16,26:28,38:40)] <- "black" ## ELECTORAL QUARTERS AND THOS SANDWICHING THEM
#
wi <- .25
for (i in 1:45){
  polygon(c(rep(i-wi,2),rep(i+wi,2)), c(0,rep(shr.pty.complaints[i],2),0), col = color[i], border = "black")
}
dev.off()
#
mean ( shr.pty.complaints[ c(2:4,14:16,26:28,38:40)] ) ## Election quarters
mean ( shr.pty.complaints[-c(2:4,14:16,26:28,38:40)] ) ## Rest
mean ( shr.pty.complaints[ c(2:4,14:16,26:28,38:40)] ) / mean ( shr.pty.complaints[-c(2:4,14:16,26:28,38:40)] ) ## RATIO




###################################################################################
###################################################################################
###                                                                             ###
###   HYPOTHESIS TESTS WITH POSTERIOR SAMPLES  (mucho viene de postAnalisis.r)  ###
###                                                                             ###
###################################################################################
###################################################################################

#############################################
##  VERSION CON ESTIMACIONES TRIMESTRALES  ##
#############################################

rm(list=ls())
workdir <-  "~/Dropbox/ifeSharedGE/data"
graphdir <- "~/Dropbox/ifeSharedGE/graphs"
#
# LOAD WOLDENBERG POSTERIORS
setwd(workdir)
load ("DynWmartinQuinnTrimestresLucia.RData")
wo.results <- ife.23; rm(ife.23)
S <- wo.results$n.sims # N OF SAMPLED SIMULATIONS
I <- nrow(all23); J <- 11; T <- 28
all23 <- read.csv("tmp23.csv")
#
## All posterior ideal point simulations by quarter
xxw <- array (NA, dim=c(S, J, T))
xxw[,,1] <-  wo.results$sims.list$x1
xxw[,,2] <-  wo.results$sims.list$x2 
xxw[,,3] <-  wo.results$sims.list$x3 
xxw[,,4] <-  wo.results$sims.list$x4 
xxw[,,5] <-  wo.results$sims.list$x5 
xxw[,,6] <-  wo.results$sims.list$x6 
xxw[,,7] <-  wo.results$sims.list$x7 
xxw[,,8] <-  wo.results$sims.list$x8 
xxw[,,9] <-  wo.results$sims.list$x9 
xxw[,,10] <- wo.results$sims.list$x10
xxw[,,11] <- wo.results$sims.list$x11
xxw[,,12] <- wo.results$sims.list$x12
xxw[,,13] <- wo.results$sims.list$x13
xxw[,,14] <- wo.results$sims.list$x14
xxw[,,15] <- wo.results$sims.list$x15
xxw[,,16] <- wo.results$sims.list$x16
xxw[,,17] <- wo.results$sims.list$x17
xxw[,,18] <- wo.results$sims.list$x18
xxw[,,19] <- wo.results$sims.list$x19
xxw[,,20] <- wo.results$sims.list$x20
xxw[,,21] <- wo.results$sims.list$x21
xxw[,,22] <- wo.results$sims.list$x22
xxw[,,23] <- wo.results$sims.list$x23
xxw[,,24] <- wo.results$sims.list$x24
xxw[,,25] <- wo.results$sims.list$x25
xxw[,,26] <- wo.results$sims.list$x26
xxw[,,27] <- wo.results$sims.list$x27
xxw[,,28] <- wo.results$sims.list$x28
# Thin Lucía's results further
xxw <- xxw[(1:300)*10,,]
## TRIMESTRES CON CONSEJEROS NUEVOS Y VIEJOS PRESENTES EN t=17
qs <- c(.025,.5,.975) ## quantiles you wish to report
ip.w <- array(NA, dim = c(T,3,J)); dimnames(ip.w) <- list(NULL, qs, NULL);
for (t in 1:T){
 	for (j in 1:J){
          ip.w[t,,j] <- quantile( xxw[,j,t], probs = qs, names = FALSE )
        }
      }
ip.w[18:T,,c(7,9)] <- rep(NA,3)
ip.w[1:16,,c(10,11)] <- rep(NA,3)
#
# REMOVE ABSENT MEMBERS
xxw[,c(7,9),18:T] <- NA; xxw[,c(10,11),1:16] <- NA
#
## LOAD UGALDE ET AL POSTERIORS
load ("DynUAVmartinQuinnTrimestres2003-2011lucia.Rdata")
#
ug.results <- ife.45678; rm(ife.45678)
all456789 <- read.csv("tmp456789.csv")
all45678 <- all456789[all456789$term<9,] ## Drop term==9 which Lucía did not include
#
S <- ug.results$n.sims # N OF SAMPLED SIMULATIONS
I <- nrow(all45678); J <- 15; T <- 33
#
## All posterior ideal point simulations by quarter
xxu <- array (NA, dim=c(S, J, T))
xxu[,,1] <-  ug.results$sims.list$x1
xxu[,,2] <-  ug.results$sims.list$x2 
xxu[,,3] <-  ug.results$sims.list$x3 
xxu[,,4] <-  ug.results$sims.list$x4 
xxu[,,5] <-  ug.results$sims.list$x5 
xxu[,,6] <-  ug.results$sims.list$x6 
xxu[,,7] <-  ug.results$sims.list$x7 
xxu[,,8] <-  ug.results$sims.list$x8 
xxu[,,9] <-  ug.results$sims.list$x9 
xxu[,,10] <- ug.results$sims.list$x10
xxu[,,11] <- ug.results$sims.list$x11
xxu[,,12] <- ug.results$sims.list$x12
xxu[,,13] <- ug.results$sims.list$x13
xxu[,,14] <- ug.results$sims.list$x14
xxu[,,15] <- ug.results$sims.list$x15
xxu[,,16] <- ug.results$sims.list$x16
xxu[,,17] <- ug.results$sims.list$x17
xxu[,,18] <- ug.results$sims.list$x18
xxu[,,19] <- ug.results$sims.list$x19
xxu[,,20] <- ug.results$sims.list$x20
xxu[,,21] <- ug.results$sims.list$x21
xxu[,,22] <- ug.results$sims.list$x22
xxu[,,23] <- ug.results$sims.list$x23
xxu[,,24] <- ug.results$sims.list$x24
xxu[,,25] <- ug.results$sims.list$x25
xxu[,,26] <- ug.results$sims.list$x26
xxu[,,27] <- ug.results$sims.list$x27
xxu[,,28] <- ug.results$sims.list$x28
xxu[,,29] <- ug.results$sims.list$x29
xxu[,,30] <- ug.results$sims.list$x30
xxu[,,31] <- ug.results$sims.list$x31
xxu[,,32] <- ug.results$sims.list$x32
xxu[,,33] <- ug.results$sims.list$x33
# Thin Lucía's results further
xxu <- xxu[(1:300)*10,,]
## TRIMESTRES CON CONSEJEROS NUEVOS Y VIEJOS PRESENTES CUANDO ASÍ TOCA
qs <- c(.025,.5,.975) ## quantiles you wish to report
ip.u <- array(NA, dim = c(T,3,J)); dimnames(ip.u) <- list(NULL, qs, NULL);
for (t in 1:T){
 	for (j in 1:J){
          ip.u[t,,j] <- quantile( xxu[,j,t], probs = qs, names = FALSE )
        }
      }
ip.u[18:T,,1] <- rep(NA,3)
ip.u[19:T,,c(6,8)] <- rep(NA,3)
ip.u[1:18,,10:12] <- rep(NA,3)
ip.u[21:T,,c(2,5,7)] <- rep(NA,3)
ip.u[1:20,,13:15] <- rep(NA,3)
ip.u[29:T,,c(3,4,9)] <- rep(NA,3)
#
# REMOVE ABSENT MEMBERS
xxu[,1,18:T] <- NA; xxu[,c(6,8),19:T] <- NA; xxu[,10:12,1:17] <- NA; xxu[,c(2,5,7),21:T] <- NA; xxu[,13:15,1:19] <- NA; xxu[,c(3,4,9),29:T] <- NA
#
#
# SEPARATE PARTY CONTINGENTS IN BOTH CONSEJOS
pan.w <- xxw[,c(5,7,11),]
pri.w <- xxw[,c(1,6,8,10),]
prd.w <- xxw[,c(2,3,4,9),] ## includes cantú
pan.u <- xxu[,c(2,5,8,9,12,13),]
pri.u <- xxu[,c(1,3,4,6,7,11,15),] ## includes green
prd.u <- xxu[,c(10,14),]
#
# STANDARDIZE TO MAKE SPACES COMPARABLE
mw <- mean( xxw[!is.na(xxw)] )
sw <- sd( as.numeric(xxw[!is.na(xxw)]) )
mu <- mean( xxw[!is.na(xxw)] )
su <- sd( as.numeric(xxw[!is.na(xxw)]) )
pan.w <- (pan.w - mw) / sw
pri.w <- (pri.w - mw) / sw
prd.w <- (prd.w - mw) / sw
pan.u <- (pan.u - mu) / su
pri.u <- (pri.u - mu) / su
prd.u <- (prd.u - mu) / su
rm(mw, sw, mu, su)
# Woldenberg objects
T <- dim(xxw)[3]; S <- dim(xxw)[1];
sep.pan <- sep.pri <- sep.prd <- matrix(NA, nrow = S, ncol = T)
med.pan <- med.pri <- med.prd <- matrix(NA, nrow = S, ncol = T)
mean.pan <- mean.pri <- mean.prd <- matrix(NA, nrow = S, ncol = T)
for (t in 1:T){
  sep.pan[,t] <-   apply( pan.w[,,t], 1, function(x) max(na.omit(x)) ) -
                   apply( pan.w[,,t], 1, function(x) min(na.omit(x)) ); # row maximum omitting NAs minus row minimum omitting NAs
  sep.pri[,t] <-   apply( pri.w[,,t], 1, function(x) max(na.omit(x)) ) - 
                   apply( pri.w[,,t], 1, function(x) min(na.omit(x)) ); # row maximum omitting NAs minus row minimum omitting NAs
  sep.prd[,t] <-   apply( prd.w[,,t], 1, function(x) max(na.omit(x)) ) -
                   apply( prd.w[,,t], 1, function(x) min(na.omit(x)) ); # row maximum omitting NAs minus row minimum omitting NAs
  med.pan[,t] <-   apply( pan.w[,,t], 1, function(x) median(na.omit(x)) )  
  med.pri[,t] <-   apply( pri.w[,,t], 1, function(x) median(na.omit(x)) )
  med.prd[,t] <-   apply( prd.w[,,t], 1, function(x) median(na.omit(x)) )  
  mean.pan[,t] <-   apply( pan.w[,,t], 1, function(x) mean(na.omit(x)) )  
  mean.pri[,t] <-   apply( pri.w[,,t], 1, function(x) mean(na.omit(x)) )  
  mean.prd[,t] <-   apply( prd.w[,,t], 1, function(x) mean(na.omit(x)) )  
}
#
elec.quarter <- rep(0, T)
elec.quarter[c(3,15,27)] <- 1 ## Election quarters
elec.quarter[c(3,15,27)-1] <- 1; elec.quarter[c(3,15,27)+1] <- 1; ## Those that sandwich them
#
# Append Ug et al objects
T <- dim(xxu)[3]; S <- dim(xxu)[1];
tmp1 <- tmp2 <- tmp3 <- tmp5 <- tmp6 <- tmp7 <- tmp8 <- tmp9 <- tmp10 <- matrix(NA, nrow = S, ncol = T)
for (t in 1:19){
  tmp1[,t] <-   apply( pan.u[,,t], 1, function(x) max(na.omit(x)) ) -  # row minimum omitting NAs
                apply( pan.u[,,t], 1, function(x) min(na.omit(x)) )  # row maximum omitting NAs
  tmp2[,t] <-   apply( pri.u[,,t], 1, function(x) max(na.omit(x)) ) -  # row minimum omitting NAs
                apply( pri.u[,,t], 1, function(x) min(na.omit(x)) )  # row maximum omitting NAs
#  tmp3[,t] <-   apply( prd.u[,,t], 1, function(x) max(na.omit(x)) ) -  # row minimum omitting NAs       MULTI-MEMBER PRD STARTS IN 20    
#                apply( prd.u[,,t], 1, function(x) min(na.omit(x)) )  # row maximum omitting NAs
  tmp5[,t] <-   apply( pan.u[,,t], 1, function(x) median(na.omit(x)) )  # row median omitting NAs
  tmp6[,t] <-   apply( pri.u[,,t], 1, function(x) median(na.omit(x)) )  # row minimum omitting NAs
#  tmp7[,t] <-   apply( prd.u[,,t], 1, function(x) median(na.omit(x)) )  # row minimum omitting NAs
  tmp8[,t] <-   apply( pan.u[,,t], 1, function(x) mean(na.omit(x)) )  # row minimum omitting NAs
  tmp9[,t] <-   apply( pri.u[,,t], 1, function(x) mean(na.omit(x)) )  # row minimum omitting NAs
#  tmp10[,t] <-  apply( prd.u[,,t], 1, function(x) mean(na.omit(x)) )  # row minimum omitting NAs
}
for (t in 20:T){
  tmp1[,t] <-   apply( pan.u[,,t], 1, function(x) max(na.omit(x)) ) - # row minimum omitting NAs
                apply( pan.u[,,t], 1, function(x) min(na.omit(x)) )  # row maximum omitting NAs
  tmp2[,t] <-   apply( pri.u[,,t], 1, function(x) max(na.omit(x)) ) - # row minimum omitting NAs
                apply( pri.u[,,t], 1, function(x) min(na.omit(x)) )  # row maximum omitting NAs
  tmp3[,t] <-   apply( prd.u[,,t], 1, function(x) max(na.omit(x)) ) - # row minimum omitting NAs
                apply( prd.u[,,t], 1, function(x) min(na.omit(x)) )  # row maximum omitting NAs
  tmp5[,t] <-   apply( pan.u[,,t], 1, function(x) median(na.omit(x)) )  # row median omitting NAs
  tmp6[,t] <-   apply( pri.u[,,t], 1, function(x) median(na.omit(x)) )  # row minimum omitting NAs
  tmp7[,t] <-   apply( prd.u[,,t], 1, function(x) median(na.omit(x)) )  # row minimum omitting NAs
  tmp8[,t] <-   apply( pan.u[,,t], 1, function(x) mean(na.omit(x)) )  # row minimum omitting NAs
  tmp9[,t] <-   apply( pri.u[,,t], 1, function(x) mean(na.omit(x)) )  # row minimum omitting NAs
  tmp10[,t] <-  apply( prd.u[,,t], 1, function(x) mean(na.omit(x)) )  # row minimum omitting NAs
}
#
tmp4 <- rep(0, T)
tmp4[c(11,23)] <- 1 ## Election quarters
tmp4[c(11,23)-1] <- 1; tmp4[c(11,23)+1] <- 1; ## Those that sandwich them
#
sep.pan <- cbind( sep.pan, tmp1); sep.pri <- cbind( sep.pri, tmp2); sep.prd <- cbind( sep.prd, tmp3)
med.pan <- cbind( med.pan, tmp5); med.pri <- cbind( med.pri, tmp6); med.prd <- cbind( med.prd, tmp7)
mean.pan <- cbind( mean.pan, tmp8); mean.pri <- cbind( mean.pri, tmp9); mean.prd <- cbind( mean.prd, tmp10)
elec.quarter <- c( elec.quarter, tmp4 )
rm(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10)
#
##########################################################
# WITHIN-CONTINGENT POLARIZATION, ELEC QUARTERS vs. REST #
##########################################################
no.prd.mm.contingent.quarters <- rep(0, times=33)
no.prd.mm.contingent.quarters[1:19] <- 1
no.prd.mm.contingent.quarters <- c( rep(0, times=28), no.prd.mm.contingent.quarters) ## appends ugEtal to Wold
#  
round (mean (          sep.pan[,elec.quarter==1] ), digits = 3)
round (sd ( as.numeric(sep.pan[,elec.quarter==1]) ), digits = 3)
round (mean (          sep.pan[,elec.quarter==0] ), digits = 3)
round (sd ( as.numeric(sep.pan[,elec.quarter==0]) ), digits = 3)
# 
round (mean (          sep.pri[,elec.quarter==1] ), digits = 3)
round (sd ( as.numeric(sep.pri[,elec.quarter==1]) ), digits = 3)
round (mean (          sep.pri[,elec.quarter==0] ), digits = 3)
round (sd ( as.numeric(sep.pri[,elec.quarter==0]) ), digits = 3)
# 
round (mean ( as.numeric(sep.prd[,elec.quarter==1 & no.prd.mm.contingent.quarters==0])[!is.na(as.numeric(sep.prd[,elec.quarter==1 & no.prd.mm.contingent.quarters==0]))] ), digits = 3)
round (sd   ( as.numeric(sep.prd[,elec.quarter==1 & no.prd.mm.contingent.quarters==0])[!is.na(as.numeric(sep.prd[,elec.quarter==1 & no.prd.mm.contingent.quarters==0]))] ), digits = 3)
round (mean ( as.numeric(sep.prd[,elec.quarter==0 & no.prd.mm.contingent.quarters==0])[!is.na(as.numeric(sep.prd[,elec.quarter==0 & no.prd.mm.contingent.quarters==0]))] ), digits = 3)
round (sd   ( as.numeric(sep.prd[,elec.quarter==0 & no.prd.mm.contingent.quarters==0])[!is.na(as.numeric(sep.prd[,elec.quarter==0 & no.prd.mm.contingent.quarters==0]))] ), digits = 3)
#
## PROBABILITY WITHIN-CONTINGENT POLARIZATION SMALLER ELEC THAN NON-ELEC QUARTERS
difs.pan <- data.frame (
  elec =          as.vector(sep.pan[,elec.quarter==1]),
  noelec = sample(as.vector(sep.pan[,elec.quarter==0]),
    size = length(as.vector(sep.pan[,elec.quarter==1]) ) )
  )
difs.pan$smaller <- difs.pan$elec - difs.pan$noelec; difs.pan$smaller[difs.pan$smaller>=0] <- 0; difs.pan$smaller[difs.pan$smaller<0]  <- 1
difs.pri <- data.frame (
  elec =          as.vector(sep.pri[,elec.quarter==1]),
  noelec = sample(as.vector(sep.pri[,elec.quarter==0]),
    size = length(as.vector(sep.pri[,elec.quarter==1]) ) )
  )
difs.pri$smaller <- difs.pri$elec - difs.pri$noelec; difs.pri$smaller[difs.pri$smaller>=0] <- 0; difs.pri$smaller[difs.pri$smaller<0]  <- 1
difs.prd <- data.frame (
  elec =          as.vector(sep.prd[,elec.quarter==1 & no.prd.mm.contingent.quarters==0]),     
  noelec = sample(as.vector(sep.prd[,elec.quarter==0 & no.prd.mm.contingent.quarters==0]),     
    size = length(as.vector(sep.prd[,elec.quarter==1 & no.prd.mm.contingent.quarters==0]) ) )  
  )
difs.prd$smaller <- difs.prd$elec - difs.prd$noelec; difs.prd$smaller[difs.prd$smaller>=0] <- 0; difs.prd$smaller[difs.prd$smaller<0]  <- 1
#
sum(difs.pan$smaller) / length(difs.pan$smaller)
sum(difs.pri$smaller) / length(difs.pri$smaller)
sum(difs.prd$smaller) / length(difs.prd$smaller)
#
###########################################################
# BETWEEN-CONTINGENT POLARIZATION, ELEC QUARTERS vs. REST #
###########################################################
round(mean (           abs( med.pan[,elec.quarter==1] - med.pri[,elec.quarter==1] ) ), digits = 3)
round(sd ( as.numeric( abs( med.pan[,elec.quarter==1] - med.pri[,elec.quarter==1] ) ) ), digits = 3)
round(mean (           abs( med.pan[,elec.quarter==0] - med.pri[,elec.quarter==0] ) ) , digits = 3)
round(sd ( as.numeric( abs( med.pan[,elec.quarter==0] - med.pri[,elec.quarter==0] ) ) ), digits = 3)
#
round(mean (           abs( med.pan[,elec.quarter==1 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==1  & no.prd.mm.contingent.quarters==0] ) ) , digits = 3)
round(sd ( as.numeric( abs( med.pan[,elec.quarter==1 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==1  & no.prd.mm.contingent.quarters==0] ) ) ), digits = 3)
round(mean (           abs( med.pan[,elec.quarter==0 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==0  & no.prd.mm.contingent.quarters==0] ) ) , digits = 3)
round(sd ( as.numeric( abs( med.pan[,elec.quarter==0 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==0  & no.prd.mm.contingent.quarters==0] ) ) ), digits = 3)
#
round(mean (           abs( med.pri[,elec.quarter==1 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==1  & no.prd.mm.contingent.quarters==0] ) ) , digits = 3)
round(sd ( as.numeric( abs( med.pri[,elec.quarter==1 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==1  & no.prd.mm.contingent.quarters==0] ) ) ), digits = 3)
round(mean (           abs( med.pri[,elec.quarter==0 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==0  & no.prd.mm.contingent.quarters==0] ) ) , digits = 3)
round(sd ( as.numeric( abs( med.pri[,elec.quarter==0 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==0  & no.prd.mm.contingent.quarters==0] ) ) ), digits = 3)
#
## PROBABILITY BETWEEN-CONTINGENT POLARIZATION LARGER ELEC THAN NON-ELEC QUARTERS
difs.pan.pri <- data.frame (
  elec =          as.vector(abs( med.pan[,elec.quarter==1] - med.pri[,elec.quarter==1] )),
  noelec = sample(as.vector(abs( med.pan[,elec.quarter==0] - med.pri[,elec.quarter==0] )),
    size = length(as.vector(abs( med.pan[,elec.quarter==1] - med.pri[,elec.quarter==1] )) ) )
  )
difs.pan.pri$smaller <- difs.pan.pri$elec - difs.pan.pri$noelec; difs.pan.pri$smaller[difs.pan.pri$smaller>=0] <- 0; difs.pan.pri$smaller[difs.pan.pri$smaller<0]  <- 1
difs.pan.prd <- data.frame (
  elec =          as.vector(abs( med.pan[,elec.quarter==1 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==1  & no.prd.mm.contingent.quarters==0] )),
  noelec = sample(as.vector(abs( med.pan[,elec.quarter==0 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==0  & no.prd.mm.contingent.quarters==0] )),
    size = length(as.vector(abs( med.pan[,elec.quarter==1 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==1  & no.prd.mm.contingent.quarters==0] )) ) )
  )
difs.pan.prd$smaller <- difs.pan.prd$elec - difs.pan.prd$noelec; difs.pan.prd$smaller[difs.pan.prd$smaller>=0] <- 0; difs.pan.prd$smaller[difs.pan.prd$smaller<0]  <- 1
difs.pri.prd <- data.frame (
  elec =          as.vector(abs( med.pri[,elec.quarter==1 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==1  & no.prd.mm.contingent.quarters==0] )),
  noelec = sample(as.vector(abs( med.pri[,elec.quarter==0 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==0  & no.prd.mm.contingent.quarters==0] )),
    size = length(as.vector(abs( med.pri[,elec.quarter==1 & no.prd.mm.contingent.quarters==0] - med.prd[,elec.quarter==1  & no.prd.mm.contingent.quarters==0] )) ) )
  )
difs.pri.prd$smaller <- difs.pri.prd$elec - difs.pri.prd$noelec; difs.pri.prd$smaller[difs.pri.prd$smaller>=0] <- 0; difs.pri.prd$smaller[difs.pri.prd$smaller<0]  <- 1
#
sum(difs.pan.pri$smaller) / length(difs.pan.pri$smaller)
sum(difs.pan.prd$smaller) / length(difs.pan.prd$smaller)
sum(difs.pri.prd$smaller) / length(difs.pri.prd$smaller)


#############################################
# POSTERIOR SIGNALS; ELEC QUARTERS VS. REST #
#############################################
all45678 <- read.csv("tmp45678trim.out") ## USA INFO DE ITEM QUE PREPARO LUCIA CON ARCHIVO CADUCO. CUANDO SE CORRA IRT CON TODOS LOS DATOS USAREMOS EL DE SIEMPRE
## ALGO DEJO DE JALAR CON EL CAMBIO AL ULTIMO OBJETO DE TRIMS UG ET AL
signals.w <- wo.results$sims.list$delta[(1:300)*10,] #Thin Lucia's results further
signals.u <- ug.results$sims.list$delta[(1:300)*10,] #Thin Lucia's results further
signals <- cbind (signals.w, signals.u)
#
## replace semester by quarter (trimester) counter if so needed
quarter.w <- all23$yrtrim*4-7987
quarter.u <- all45678$t #all45678$yrtrim*4-8015
#quarter <- c(quarter.w, all456789$yrtrim*4-7987)
quarter <- c(quarter.w, quarter.u+max(quarter.w))#all45678$yrtrim*4-7987)
#
I <- dim(signals.w)[2]; S <- dim(signals.w)[1];
elec.items.w <- inaug.items.w <- rep(0, times=I)
elec.items.w[quarter.w==c(3,15,27)] <- 1 ## Election quarters
elec.items.w[quarter.w==c(3,15,27)-1] <- 1; elec.items.w[quarter.w==c(3,15,27)-1] <- 1 ## Those that sandwich them
inaug.items.w[quarter.w==1] <- 1 ## Inaugural quarter
#
I <- dim(signals.u)[2]; S <- dim(signals.u)[1];
elec.items.u <- inaug.items.u <- rep(0, times=I)
elec.items.u[quarter.u==c(11,23)] <- 1 ## Election quarters
elec.items.u[quarter.u==c(11,23)-1] <- 1; elec.items.u[quarter.u==c(11,23)-1] <- 1 ## Those that sandwich them
inaug.items.u[quarter.u==1] <- 1 ## Inaugural quarter
#
elec.items <- c(elec.items.w, elec.items.u)
inaug.items <- c(inaug.items.w, inaug.items.u)
#
## MEAN ABSOLUTE POSTERIOR SIGNAL WITHOUT INAUGURAL QUARTERS
mean (            abs(signals[,elec.items==1 & inaug.items==0]) )
sd ( as.character(abs(signals[,elec.items==1 & inaug.items==0])) )
mean (            abs(signals[,elec.items==0 & inaug.items==0]) )
sd ( as.character(abs(signals[,elec.items==0 & inaug.items==0])) )
#
## MEAN ABSOLUTE POSTERIOR SIGNAL
mean (            abs(signals[,elec.items==1]) )
sd ( as.character(abs(signals[,elec.items==1])) )
mean (            abs(signals[,elec.items==0]) )
sd ( as.character(abs(signals[,elec.items==0])) )
#
## PERCENTAGE SIGNALS WITH 95ci OFF ZERO
hi <- apply ( signals[,elec.items==1], 2, function(x) quantile(x, probs = .975, names = FALSE ))
lo <- apply ( signals[,elec.items==1], 2, function(x) quantile(x, probs = .025, names = FALSE ))
aa <- length(lo[lo>0 & hi>0]); bb <- length(lo[lo<0 & hi>0]); cc <- length(lo[lo<0 & hi<0])
(aa+cc)/(aa+bb+cc)
length(lo)
#
hi <- apply ( signals[,elec.items==0], 2, function(x) quantile(x, probs = .975, names = FALSE ))
lo <- apply ( signals[,elec.items==0], 2, function(x) quantile(x, probs = .025, names = FALSE ))
aa <- length(lo[lo>0 & hi>0]); bb <- length(lo[lo<0 & hi>0]); cc <- length(lo[lo<0 & hi<0])
(aa+cc)/(aa+bb+cc)
length(lo)
#
## PROBABILITY SIGNAL GREATER ELECTION THAN NON-ELECTION QUARTERS
sigs <- data.frame (
  elec = as.vector(abs(signals[,elec.items==1])),
  noelec = sample(as.vector(abs(signals[,elec.items==0])),
                  size = length(as.vector(abs(signals[,elec.items==1])))
                 ) )
sigs$larger <- sigs$elec - sigs$noelec
sigs$larger[sigs$larger>0]  <- 1
sigs$larger[sigs$larger<=0] <- 0
#
sum(sigs$larger) / length(sigs$larger)



######################
## ABSTENTION RATES ##
######################
## rm(list = ls())
## workdir <- c("~/Dropbox/ifeSharedGE/data")
## setwd(workdir)
## all23 <- read.csv("tmp23.csv")
## all456789 <- read.csv("tmp456789.csv")
## all45678 <- all456789[all456789$term<9,] # drop when using whole period
#
abst.w <- abs(all23[,1:11]); abst.w <- 1-abst.w
abst.u <- abs(all45678[,1:15]); abst.u <- 1-abst.u
#abst.u <- abs(all456789[,1:18]); abst.u <- 1-abst.u
#
# REMOVE NON-MEMBERS
abst.w[all23$term==3,c(7,9)] <- NA; abst.w[all23$term==2,c(10,11)] <- NA
abst.u[all45678$term>4,1] <- NA; abst.u[all45678$term>5,c(6,8)] <- NA; abst.u[all45678$term<6,10:12] <- NA; abst.u[all45678$term>6,c(2,5,7)] <- NA; abst.u[all45678$term<7,13:15] <- NA; abst.u[all45678$term>7,c(3,4,9)] <- NA
#abst.u[all456789$term>4,1] <- NA; abst.u[all456789$term>5,c(6,8)] <- NA; abst.u[all456789$term<6,10:12] <- NA; abst.u[all456789$term>6,c(2,5,7)] <- NA; abst.u[all456789$term<7,13:15] <- NA; abst.u[all456789$term>7,c(3,4,9)] <- NA; abst.u[all456789$term<9,17:18] <- NA
#
party.w <- c(2,3,3,3,1,2,1,2,3,2,1)
party.u <- c(2,1,2,2,1,2,2,1,1,3,2,1,1,3,2) ## Alcántar as priista
#party.u(2,1,2,2,1,2,2,1,1,3,2,1,1,3,2,1,3,2) ## Alcántar as priista
#
# PAN
num.w <- sum (apply (abst.w[elec.items.w==1,party.w==1], 1, function(x) sum(na.omit(x))))
den.w <- sum (apply ( abst.w[elec.items.w==1,party.w==1], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
num.u <- sum (apply (abst.u[elec.items.u==1,party.u==1], 1, function(x) sum(na.omit(x))))
den.u <- sum (apply ( abst.u[elec.items.u==1,party.u==1], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
(num.w+num.u)/(den.w+den.u) # Proporción de abstenciones
#
num.w <- sum (apply (abst.w[elec.items.w==0,party.w==1], 1, function(x) sum(na.omit(x))))
den.w <- sum (apply ( abst.w[elec.items.w==0,party.w==1], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
num.u <- sum (apply (abst.u[elec.items.u==0,party.u==1], 1, function(x) sum(na.omit(x))))
den.u <- sum (apply ( abst.u[elec.items.u==0,party.u==1], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
(num.w+num.u)/(den.w+den.u) # Proporción de abstenciones
#
# PRI
num.w <- sum (apply (abst.w[elec.items.w==1,party.w==2], 1, function(x) sum(na.omit(x))))
den.w <- sum (apply ( abst.w[elec.items.w==1,party.w==2], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
num.u <- sum (apply (abst.u[elec.items.u==1,party.u==2], 1, function(x) sum(na.omit(x))))
den.u <- sum (apply ( abst.u[elec.items.u==1,party.u==2], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
(num.w+num.u)/(den.w+den.u) # Proporción de abstenciones
#
num.w <- sum (apply (abst.w[elec.items.w==0,party.w==2], 1, function(x) sum(na.omit(x))))
den.w <- sum (apply ( abst.w[elec.items.w==0,party.w==2], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
num.u <- sum (apply (abst.u[elec.items.u==0,party.u==2], 1, function(x) sum(na.omit(x))))
den.u <- sum (apply ( abst.u[elec.items.u==0,party.u==2], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
(num.w+num.u)/(den.w+den.u) # Proporción de abstenciones
#
# PRD
num.w <- sum (apply (abst.w[elec.items.w==1,party.w==3], 1, function(x) sum(na.omit(x))))
den.w <- sum (apply ( abst.w[elec.items.w==1,party.w==3], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
num.u <- sum (apply (abst.u[elec.items.u==1,party.u==3], 1, function(x) sum(na.omit(x))))
den.u <- sum (apply ( abst.u[elec.items.u==1,party.u==3], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
(num.w+num.u)/(den.w+den.u) # Proporción de abstenciones
#
num.w <- sum (apply (abst.w[elec.items.w==0,party.w==3], 1, function(x) sum(na.omit(x))))
den.w <- sum (apply ( abst.w[elec.items.w==0,party.w==3], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
num.u <- sum (apply (abst.u[elec.items.u==0,party.u==3], 1, function(x) sum(na.omit(x))))
den.u <- sum (apply ( abst.u[elec.items.u==0,party.u==3], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ))
(num.w+num.u)/(den.w+den.u) # Proporción de abstenciones
#
# PAN
mean (
  apply (abst.w[elec.items.w==1,party.w==1], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones en contingente
  apply (apply ( abst.w[elec.items.w==1,party.w==1], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
sd (
  apply (abst.w[elec.items.w==1,party.w==1], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones del contingente
  apply (apply ( abst.w[elec.items.w==1,party.w==1], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
#
mean (
  apply (abst.w[elec.items.w==0,party.w==1], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones en contingente
  apply (apply ( abst.w[elec.items.w==0,party.w==1], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
sd (
  apply (abst.w[elec.items.w==0,party.w==1], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones del contingente
  apply (apply ( abst.w[elec.items.w==0,party.w==1], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
#
# PRI
mean (
  apply (abst.w[elec.items.w==1,party.w==2], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones en contingente
  apply (apply ( abst.w[elec.items.w==1,party.w==2], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
sd (
  apply (abst.w[elec.items.w==1,party.w==2], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones del contingente
  apply (apply ( abst.w[elec.items.w==1,party.w==2], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
#
mean (
  apply (abst.w[elec.items.w==0,party.w==2], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones en contingente
  apply (apply ( abst.w[elec.items.w==0,party.w==2], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
sd (
  apply (abst.w[elec.items.w==0,party.w==2], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones del contingente
  apply (apply ( abst.w[elec.items.w==0,party.w==2], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
#
# PRD
mean (
  apply (abst.w[elec.items.w==1,party.w==3], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones en contingente
  apply (apply ( abst.w[elec.items.w==1,party.w==3], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
sd (
  apply (abst.w[elec.items.w==1,party.w==3], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones del contingente
  apply (apply ( abst.w[elec.items.w==1,party.w==3], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
#
mean (
  apply (abst.w[elec.items.w==0,party.w==3], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones en contingente
  apply (apply ( abst.w[elec.items.w==0,party.w==3], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
sd (
  apply (abst.w[elec.items.w==0,party.w==3], 1, function(x) sum(na.omit(x))) /                                     # núm. de abstenciones del contingente
  apply (apply ( abst.w[elec.items.w==0,party.w==3], c(1,2), function(x) ifelse (is.na(x)==TRUE, 0, 1) ), 1, sum)  # entre tamaño del contingente
  ) 
#








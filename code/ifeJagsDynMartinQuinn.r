# -*- coding: utf-8 -*-
# Invoca Jags desde R
##
library(arm)
library (MCMCpack)
library (foreign)
library (car)
library (gtools)
library (lubridate)
#library (multicore)

#library (R2WinBUGS)
library (R2jags)
#library(BRugs)

rm(list = ls())
workdir <- c("~/Dropbox/data/rollcall/ife_cg/ife-update/")
setwd(workdir)
#set.seed(1970)

## names.1 <- c("Woldenberg", "segob", "senpri", "senprd", "dippri", "dippan", "Creel", "Granados", "Zertuche", "Pinchetti", "Pozas")
## names.2 <- c("Woldenberg", "Barragán", "Cantú", "Cárdenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebadúa")
## names.3 <- c("Woldenberg", "Barragán", "Cantú", "Cárdenas", "Lujambio", "Merino", "Peschard", "Rivera", "Luken")
names.23 <- c("Woldenberg", "Barragán", "Cantú", "Cárdenas", "Lujambio", "Merino", "Molinar", "Peschard", "Zebadúa", "Rivera", "Luken")
## names.4 <- c("Ugalde", "Albo", "Andrade", "Alcántar", "Glez. Luna", "Latapi", "López Flores", "Morales", "Sánchez")
## names.5 <-           c("Albo", "Andrade", "Alcántar", "Glez. Luna", "Latapi", "López Flores", "Morales", "Sánchez")
## names.6 <- c("Valdés", "Albo", "Andrade", "Alcántar", "Baños", "Glez. Luna", "López Flores", "Nacif", "Sánchez")
## names.7 <- c("Valdés", "Andrade", "Alcántar", "Baños", "Elizondo", "Figueroa", "Guerrero", "Nacif", "Sánchez")
## names.8 <- c("Valdés", "Baños", "Elizondo", "Figueroa", "Guerrero", "Nacif")
## names.9 <- c("Valdés", "Baños", "Elizondo", "Figueroa", "Guerrero", "Nacif", "Marván", "Córdova", "García Rmz.")
## names.4567 <- c("Ugalde", "Albo", "Andrade", "Alcántar", "Glez. Luna", "Latapi", "López Flores", "Morales", "Sánchez", "Valdés", "Baños", "Nacif", "Elizondo", "Figueroa", "Guerrero")
## names.45678 <- c("Ugalde", "Albo", "Andrade", "Alcántar", "Glez. Luna", "Latapi", "López Flores", "Morales", "Sánchez", "Valdés", "Baños", "Nacif", "Elizondo", "Figueroa", "Guerrero")
names.456789 <- c("Ugalde", "Albo", "Andrade", "Alcántar", "Glez. Luna", "Latapi", "López Flores", "Morales", "Sánchez", "Valdés", "Baños", "Nacif", "Elizondo", "Figueroa", "Guerrero", "Marván", "Córdova", "García Rmz.")

color.23 <- c("red", "gold", "gold", "gold",  "blue", "red", "blue", "red", "gold", "red", "blue")
rgb.23<-matrix(NA,11,3)
rgb.23[1,]<-  c(255,  0,  0)/255 #red
rgb.23[2,]<-  c(255,215,  0)/255 #gold
rgb.23[3,]<-  c(255,215,  0)/255 #gold
rgb.23[4,]<-  c(255,215,  0)/255 #gold
rgb.23[5,]<-  c(  0,  0,255)/255 #blue
rgb.23[6,]<-  c(255,  0,  0)/255 #red
rgb.23[7,]<-  c(  0,  0,255)/255 #blue
rgb.23[8,]<-  c(255,  0,  0)/255 #red
rgb.23[9,]<-  c(255,215,  0)/255 #gold
rgb.23[10,]<- c(255,  0,  0)/255 #red
rgb.23[11,]<- c(  0,  0,255)/255 #blue
#
color.4567 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red")
rgb.4567<-matrix(NA,15,3)
rgb.4567[1,] <- c(255,  0,  0)/255 #red
rgb.4567[2,] <- c(  0,  0,255)/255 #blue
rgb.4567[3,] <- c(255,  0,  0)/255 #red
rgb.4567[4,] <- c( 34,139, 34)/255 #verde
rgb.4567[5,] <- c(  0,  0,255)/255 #blue
rgb.4567[6,] <- c(255,  0,  0)/255 #red
rgb.4567[7,] <- c(255,  0,  0)/255 #red
rgb.4567[8,] <- c(  0,  0,255)/255 #blue
rgb.4567[9,] <- c(  0,  0,255)/255 #blue
rgb.4567[10,]<- c(255,215,  0)/255 #gold
rgb.4567[11,]<- c(255,  0,  0)/255 #red
rgb.4567[12,]<- c(  0,  0,255)/255 #blue
rgb.4567[13,]<- c(  0,  0,255)/255 #blue
rgb.4567[14,]<- c(255,215,  0)/255 #gold
rgb.4567[15,]<- c(255,  0,  0)/255 #red
#
color.45678 <- c("red", "blue", "red", "green",  "blue", "red", "red", "blue", "blue", "gold", "red", "blue", "blue", "gold", "red")
rgb.45678 <- rgb.4567
color.456789 <- c(color.45678, "blue", "gold", "red")
rgb.456789 <- rgb.45678
rgb.456789 <- rbind(rgb.456789, c(  0,  0,255)/255) #blue
rgb.456789 <- rbind(rgb.456789, c(255,215,  0)/255) #gold
rgb.456789 <- rbind(rgb.456789, c(255,  0,  0)/255) #red
#
#c(255,215,  0)/255 #gold
#c(255,140,  0)/255 #naranja
#c(  0,  0,255)/255 #blue
#c(255,  0,  0)/255 #red
#c( 34,139, 34)/255 #verde

## greys.4567 <- c("grey50", "grey30", "grey50", "grey50", "grey30", "grey50", "grey50", "grey30", "grey30",
##                 "grey70", "grey50", "grey30", "grey30", "grey70", "grey50")
## greyLines.4567 <- matrix(NA, nrow=15, ncol=2)
## greyLines.4567[,1] <- c("grey80", "white", "grey80", "grey80", "white", "grey80", "grey80", "white", "white",
##                         "black", "grey80", "white", "white", "black", "grey80")
## greyLines.4567[,2] <- c(1,1,1,3,1,1,1,1,1,1,1,1,1,1,1)
#
#  area    lines   linetype
#"grey30" "white"    1 #pan
#"grey50" "grey80"   1 #pri
#"grey70" "black"    1 #prd
#"grey70" "black"    3 #pt
#"grey50" "grey80"   3 #pvem
all23       <- read.csv(      "data/v23.csv",  header=TRUE)
all456789ab <- read.csv("data/v456789ab.csv",  header=TRUE)

# sort by date
all23 <- all23[order(all23$date),]
all456789ab <- all456789ab[order(all456789ab$date),]

# replace semester by quarter (trimester) counter
all23$t       <- as.numeric(factor(    all23$qtr*10))
all456789ab$t <- as.numeric(factor(all456789ab$qtr*10))


## Create object with session traits (choose one)
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

## IN AND OUT SUMMARY QUARTERS (ROMAN NUMERALS CORRESPOND TO THOSE IN PLOT)
#        term event         date       qtr (progress)  qr_count
# I         2 Molinar out   5/12/2000  2001.1 (35%)    17
# II        3 Luken in     11/12/2000  2001.1 (40%)    17
#             All Wold out 31/10/2003  2003.4 (100%)   28
# III     4&5 All Ugalde in 3/11/2003  2004.1  (0%)    29 or 1
#             Ugalde out   14/12/2007  2008.1 (50%)    45 or 17
#             Latapí out     7/2/2008  2008.2 (10%)    46 or 18
#             Morales out    7/2/2008  2008.2 (50%)    46 or 18
# IV        6 Valdés in      8/2/2008  2008.2 (10%)    46 or 18
#             Nacif in       8/2/2008  2008.2 (10%)    46 or 18
#             Baños in       8/2/2008  2008.2 (10%)    46 or 18
#             Albo out      14/8/2008  2008.4 (20%)    48 or 20
#             GlezLuna out  14/8/2008  2008.4 (20%)    48 or 20
#             LpzFlores out 14/8/2008  2008.4 (20%)    48 or 20
# V         7 Figueroa in   15/8/2008  2008.4 (20%)    48 or 20
#             Guerrero in   15/8/2008  2008.4 (20%)    48 or 20
#             Elizondo in   15/8/2008  2008.4 (20%)    48 or 20
# VI        8 Sánchez out  31/10/2010  2010.4 (100%)   56 or 28
#             Andrade out  31/10/2010  2010.4 (100%)   56 or 28
#             Alcántar out 31/10/2010  2010.4 (100%)   56 or 28
# VII       9 Córdova in   15/11/2011  2012.1 (20%)    61 or 33
#             Marván in    15/11/2011  2012.1 (20%)    61 or 33
#             GarcíaRmz in 15/11/2011  2012.1 (20%)    61 or 33
# VIII  10&11 G. Rmz out     8/2/2013  2013.2 (10%)    66 or 38
#             Valdés out   30/10/2013  2013.4 (100%)   68 or 40
#             Elizondo out 30/10/2013  2013.4 (100%)   68 or 40
#             Figueroa out 30/10/2013  2013.4 (100%)   68 or 40
#             Guerrero out 30/10/2013  2013.4 (100%)   68 or 40
# IX       12 Andrade2 in   11/4/2014  2014.2  (68%)   70 or 42
#             Favela in     11/4/2014  2014.2  (68%)   70 or 42
#             Galindo in    11/4/2014  2014.2  (68%)   70 or 42
#             Murayama in   11/4/2014  2014.2  (68%)   70 or 42
#             Ruiz in       11/4/2014  2014.2  (68%)   70 or 42
#             Sánchez in    11/4/2014  2014.2  (68%)   70 or 42
#             Santiago in   11/4/2014  2014.2  (68%)   70 or 42
#             SnMartín in   11/4/2014  2014.2  (68%)   70 or 42
#             Baños out      3/4/2014  2014.2  (68%)   70 or 42
#             Marván out     3/4/2014  2014.2  (68%)   70 or 42
#             Nacif out      3/4/2014  2014.2  (68%)   70 or 42
#
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
# add remainder here...


###############################################
###     WOLDENBERG A LA MARTIN-QUINN        ###
##############################################

### MODEL: 14 semesters or 28 quarters (WOLDENBERG 1y2 together) extremist anchors # # # # # #
model1Dj.irt <- function() {
  for (j in 1:J){                ## loop over councilors
    for (i in 1:I){              ## loop over items
      v[j,i] ~ dbern(p[j,i]);    ## voting rule
      probit(p[j,i]) <- mu[j,i]; ## sets 0<p<1 as function of mu
      prod1[j,i]    <- x1[j]*d1[i]*p1[j]    + x2[j]*d2[i]*p2[j];
      prod2[j,i]    <- x3[j]*d3[i]*p3[j]    + x4[j]*d4[i]*p4[j];
      prod3[j,i]    <- x5[j]*d5[i]*p5[j]    + x6[j]*d6[i]*p6[j];
      prod4[j,i]    <- x7[j]*d7[i]*p7[j]    + x8[j]*d8[i]*p8[j];
      prod5[j,i]    <- x9[j]*d9[i]*p9[j]    + x10[j]*d10[i]*p10[j];
      prod6[j,i]    <- x11[j]*d11[i]*p11[j] + x12[j]*d12[i]*p12[j];
      prod7[j,i]    <- x13[j]*d13[i]*p13[j] + x14[j]*d14[i]*p14[j];
      prod8[j,i]    <- x15[j]*d15[i]*p15[j] + x16[j]*d16[i]*p16[j];
      prod9[j,i]    <- x17[j]*d17[i]*p17[j] + x18[j]*d18[i]*p18[j];
      prod10[j,i]   <- x19[j]*d19[i]*p19[j] + x20[j]*d20[i]*p20[j];
      prod11[j,i]   <- x21[j]*d21[i]*p21[j] + x22[j]*d22[i]*p22[j];
      prod12[j,i]   <- x23[j]*d23[i]*p23[j] + x24[j]*d24[i]*p24[j];
      prod13[j,i]   <- x25[j]*d25[i]*p25[j] + x26[j]*d26[i]*p26[j];
      prod14[j,i]   <- x27[j]*d27[i]*p27[j] + x28[j]*d28[i]*p28[j];
      sumprod1[j,i] <- prod1[j,i] + prod2[j,i] + prod3[j,i] + prod4[j,i] + prod5[j,i] + prod6[j,i] + prod7[j,i];
      sumprod2[j,i] <- prod8[j,i] + prod9[j,i] + prod10[j,i] + prod11[j,i] + prod12[j,i] + prod13[j,i] + prod14[j,i];
      sumprod[j,i] <- sumprod1[j,i] + sumprod2[j,i]
      ## mu[j,i] <- signal[i]*(x1[j]*d1[i]*p1[j] + x2[j]*d2[i]*p2[j]
      ##                      + x3[j]*d3[i]*p3[j] + x4[j]*d4[i]*p4[j]
      ##                      + x5[j]*d5[i]*p5[j] + x6[j]*d6[i]*p6[j]
      ##                      + x7[j]*d7[i]*p7[j] + x8[j]*d8[i]*p8[j]
      ##                      + x9[j]*d9[i]*p9[j] + x10[j]*d10[i]*p10[j]
      ##                      + x11[j]*d11[i]*p11[j] + x12[j]*d12[i]*p12[j]
      ##                      + x13[j]*d13[i]*p13[j] + x14[j]*d14[i]*p14[j]) - difficulty[i];   ## utility differential
      ## for (t in 1:T){ #inprod(beta[],X[n,])
      ##     mu[j,i] <- signal[i]*inprod(x[t,],dp[i,,t]) - difficulty[i];     ## utility differential
      ## }
      mu[j,i] <- signal[i]*sumprod[j,i] - difficulty[i];     ## utility differential
                }
      x1[j]  ~ dnorm ( x0[j],50);  ## Autoregressive process
      x2[j]  ~ dnorm ( x1[j],50);
      x3[j]  ~ dnorm ( x2[j],50);
      x4[j]  ~ dnorm ( x3[j],50);
      x5[j]  ~ dnorm ( x4[j],50);
      x6[j]  ~ dnorm ( x5[j],50);
      x7[j]  ~ dnorm ( x6[j],50);
      x8[j]  ~ dnorm ( x7[j],50);
      x9[j]  ~ dnorm ( x8[j],50);
      x10[j] ~ dnorm ( x9[j],50);
      x11[j] ~ dnorm (x10[j],50);
      x12[j] ~ dnorm (x11[j],50);
      x13[j] ~ dnorm (x12[j],50);
      x14[j] ~ dnorm (x13[j],50);
      x15[j] ~ dnorm (x14[j],50);
      x16[j] ~ dnorm (x15[j],50);
      x17[j] ~ dnorm (x16[j],50);
      x18[j] ~ dnorm (x17[j],50);
      x19[j] ~ dnorm (x18[j],50);
      x20[j] ~ dnorm (x19[j],50);
      x21[j] ~ dnorm (x20[j],50);
      x22[j] ~ dnorm (x21[j],50);
      x23[j] ~ dnorm (x22[j],50);
      x24[j] ~ dnorm (x23[j],50);
      x25[j] ~ dnorm (x24[j],50);
      x26[j] ~ dnorm (x25[j],50);
      x27[j] ~ dnorm (x26[j],50);
      x28[j] ~ dnorm (x27[j],50);
              }
  ## for (i in 1:I){
  ##    m[i] <- difficulty[i] / signal[i]                                      ## midpoint
  ## }
  ## priors
    x0[1] ~ dnorm(1, 4);    #Woldenberg
    x0[2] ~ dnorm(2, 4);    #barragan
    x0[3] ~ dnorm(0, 1);    #cantu
    x0[4] ~ dnorm(-2, 4);   #cardenas
    x0[5] ~ dnorm(0, 1);    #lujambio
    x0[6] ~ dnorm(0, 1);    #merino
    x0[7] ~ dnorm(0, 1);    #molinar
    x0[8] ~ dnorm(0, 1);    #peschard
    x0[9] ~ dnorm(0, 1);    #zebadua
    x0[10] ~ dnorm(0, 1);   #rivera
    x0[11] ~ dnorm(0, 1);   #luken
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
d1  <- ifelse(time==1,1,0)
d2  <- ifelse(time==2,1,0)
d3  <- ifelse(time==3,1,0)
d4  <- ifelse(time==4,1,0)
d5  <- ifelse(time==5,1,0)
d6  <- ifelse(time==6,1,0)
d7  <- ifelse(time==7,1,0)
d8  <- ifelse(time==8,1,0)
d9  <- ifelse(time==9,1,0)
d10 <- ifelse(time==10,1,0)
d11 <- ifelse(time==11,1,0)
d12 <- ifelse(time==12,1,0)
d13 <- ifelse(time==13,1,0)
d14 <- ifelse(time==14,1,0)
d15 <- ifelse(time==15,1,0)
d16 <- ifelse(time==16,1,0)
d17 <- ifelse(time==17,1,0)
d18 <- ifelse(time==18,1,0)
d19 <- ifelse(time==19,1,0)
d20 <- ifelse(time==20,1,0)
d21 <- ifelse(time==21,1,0)
d22 <- ifelse(time==22,1,0)
d23 <- ifelse(time==23,1,0)
d24 <- ifelse(time==24,1,0)
d25 <- ifelse(time==25,1,0)
d26 <- ifelse(time==26,1,0)
d27 <- ifelse(time==27,1,0)
d28 <- ifelse(time==28,1,0)
## ##  SEMESTRALES
## ##       1 2 3 4 5 6 7 8 9 0 1
## p1 <-  c(1,1,1,1,1,1,1,1,1,0,0) # presente o ausente en el semestre
## p2 <-  c(1,1,1,1,1,1,1,1,1,0,0)
## p3 <-  c(1,1,1,1,1,1,1,1,1,0,0)
## p4 <-  c(1,1,1,1,1,1,1,1,1,0,0)
## p5 <-  c(1,1,1,1,1,1,1,1,1,0,0)
## p6 <-  c(1,1,1,1,1,1,1,1,1,0,0)
## p7 <-  c(1,1,1,1,1,1,1,1,1,0,0)
## p8 <-  c(1,1,1,1,1,1,1,1,1,0,0)
## p9 <-  c(1,1,1,1,1,1,0,1,0,1,1)
## p10 <- c(1,1,1,1,1,1,0,1,0,1,1)
## p11 <- c(1,1,1,1,1,1,0,1,0,1,1)
## p12 <- c(1,1,1,1,1,1,0,1,0,1,1)
## p13 <- c(1,1,1,1,1,1,0,1,0,1,1)
## p14 <- c(1,1,1,1,1,1,0,1,0,1,1)
## TRIMESTRALES
##       1 2 3 4 5 6 7 8 9 0 1
p1 <-  c(1,1,1,1,1,1,1,1,1,0,0) # presente o ausente en el semestre
p2 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p3 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p4 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p5 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p6 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p7 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p8 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p9 <-  c(1,1,1,1,1,1,1,1,1,0,0)
p10 <- c(1,1,1,1,1,1,1,1,1,0,0)
p11 <- c(1,1,1,1,1,1,1,1,1,0,0)
p12 <- c(1,1,1,1,1,1,1,1,1,0,0)
p13 <- c(1,1,1,1,1,1,1,1,1,0,0)
p14 <- c(1,1,1,1,1,1,1,1,1,0,0)
p15 <- c(1,1,1,1,1,1,1,1,1,0,0)
p16 <- c(1,1,1,1,1,1,1,1,1,0,0)
p17 <- c(1,1,1,1,1,1,1,1,1,0,0)
p18 <- c(1,1,1,1,1,1,0,1,0,1,1)
p19 <- c(1,1,1,1,1,1,0,1,0,1,1)
p20 <- c(1,1,1,1,1,1,0,1,0,1,1)
p21 <- c(1,1,1,1,1,1,0,1,0,1,1)
p22 <- c(1,1,1,1,1,1,0,1,0,1,1)
p23 <- c(1,1,1,1,1,1,0,1,0,1,1)
p24 <- c(1,1,1,1,1,1,0,1,0,1,1)
p25 <- c(1,1,1,1,1,1,0,1,0,1,1)
p26 <- c(1,1,1,1,1,1,0,1,0,1,1)
p27 <- c(1,1,1,1,1,1,0,1,0,1,1)
p28 <- c(1,1,1,1,1,1,0,1,0,1,1)
#
v <- all23[,1:11]; ## EXTRACT VOTES
v[v==0] <- NA; v[v==-1] <- 0    ## Versión probit requiere 0s y 1s con NAs
v <- t(v)                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
J <- nrow(v); I <- ncol(v)      ## SESSION TOTALS
##
ife.data <- list ("J", "I", "v", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25", "d26", "d27", "d28", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14", "p15", "p16", "p17", "p18", "p19", "p20", "p21", "p22", "p23", "p24", "p25", "p26", "p27", "p28")
#ife.data <- list ("J", "I", "T", "v", "dp")
ife.inits <- function (){
    list (
    x1=rnorm(J),
    x2=rnorm(J),
    x3=rnorm(J),
    x4=rnorm(J),
    x5=rnorm(J),
    x6=rnorm(J),
    x7=rnorm(J),
    x8=rnorm(J),
    x9=rnorm(J),
    x10=rnorm(J),
    x11=rnorm(J),
    x12=rnorm(J),
    x13=rnorm(J),
    x14=rnorm(J),
    x15=rnorm(J),
    x16=rnorm(J),
    x17=rnorm(J),
    x18=rnorm(J),
    x19=rnorm(J),
    x20=rnorm(J),
    x21=rnorm(J),
    x22=rnorm(J),
    x23=rnorm(J),
    x24=rnorm(J),
    x25=rnorm(J),
    x26=rnorm(J),
    x27=rnorm(J),
    x28=rnorm(J),
    signal=rnorm(I),
    difficulty=rnorm(I)
    )
    }
## ife.inits <- function (){
##     list (
##     x=rnorm(J*T),
##     signal=rnorm(I),
##     difficulty=rnorm(I)
##     )
##     }
ife.parameters <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "signal", "difficulty")

#test ride to see program works
start.time <- proc.time()
results <- jags (data=ife.data, inits=ife.inits, ife.parameters,
                 model.file=model1Dj.irt, n.chains=2,
                 n.iter=100, n.thin=10
                 )
time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)

## multicore call
start.time <- proc.time()
results <- mclapply(1:2, function(x) {
   model.jags.re <- try(jags (data=ife.data, inits=ife.inits, ife.parameters,
   model.file=model1Dj.irt, n.chains=1,
   n.iter=100, n.burnin=50, n.thin=1
#   n.iter=20000, n.burnin=10000, n.thin=100
   ))
   if(inherits(model.jags.re,"try-error")) {return()}
   return(model.jags.re)
   }, mc.cores=2 )
time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)

#longer run
start.time <- proc.time()
results <- jags (data=ife.data, inits=ife.inits, ife.parameters,
                 model.file=model1Dj.irt, n.chains=2,
                n.iter=30000, n.burnin=20000, n.thin=100
                 )
time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)


xxxxxx viejo

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
j2 <- jotas[,,2]    #Barragán
j3 <- jotas[,,3]    #Cantú
j4 <- jotas[,,4]    #Cárdenas
j5 <- jotas[,,5]    #Lujambio
j6 <- jotas[,,6]    #Merino
j7 <- jotas[,,7]    #Molinar
j8 <- jotas[,,8]    #Peschard
j9 <- jotas[,,9]    #Zebadúa
j10 <- jotas[,,10]  #Rivera
j11 <- jotas[,,11]  #Luken

## item.midpoints <- matrix(NA, ncol=3, nrow=I)
## for (i in 1:I){
##     item.midpoints[i,1] <- quantile (m[,i], 0.10, names=F)
##     item.midpoints[i,2] <- quantile (m[,i], 0.50, names=F)
##     item.midpoints[i,3] <- quantile (m[,i], 0.90, names=F)
##               }
## tmp <- t(m)
## sem <- d1+d2*2+d3*3+d4*4+d5*5+d6*6+d7*7+d8*8+d9*9+d10*10+d11*11+d12*12+d13*13+d14*14
## sem.midpoints <- matrix(NA, ncol=3, nrow=T)
## for (i in 1:T){
##     sem.midpoints[i,1] <- quantile (tmp[sem==i,], 0.10, names=F)
##     sem.midpoints[i,2] <- quantile (tmp[sem==i,], 0.50, names=F)
##     sem.midpoints[i,3] <- quantile (tmp[sem==i,], 0.90, names=F)
##               }



##################################################
###     UGALDE-VALDES A LA MARTIN-QUINN        ###
###            oct-2003--oct-2012              ###
##################################################

### model for 18 members and 18 semesters (UGALDE y VALDES juntos) extremist anchors # # # # # #
model17Dynj <- function() {
  for (j in 1:J){                  ## loop over councilors
    for (i in 1:I){                ## loop over items
      v[j,i] ~ dbern(p[j,i]);       ## voting rule
      probit(p[j,i]) <- mu[j,i];    ## sets 0<p<1 as function of mu
      sumprod1[j,i] <- x1[j]*d1[i]*p1[j] + x2[j]*d2[i]*p2[j] + x3[j]*d3[i]*p3[j] + x4[j]*d4[i]*p4[j]
                     + x5[j]*d5[i]*p5[j] + x6[j]*d6[i]*p6[j] + x7[j]*d7[i]*p7[j] + x8[j]*d8[i]*p8[j]
                     + x9[j]*d9[i]*p9[j] + x10[j]*d10[i]*p10[j];
      sumprod2[j,i] <- x11[j]*d11[i]*p11[j] + x12[j]*d12[i]*p12[j] + x13[j]*d13[i]*p13[j] + x14[j]*d14[i]*p14[j]
                     + x15[j]*d15[i]*p15[j] + x16[j]*d16[i]*p16[j] + x17[j]*d17[i]*p17[j] + x18[j]*d18[i]*p18[j];
      sumprod3[j,i] <- x19[j]*d19[i]*p19[j] + x20[j]*d20[i]*p20[j] + x21[j]*d11[i]*p11[j] + x22[j]*d12[i]*p12[j]
                     + x23[j]*d13[i]*p13[j] + x24[j]*d14[i]*p14[j] + x25[j]*d15[i]*p15[j] + x26[j]*d16[i]*p16[j];
      sumprod4[j,i] <- x27[j]*d17[i]*p17[j] + x28[j]*d18[i]*p18[j] + x29[j]*d19[i]*p19[j] + x30[j]*d30[i]*p30[j]
                     + x31[j]*d11[i]*p11[j] + x32[j]*d12[i]*p12[j] + x33[j]*d13[i]*p13[j] + x34[j]*d14[i]*p14[j]
                     + x35[j]*d15[i]*p15[j] + x36[j]*d16[i]*p16[j];
      mu[j,i] <- signal[i]*(  sumprod1[j,i] + sumprod2[j,i] + sumprod3[j,i] + sumprod4[j,i] ) - difficulty[i];         ## utility differential
                }
      x1[j] ~ dnorm (x0[j],50);
      x2[j] ~ dnorm (x1[j],50);
      x3[j] ~ dnorm (x2[j],50);
      x4[j] ~ dnorm (x3[j],50);
      x5[j] ~ dnorm (x4[j],50);
      x6[j] ~ dnorm (x5[j],50);
      x7[j] ~ dnorm (x6[j],50);
      x8[j] ~ dnorm (x7[j],50);
      x9[j] ~ dnorm (x8[j],50);
      x10[j] ~ dnorm (x9[j],50);
      x11[j] ~ dnorm (x10[j],50);
      x12[j] ~ dnorm (x11[j],50);
      x13[j] ~ dnorm (x12[j],50);
      x14[j] ~ dnorm (x13[j],50);
      x15[j] ~ dnorm (x14[j],50);
      x16[j] ~ dnorm (x15[j],50);
      x17[j] ~ dnorm (x16[j],50);
      x18[j] ~ dnorm (x17[j],50);
      x19[j] ~ dnorm (x18[j],50);
      x20[j] ~ dnorm (x19[j],50);
      x21[j] ~ dnorm (x20[j],50);
      x22[j] ~ dnorm (x21[j],50);
      x23[j] ~ dnorm (x22[j],50);
      x24[j] ~ dnorm (x23[j],50);
      x25[j] ~ dnorm (x24[j],50);
      x26[j] ~ dnorm (x25[j],50);
      x27[j] ~ dnorm (x26[j],50);
      x28[j] ~ dnorm (x27[j],50);
      x29[j] ~ dnorm (x28[j],50);
      x30[j] ~ dnorm (x29[j],50);
      x31[j] ~ dnorm (x30[j],50);
      x32[j] ~ dnorm (x31[j],50);
      x33[j] ~ dnorm (x32[j],50);
      x34[j] ~ dnorm (x33[j],50);
      x35[j] ~ dnorm (x34[j],50);
      x36[j] ~ dnorm (x35[j],50);
              }
#  for (i in 1:I){
#     m[i] <- difficulty[i] / signal[i]                                      ## cutpoint
#  }
  ## priors for 18 counsilors
    x0[1] ~ dnorm(1, 4);    #Ugalde
    x0[2] ~ dnorm(0, 1);    #Albo
    x0[3] ~ dnorm(0, 1);    #Andrade
    x0[4] ~ dnorm(2, 4);    #Alcántar
    x0[5] ~ dnorm(-2, 4);   #Glez. Luna
    x0[6] ~ dnorm(0, 4);    #Latapi
    x0[7] ~ dnorm(0, 1);    #Lopez Flores
    x0[8] ~ dnorm(2, 4);    #Morales
    x0[9] ~ dnorm(-2, 4);   #Sánchez
    x0[10] ~ dnorm(0, 1);   #Valdés
    x0[11] ~ dnorm(2, 4);   #Baños
    x0[12] ~ dnorm(0, 1);   #Nacif
    x0[13] ~ dnorm(0, 1);   #Elizondo
    x0[14] ~ dnorm(-2, 4);  #Figueroa
    x0[15] ~ dnorm(0, 1);   #Guerrero
    x0[16] ~ dnorm(0, 1);   #Marván
    x0[17] ~ dnorm(-2, 4);  #Córdova
    x0[18] ~ dnorm(0, 1);   #García Ramírez
    for(i in 1:I){
        signal[i] ~ dnorm( 0, 0.1);
        difficulty[i] ~ dnorm( 0, 0.25);
                 }
}
## END MODEL

#########################################
###     all456789 UGALDE-VALDES       ###
#########################################

time <- all456789$t
## SEMESTRALES
d1 <-  ifelse(time==1,1,0)
d2 <-  ifelse(time==2,1,0)
d3 <-  ifelse(time==3,1,0)
d4 <-  ifelse(time==4,1,0)
d5 <-  ifelse(time==5,1,0)
d6 <-  ifelse(time==6,1,0)
d7 <-  ifelse(time==7,1,0)
d8 <-  ifelse(time==8,1,0)
d9 <-  ifelse(time==9,1,0)
d10 <- ifelse(time==10,1,0)
d11 <- ifelse(time==11,1,0)
d12 <- ifelse(time==12,1,0)
d13 <- ifelse(time==13,1,0)
d14 <- ifelse(time==14,1,0)
d15 <- ifelse(time==15,1,0)
d16 <- ifelse(time==16,1,0)
d17 <- ifelse(time==17,1,0)
d18 <- ifelse(time==18,1,0)
d19 <- ifelse(time==19,1,0)
d20 <- ifelse(time==20,1,0)
d21 <- ifelse(time==21,1,0)
d22 <- ifelse(time==22,1,0)
d23 <- ifelse(time==23,1,0)
d24 <- ifelse(time==24,1,0)
d25 <- ifelse(time==25,1,0)
d26 <- ifelse(time==26,1,0)
d27 <- ifelse(time==27,1,0)
d28 <- ifelse(time==28,1,0)
d29 <- ifelse(time==29,1,0)
d30 <- ifelse(time==30,1,0)
d31 <- ifelse(time==31,1,0)
d32 <- ifelse(time==32,1,0)
d33 <- ifelse(time==33,1,0)
d34 <- ifelse(time==34,1,0)
d35 <- ifelse(time==35,1,0)
d36 <- ifelse(time==36,1,0)

## ## VERSIÓN QUE MODIFICA LAS FRONTERAS SEMESTRALES PARA QUE
## ## CONSEJEROS SALIENTES Y ENTRANTES NO ESTÉN JUNTOS
## #        u  a  a  a  g  l  l  m  s  v  b  n  e  f  g
## #        g  l  n  l  l  a  p  o  a  a  a  a  l  i  u
## #        a  b  d  c  z  t  z  r  n  l  ñ  c  i  g  e
## p1 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0) # presente o ausente en el semestre
## p2 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
## p3 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
## p4 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
## p5 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
## p6 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
## p7 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
## p8 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
## p9 <-  c(0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0)
## p10 <- c(0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0)
## p11 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
## p12 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
## p13 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
## p14 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
## p15 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
## p16 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
## p17 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
## p18 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)

## ## VERSIÓN QUE NO MODIFICA LOS SEMESTRES, CONSEJEROS
## ## SALIENTES Y ENTRANTES PUEDEN ESTAR JUNTOS EN UN SEMESTRE
## #        u  a  a  a  g  l  l  m  s  v  b  n  e  f  g  m  c  g
## #        g  l  n  l  l  a  p  o  a  a  a  a  l  i  u  a  o  a
## #        a  b  d  c  z  t  z  r  n  l  ñ  c  i  g  e  r  r  r
## p1 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0) # presente o ausente en el semestre
## p2 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## p3 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## p4 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## p5 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## p6 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## p7 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## p8 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
## p9 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
## p10 <- c(0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
## p11 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
## p12 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
## p13 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
## p14 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
## p15 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
## p16 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
## p17 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
## p18 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)

## VERSIÓN QUE NO MODIFICA LOS SEMESTRES, CONSEJEROS
## SALIENTES Y ENTRANTES PUEDEN ESTAR JUNTOS EN UN SEMESTRE
#        u  a  a  a  g  l  l  m  s  v  b  n  e  f  g  m  c  g
#        g  l  n  l  l  a  p  o  a  a  a  a  l  i  u  a  o  a
#        a  b  d  c  z  t  z  r  n  l  ñ  c  i  g  e  r  r  r
p1 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0) # presente o ausente en el semestre
p2 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p3 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p4 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p5 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p6 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p7 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p8 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p9 <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p13 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p14 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p15 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p16 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p17 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
p18 <- c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
p19 <- c(0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
p20 <- c(0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p21 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p22 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p23 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p24 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p25 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p26 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p27 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p28 <- c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p29 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p30 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p31 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p32 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
p33 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
p34 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
p35 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
p36 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)

v <- all456789[,1:18]
#v[v==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION COMO VOTO NAY
v[v==0] <- NA; v[v==-1] <- 0                    ## Versión probit requiere 0s y 1s
v <- t(v)                                       ## ROLL CALLS NEED ITEMS IN COLUMNS, LEGISLATORS IN ROWS
J <- nrow(v); I <- ncol(v); T <- max(time)      ## SESSION TOTALS


#ife.data <- list ("J", "I", "v", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13", "d14", "d15", "d16", "d17", "d18", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14", "p15", "p16", "p17", "p18")
ife.data <- list ("J", "I", "v", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25", "d26", "d27", "d28", "d29", "d30", "d31", "d32", "d33", "d34", "d35", "d36", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14", "p15", "p16", "p17", "p18", "p19", "p20", "p21", "p22", "p23", "p24", "p25", "p26", "p27", "p28", "p29", "p30", "p31", "p32", "p33", "p34", "p35", "p36")
ife.inits <- function (){
    list (
    x1=rnorm(J),
    x2=rnorm(J),
    x3=rnorm(J),
    x4=rnorm(J),
    x5=rnorm(J),
    x6=rnorm(J),
    x7=rnorm(J),
    x8=rnorm(J),
    x9=rnorm(J),
    x10=rnorm(J),
    x11=rnorm(J),
    x12=rnorm(J),
    x13=rnorm(J),
    x14=rnorm(J),
    x15=rnorm(J),
    x16=rnorm(J),
    x17=rnorm(J),
    x18=rnorm(J),
    x19=rnorm(J),
    x20=rnorm(J),
    x21=rnorm(J),
    x22=rnorm(J),
    x23=rnorm(J),
    x24=rnorm(J),
    x25=rnorm(J),
    x26=rnorm(J),
    x27=rnorm(J),
    x28=rnorm(J),
    x29=rnorm(J),
    x30=rnorm(J),
    x31=rnorm(J),
    x32=rnorm(J),
    x33=rnorm(J),
    x34=rnorm(J),
    x35=rnorm(J),
    x36=rnorm(J),
    signal=rnorm(I),
    difficulty=rnorm(I)
    )
    }
ife.parameters <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31", "x32", "x33", "x34", "x35", "x36", "signal", "difficulty")

#test ride to see program works
start.time <- proc.time()
results <- jags (data=ife.data, inits=ife.inits, ife.parameters,
                 model.file=model17Dynj, n.chains=1,
                 n.iter=100, n.thin=10
#                 ,bugs.directory="c:/Program Files (x86)/WinBUGS14/", debug=TRUE
                 )
time.elapsed <- round(((proc.time()-start.time)[3])/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"minutes","\n")); rm(time.elapsed)

## multicore call
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



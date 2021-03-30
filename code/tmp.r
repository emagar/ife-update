###################
## Vectorization ##
###################
sel <- which(vot$dunan==1)
vot <- vot[-sel,] # drop uncontested votes

J <- length(name) # How many councilors in period chosen

library (runjags)
library (coda)
rc <- as.data.frame(t(vs)) # as.data.frame(t(vot[,1:J]))
leg.index  <- 1:nrow(rc)
vote.index <- 1:ncol(rc)

## Melt RC
rc.2 <- as.data.frame (rc)
colnames (rc.2) <- vote.index
rc.2$leg <- leg.index
molten.rc <- reshape2::melt(rc.2, id.vars="leg", variable.name="vote", value.name="rc")
molten.rc$rc <- car::recode (molten.rc$rc, "0=NA")
molten.rc <- na.omit (molten.rc)
molten.rc$rc <- car::recode (molten.rc$rc, "2=0; c(3,4,5)=NA")

cjr.data.vector <- dump.format(list(y=molten.rc$rc
                                    , n.legs=max(leg.index)
                                    , n.item=max(vote.index)
                                    , n.obs=nrow(molten.rc)
                                    , vote=molten.rc$vote
                                    , dep=molten.rc$leg
))

cjr.parameters = c("theta", "alpha", "beta", "deviance")

cjr.inits <- function() {
  dump.format(
    list(
#      theta = c(NA, NA, rnorm(max(leg.index)-2))
      theta = c(rnorm(3), NA, rnorm(4), NA, rnorm(max(leg.index)-9))
      , alpha = rnorm(max(vote.index))
      , beta = rnorm(max(vote.index))
      ,'.RNG.name'="base::Wichmann-Hill"
      ,'.RNG.seed'= 1971)   #randomNumbers(n = 1, min = 1, max = 1e+04,col=1))
  )
}

## MODEL
cjr.vector <- function(){
	for (i in 1:n.obs) {
		y[i] ~ dbern (pi[i])
		probit(pi[i]) <- beta[vote[i]]*theta[dep[i]] - alpha[vote[i]]
	}
        # PRIORS
        for (j in 1:n.item){ alpha[j] ~ dnorm(0, 0.25) }   
        # Beta (discrimination, dimension 1)
        for (j in 1:n.item){ beta[j] ~ dnorm(0, 0.1) }   
        # ideal points
        north <- which(column=="alcantar")
        south <- which(column=="sanchez")
        for(i in setdiff(c(north, south), 1:n.item)){
            theta[i] ~ dnorm( 0,1)
        }
        theta[north] ~ dnorm( 1,4)T(0,) # normal + truncada
        theta[south] ~ dnorm(-1,4)T(,0) # normal - truncada
        ## for(i in 1:3)        { theta[i] ~ dnorm(0,1) }
        ## theta[4] ~ dnorm( 1,4)T(0,) # normal + truncada
        ## for(i in 5:8)        { theta[i] ~ dnorm(0,1) }
        ## theta[9] ~ dnorm(-1,4)T(,0) # normal - truncada
        ## for(i in 10:n.legs)  { theta[i] ~ dnorm(0,1) }
}

cjr.model.v <- run.jags(
  model=cjr.vector,
  monitor=cjr.parameters,
  method="parallel",
  n.chains=2,
#  n.chains=1,
  data=cjr.data.vector,
  inits=list (cjr.inits(), cjr.inits()),
  thin=50, burnin=10000, sample=200,
#  thin=5,  burnin=200,   sample=3,
  check.conv=FALSE, plots=FALSE
)

chainsCJR.v <- mcmc.list(list (cjr.model.v$mcmc[[1]], cjr.model.v$mcmc[[2]]))
gelman.diag (chainsCJR.v, multivariate=F) # convergence looks fine for both models

Alpha.v <- rbind ( chainsCJR.v[[1]][,grep("alpha", colnames(chainsCJR.v[[1]]))]
                   , chainsCJR.v[[2]][,grep("alpha", colnames(chainsCJR.v[[2]]))])
Beta.v <- rbind ( chainsCJR.v[[1]][,grep("beta", colnames(chainsCJR.v[[1]]))]
                  , chainsCJR.v[[2]][,grep("beta", colnames(chainsCJR.v[[2]]))])
Theta.v <- rbind ( chainsCJR.v[[1]][,grep("theta", colnames(chainsCJR.v[[1]]))]
                   , chainsCJR.v[[2]][,grep("theta", colnames(chainsCJR.v[[2]]))])

plot (colMeans (Alpha.v))  # difficulties
plot (colMeans (Beta.v))   # signal

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

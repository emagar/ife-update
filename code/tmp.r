
###########
## MODEL ##
###########
## alpha is vote's   difficulty
## beta  is vote's   signal strength
## theta is member's ideal point
ife.vector <- function(){
    for (i in 1:n.obs) {
        y[i] ~ dbern (pi[i])
        probit(pi[i]) <- beta[vote[i]]*theta[member[i]] - alpha[vote[i]]
    }
    # PRIORS
    for (j in 1:n.item){
        alpha[j] ~ dnorm(0, 0.25);   
        beta [j] ~ dnorm(0, 0.1)
    }   
    # IDEAL POINTS
    north <- which(column=="alcantar")
    south <- which(column=="sanchez")
    theta[north] ~ dnorm( 1,4)T(0,) # normal + truncada
    theta[south] ~ dnorm(-1,4)T(,0) # normal - truncada
    for(i in setdiff(c(north, south), 1:n.item)){
        theta[i] ~ dnorm( 0,1)
    }
}

###################
## Vectorization ##
###################
member.index  <- 1:nrow(v)
vote.index <- 1:ncol(v)

## Melt RC
rc <- as.data.frame (v)
colnames (rc) <- vote.index
rc$member <- member.index
molten.rc <- reshape2::melt(rc, id.vars="member", variable.name="vote", value.name="rc")
#molten.rc$rc <- car::recode (molten.rc$rc, "0=NA")
molten.rc <- na.omit (molten.rc)
#molten.rc$rc <- car::recode (molten.rc$rc, "2=0; c(3,4,5)=NA")

ife.data.vector <- dump.format(list(
    y         = molten.rc$rc
  , n.members = max(member.index)
  , n.item    = max(vote.index)
  , n.obs     = nrow(molten.rc)
  , vote      = molten.rc$vote
  , member    = molten.rc$member
  , north     = north
  , south     = south
))

ife.parameters = c("theta", "alpha", "beta", "deviance")

ife.inits <- function() {
  dump.format(
    list(
        theta = rnorm(max(member.index))
#      theta = c(rnorm(3), NA, rnorm(4), NA, rnorm(max(member.index)-9)) # NAs sólo en caso de spike priors, correcto?
      , alpha = rnorm(max(vote.index))
      , beta = rnorm(max(vote.index))
      ,'.RNG.name'="base::Wichmann-Hill"
      ,'.RNG.seed'= 1971)   #randomNumbers(n = 1, min = 1, max = 1e+04,col=1))
  )
}

ife.model.v <- run.jags(
  model   = ife.vector,
  monitor = ife.parameters,
  method  = "parallel",
  data    = ife.data.vector,
  inits   = list (ife.inits(), ife.inits()),
  n.chains=2, thin=50, burnin=10000, sample=200,
#  n.chains=1, thin=5,  burnin=200,   sample=3,
  check.conv=FALSE, plots=FALSE
)

chainsIFE.v <- mcmc.list(list (ife.model.v$mcmc[[1]], ife.model.v$mcmc[[2]])) 
gelman.diag (chainsIFE.v, multivariate=F)

Alpha.v <- rbind ( chainsIFE.v[[1]][,grep("alpha", colnames(chainsIFE.v[[1]]))]
                   , chainsIFE.v[[2]][,grep("alpha", colnames(chainsIFE.v[[2]]))])
Beta.v <- rbind ( chainsIFE.v[[1]][,grep("beta", colnames(chainsIFE.v[[1]]))]
                  , chainsIFE.v[[2]][,grep("beta", colnames(chainsIFE.v[[2]]))])
Theta.v <- rbind ( chainsIFE.v[[1]][,grep("theta", colnames(chainsIFE.v[[1]]))]
                   , chainsIFE.v[[2]][,grep("theta", colnames(chainsIFE.v[[2]]))])

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

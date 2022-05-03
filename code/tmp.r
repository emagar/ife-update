
tmp.loc  <- rep (NA, J)
tmp.pre <- rep (100, J)
#locs <- apply( rbind (results[[1]]$BUGSoutput$sims.list$x, results[[2]]$BUGSoutput$sims.list$x), 2, median)
#partyPlacement <- apply( rbind (results[[1]]$BUGSoutput$sims.list$partyPos, results[[2]]$BUGSoutput$sims.list$partyPos), 2, median)
locs <- apply( results$BUGSoutput$sims.list$theta, 2, median)
partyPlacement <- apply( results$BUGSoutput$sims.list$party.theta, 2, median)
for (n in 1:J){
    if (length( which(councilors==name[n]) )==0) {            # if councilor not member current round
        tmp.loc[n] <- NA                                      # then prior for next round set to NA
        tmp.pre[n] <- NA                                      # (and line above sets it to party placement)
    }
    else { tmp.loc[n] <-  locs[which (councilors==name[n])] } # councilor's prior for next round is current x 
}
# Precision prior is always constant at 100, implying standard deviation = sqrt (1/100) = 0.1



                                        # IDEAL POINTS ---  


sel.trunc <- function(i){
        if (!is.na(north) & i==north){
            draw ~ dnorm(x.mean[i], x.tau[i])T(,0)

                               for(i 1:n.members)){
     theta[i] ~ sel.trunc (x.mean[i], x.tau[i]); # auto-regressive process
}


        setdiff(1:9, c(NA,NA))

            theta[north] ~ ifelse (is.na(north)==FALSE, dnorm(x.mean[north], x.tau[north])T(0,) # normal + truncada
    ifelse (is.na(south)==FALSE) theta[south] ~ dnorm(x.mean[south], x.tau[south])T(,0) # normal - truncada

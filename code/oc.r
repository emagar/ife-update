## ONE-DIM ARRANGEMENT
## AGREEMENT MATRIX --- LA GUARDE PORQUE ESTO TARDA A?OS
load("agreeMatrix.Rdata")
#votes <- rc
#votes[votes==0] <- -1  # los 0s se vuelven -1s # DEJA ABSTENCION == NAY
#I <- dim(votes)[1]; J <- dim(votes)[2]
#agreeMatrix <- matrix(NA, ncol=J, nrow=J); tmp <- rep(NA, times=I)
#for (j in 1:J){
#    agreeMatrix[j,j] <- 1  ## DIAGONAL
#              }
#for (j1 in 2:J){
#    for (j2 in (j1-1):1){
#        for (i in 1:I){
#            tmp[i] <- ifelse(votes[i,j1]==votes[i,j2], 1, 0)
#                      }
#        agreeMatrix[j2,j1] <- sum(tmp)/I; agreeMatrix[j1,j2] <- agreeMatrix[j2,j1]
#        print( paste("j1 =",j1,"; j2 =",j2) )
#                        }
#               }
## SQUARED DISTANCES
sd <- (1-agreeMatrix)^2
## DOUBLE-CENTRED MATRIX
pmean <- rep(NA, times=J); mmat <- mean(sd); dc <- sd
for (j in 1:J){
    pmean[j] <- mean(sd[j,])
              }
for (r in 1:J){
    for (c in 1:J){
        dc[r,c] <- (sd[r,c] - pmean[r] - pmean[c] + mmat)/-2
                  }
              }
## SIMPLE ONE-DIM IDEAL POINTS
tmp <- sqrt(dc[1,1])
ip  <- c(tmp, dc[2:J,1]/tmp)
##
## EXTREMA DERECHA
thr <- .14
data.frame(ip=ip[c(1:J)[ip>thr]], id=dipdat$id[c(1:J)[ip>thr]], nom=dipdat$nom[c(1:J)[ip>thr]], part=dipdat$part[c(1:J)[ip>thr]], noVote=dipdat$noVoteRate[c(1:J)[ip>thr]])
##EXTREMA IZQUIERDA
thr <- -.185
data.frame(ip=ip[c(1:J)[ip<thr]], id=dipdat$id[c(1:J)[ip<thr]], nom=dipdat$nom[c(1:J)[ip<thr]], part=dipdat$part[c(1:J)[ip<thr]], noVote=dipdat$noVoteRate[c(1:J)[ip< thr]])
##
plot(c(-.3,.3), c(1,7), type="n")
for (j in 1:J){
    points(ip[j], dipdat$part[j], pch=20,col=dipdat$color[j])
    
}


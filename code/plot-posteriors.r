

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

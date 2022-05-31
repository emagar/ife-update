Este es un mixture model que pesqué del internet:

mixmodel = function() {
    for( i in 1 : N ) {
        y[i] ˜ dnorm(mu[i], tau)
        mu[i] <- lambda[T[i]]
        T[i] ˜ dcat(pi[]) }
    pi[1:2] ˜ ddirch(alpha[])
    theta ˜ dnorm(0.0, 1.0E-6)%_%I(0.0, )
    lambda[1] ˜ dnorm(0.0, 1.0E-6)
    lambda[2] <- lambda[1] + theta
    tau ˜ dgamma(0.001,0.001)
    sigma <- 1 / sqrt(tau)
}

Este mi intento de traducirlo al IFE:

ife.model.items.mix = "model {
	for (n in 1:n.obs) {
		y[n] ~ dbern (pi[n])
		probit(pi[n]) <- beta[vote[n]]*theta[contingent[member[n]]] - alpha[vote[n]]
		contingent[n] ~ dcat(P[])
	}
        # three party contingents
        P[1:3] ~ ddirch(a[])
	# PRIORS
	# Alpha (difficulty)
	for (j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25) }   
	# Beta (discrimination) --- votes sorted such that 1-2 are north-south, rest uninformative
        for(j in 3:n.item){ 
            beta [j] ~ dnorm(0, 0.1)
        }
        beta [1] ~ dnorm( 4, 4)
        beta [2] ~ dnorm(-4, 4)
	# ideal points
	for(i in 1:n.member) { theta[i] ~ dnorm(0,1) }
	
	skip[1:2] ~ dnorm(0, 1.0E-6)T(0,)
	lambda[1] ~ dnorm(0.0, 1.0E-6)
	lambda[2] <- lambda[1] + skip[1]
	lambda[3] <- lambda[2] + skip[2]
}"

nota: creo que a[] tiene que ser un vector de tres 1s en los datos (límite superior de categorías)


###########





# contested votes histogram at bottom
vot$date <- ymd(vot$date)
# aggregate weekly split votes
tmp <- data.frame(
    dt = vot$date,
    wk = floor_date(vot$date, "weeks"))
tmp <- tmp[order(tmp$wk),]
tmp$n <- 0
tmp$n <- ave(tmp$n, as.factor(tmp$wk), FUN=length, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$wk)==FALSE,]
# to list
tmp1 <- lapply(1:nrow(tmp), function(x){
    res <- list(xx = c(tmp$wk[x], tmp$wk[x]),
                yy = c(0, tmp$n[x]))
    return(res)
})
#
plot(c(min(vot$date), max(vot$date)),
     c(0, max(tmp$n)),
     type="n", xlab = "week", ylab = "freq", axes = FALSE,
     main = "Weekly contested votes")
axis(1, at = c(min(vot$date), max(vot$date)), labels = FALSE)
axis(1, at = terms.dates$mid, labels = c("Ugalde\n2003-08", "Valdés I\n2008", "Valdés II\n2008-10", "Valdés III\n2010-11", "Valdés IV\n2011-13", "Valdés V\n2013"), padj = .25)
axis(2)
#
lapply(tmp1, function(x){lines(x$xx,x$yy, col = "gray")}) # draws transparent confidence bands



lines()

i <- 2
polygon(tmp1[[i]]$xx,tmp1[[i]]$yy, border=tmp1[[i]]$color)
polygon(tmp1[[1]]$xx,tmp1[[1]]$yy, col="#000000", border=tmp1[[1]]$color)
polygon(tmp1[[1]]$xx,tmp1[[1]]$yy)

for (i in 1:nrow(point.est)){
    lines(x = terms.dates$mid,
          y = point.est[i,])
}


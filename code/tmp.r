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

sel.r <- ids$column[which(!is.na(point.est[,1]))]
text(x = 1,
     y = point.est[sel.r, 1],
     labels = ids[sel.r, "short"],
     pos = 2 )
sel.r <- ids$column[which(!is.na(point.est[,5]))]
text(x = 5,
     y = point.est[sel.r, 5],
     labels = ids[sel.r, "short"],
     pos = 4 )
#dev.off()

#################################
## plot with time-scale X axis ##
#################################
sel <- which(terms.dates$term %in% 4:10)
terms.dates <- terms.dates[sel,] # subset terms.dates
plot(c(min(terms.dates$start), max(terms.dates$end)), c(min(lo, na.rm = TRUE), max(hi, na.rm = TRUE)), type="n", xlab = "term", ylab = "ideal point", axes = FALSE,
     main = "Static estimates by term, item-identified")
axis(1, at = c(min(terms.dates$start), max(terms.dates$end)), labels = FALSE)
axis(1, at = terms.dates$mid, labels = c("Ugalde\n2003-08", "Valdés I\n2008", "Valdés II\n2008-10", "Valdés III\n2010-11", "Valdés IV\n2011-13", "Valdés V\n2013"), padj = .25)
axis(2)

for (t in 1:T){
    #t <- 1
    sel <- c("4","6","7","8","9","a","b"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t  <- ids$party [sel]
    column.t <- ids$column[sel]
    color.t <- ids$color[sel]
    color50.t <- ids$color50[sel]
    tmp <- point.est[column.t, t]
    points(x = rep(terms.dates$mid[t], length(tmp)),
           y = tmp,
           col = color.t)
    tmp <- data.frame(y1 = lo[column.t, t],
                      y2 = lo[column.t, t],
                      y3 = hi[column.t, t],
                      y4 = hi[column.t, t])
    tmp$x1 <- terms.dates$start[t]
    tmp$x2 <- terms.dates$end[t]
    tmp$x3 <- terms.dates$end[t]
    tmp$x4 <- terms.dates$start[t]
    tmp$color <- color50.t
    tmp$who <- column.t
    # turn into list
    tmp1 <- lapply(1:length(sel), function(x){
        res <- list(xx = tmp[x, grep("^x", colnames(tmp))],
                    yy = tmp[x, grep("^y", colnames(tmp))],
                    color = tmp$color[x],
                    who = tmp$who[x])
        return(res)
    })
    ## tmp1 <- vector(mode = "list", length = nrow(ids))
    ## tmp1[sel] <- lapply(1:length(sel), function(x){
    ##     res <- list(xx = tmp[x, grep("^x", colnames(tmp))],
    ##                 yy = tmp[x, grep("^y", colnames(tmp))])
    ##     return(res)
    ## })
    lapply(tmp1, function(x){polygon(x$xx,x$yy, col=x$color, border=x$color)})
}

i <- 2
polygon(tmp1[[i]]$xx,tmp1[[i]]$yy, border=tmp1[[i]]$color)
polygon(tmp1[[1]]$xx,tmp1[[1]]$yy, col="#000000", border=tmp1[[1]]$color)
polygon(tmp1[[1]]$xx,tmp1[[1]]$yy)

for (i in 1:nrow(point.est)){
    lines(x = terms.dates$mid,
          y = point.est[i,])
}

for (t in 1:T){
    sel <- c("4","6","7","8","9","a","b"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t  <- ids$party [sel]
    column.t <- ids$column[sel]
    color.t <- ids$color[sel]
    color50.t <- ids$color50[sel]
    tmp <- point.est[column.t, t]
    points(x = rep(terms.dates$mid[t], length(tmp)),
           y = tmp,
           col = color.t[t])
}
    

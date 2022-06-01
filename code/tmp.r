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


#####################################
## plot terms 2:3 point estimates ##
#####################################
#pdf(file = "../plots/statics-terms-2-3-item.pdf", width = 10, height = 7)
plot(c(.25,T+.75), c(min(point.est, na.rm = TRUE), max(point.est, na.rm = TRUE)), type="n", xlab = "term", ylab = "ideal point", axes = FALSE,
     main = "Static estimates by term, item-identified") 
axis(1, at = 1:T, labels = c("Woldenberg I\n1996-2000", "Woldenberg II\n2000-2003"), padj = .25)
axis(2)
for (t in 1:T){
    sel <- c("2","3"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t   <- ids$party [sel]
    column.t  <- ids$column[sel]
    color.t   <- ids$color[sel]
    color50.t <- ids$color50[sel]
    tmp <- point.est[column.t, t]
    points(x = rep(t, length(tmp)),
           y = tmp,
           col = color.t)
}
for (i in 1:nrow(point.est)){
    lines(x = 1:T,
          y = point.est[i,],
          col = ids$color[i])
}
sel.r <- ids$column[which(!is.na(point.est[,1]))]
text(x = 1,
     y = point.est[sel.r, 1],
     labels = ids[sel.r, "short"],
     pos = 2 )
sel.r <- ids$column[which(is.na(point.est[,1]))]
text(x = 2,
     y = point.est[sel.r, 2],
     labels = ids[sel.r, "short"],
     pos = 4 )
#dev.off()

#################################################################
## plot terms 2:3 with time-scale X axis and confidence bands ##
#################################################################
#pdf(file = "../plots/statics-terms-2-3-item-time-scale.pdf", width = 10, height = 7)
sel <- which(terms.dates$term %in% 2:3)
terms.dates <- terms.dates[sel,] # subset terms.dates
# set plot
plot(c(min(terms.dates$start), max(terms.dates$end)+100),
     c(min(lo, na.rm = TRUE)-.5, max(hi, na.rm = TRUE)+.15), type="n", xlab = "Year", ylab = "Ideal point", axes = FALSE,
     main = "Static estimates by term, item-identified")
#axis(1, at = c(min(terms.dates$start), max(terms.dates$end)), labels = FALSE)
axis(1, at = seq(from = ymd("19960101"), to = ymd("20220101"), by = "year"), labels = FALSE)
axis(1, at = seq(from = ymd("19970701"), to = ymd("20030701"), by = "year"), tick = FALSE, labels = 1997:2003)
#axis(1, at = terms.dates$mid, labels = c("Ugalde\n2003-08", "Valdés I\n2008", "Valdés II\n2008-10", "Valdés III\n2010-11", "Valdés IV\n2011-13", "Valdés V\n2013"), padj = .25)
axis(2)
# lines connecting point estimates
for (i in 1:nrow(point.est)){
    #i <- 1
    lines(x = terms.dates$mid,
          y = point.est[i,],
          col = ids$color[i])
}
# add point estimates and confidence bands
for (t in 1:T){
    #t <- 2
    sel <- c("2","3"); sel <- sel[t]; sel <- grep(pattern = sel, ids$tenure)
    party.t  <- ids$party [sel]
    column.t <- ids$column[sel]
    color.t <- ids$color[sel]
    color50.t <- ids$color50[sel]
    tmp <- point.est[column.t, t]
    points(x = rep(terms.dates$mid[t], length(tmp)),
           y = tmp,
           col = "white", pch = 19) # makes points non-transparent
    points(x = rep(terms.dates$mid[t], length(tmp)),
           y = tmp,
           col = color.t)           # draws point estimates
    # prepares data for conf bands
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
    lapply(tmp1, function(x){polygon(x$xx,x$yy, col=x$color, border=NA)}) # draws transparent confidence bands
    if (t==1) abline(v = tmp1[[1]]$xx$x1, lty = 3)
    abline(v = tmp1[[1]]$xx$x2, lty = 3)
}
# add member names
sel.r <- ids$column[which(!is.na(point.est[,1]))]
text(x = terms.dates$mid[1],
     y = point.est[sel.r, 1],
     labels = ids[sel.r, "short"],
     cex = .9, 
     pos = c(4,2,2,2,4,2,2,2,2) )
sel.r <- ids$column[which(is.na(point.est[,1]))]
text(x = terms.dates$mid[2],
     y = point.est[sel.r, 2],
     labels = ids[sel.r, "short"],
     pos = 4 )
#
# add term labels on top
text(x = terms.dates$mid,
     y = max(hi, na.rm = TRUE)+.25,
     labels = c("Woldenberg I", "Woldenberg II"), cex = .8)
#
# add elections
text(x=ymd(c("19970702", "20000702", "20030701")),
     y=min(lo, na.rm = TRUE)-.63 ,
     labels="*")
#
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
# change scale
tmp$n <- (tmp$n/(2*max(tmp$n))) + (min(lo, na.rm = TRUE)-.58)
# to list
tmp1 <- lapply(1:nrow(tmp), function(x){
    res <- list(xx = c(tmp$wk[x], tmp$wk[x]),
                yy = c((min(lo, na.rm = TRUE)-.58), tmp$n[x]))
    return(res)
})
#
lapply(tmp1, function(x){lines(x$xx,x$yy, col = "gray")}) # draws transparent confidence bands
#dev.off()

## Since the "oc" package was removed from CRAN on 2017-09-11,
## need to install from earlier snapshot using "checkpoint"
install.packages("checkpoint")
library(checkpoint)
dir.create("~/.checkpoint", showWarnings=FALSE) # for batch mode
checkpoint("2017-08-01")
install.packages("oc")
library(oc)

library(Rcpp)
library(RcppEigen)
install.packages("npideal", repos="https://r.tahk.us/")
## To install from source:
## devtools::install_github("atahk/npideal", ref="v0.1.1")
library(npideal)

library(pscl)
library(xtable)

sim.data <- function(prob.fun, ideal.rank, n) {
    p1 <- t(replicate(n, pxf(ideal.rank)))
    ## polarity <- sapply(p1[,ncol(p1)]-p1[,1],sign)
    ## mean.prob <- apply(p1*polarity, 2, mean)
    v1 <- array(rbinom(length(p1),1,p1),dim=dim(p1))*2-1
    ## attr(v1, "mean.prob") <- mean.prob
    return(v1)
}

pxf.sym <- function(ideal.rank, sample.polarity=TRUE)
    pnorm(ideal.rank, sample(ideal.rank, 1), 2,
          lower.tail = sample(c(!sample.polarity, TRUE), 1))

pxf.asym <- function(ideal.rank, sample.polarity=TRUE)
    punif(.5^(ideal.rank), lower.tail=sample(c(!sample.polarity,TRUE),1))

num.sim <- 10000
k <- 9
ideal.rank <- 1:k

if (file.exists("mc-1a.RData")) {
    load("mc-1a.RData")
} else {
    num.votes <- 100
    mean.prob <- est.sim.oc <- est.sim.new <- matrix(NA,k,num.sim)
    pxf <- pxf.sym
    for (i in 1:num.sim) {
        v <- sim.data(pxf, ideal.rank, num.votes)
        est.sim.new[,i] <- voteest(v)
        v.rc <- rollcall(t(v),yea=1,nay=-1,missing=0)
        sink("/dev/null")
        oc1 <- oc(v.rc, dims=1, polarity=9)
        sink()
        est.sim.oc[,i] <- oc1$legislators[,1]
    }    
    true.prob <- apply(replicate(num.sim, pxf(ideal.rank, sample.polarity=FALSE)),1,mean)
    mse.new <- mean(apply((est.sim.new-(ideal.rank))^2,2,sum))
    mse.oc <- mean(apply((est.sim.oc-(ideal.rank))^2,2,sum))
    mean.est.new <- apply(est.sim.new,1,mean)
    mean.est.oc <- apply(est.sim.oc,1,mean)
    library("xtable")
    out <- cbind("True prob. of cons. vote"=true.prob, "True rank-order"=ideal.rank, "New estimator (mean est.)"=mean.est.new, "OC (Mean est.)"=mean.est.oc)
    rownames(out) <- paste("Legislator", 1:k)
    out <- rbind(out,"Mean Squared Error"=c(NA,NA,mse.new,mse.oc))
    save.image("mc-1a.RData", compress=TRUE)
}
print(xtable(out, digits=c(2,2,0,2,2), caption="Estimate means for uniform bill distributions"), booktabs=TRUE, hline.after = c(-1,0,k,k+1), file="table-sim-sym.tex")

if (file.exists("mc-1b.RData")) {
    load("mc-1b.RData")
} else {
    num.votes <- 100
    mean.prob <- est.sim.oc <- est.sim.new <- matrix(NA,k,num.sim)
    pxf <- pxf.asym
    for (i in 1:num.sim) {
        v <- sim.data(pxf, ideal.rank, num.votes)
        est.sim.new[,i] <- voteest(v)
        v.rc <- rollcall(t(v),yea=1,nay=-1,missing=0)
        sink("/dev/null")
        oc1 <- oc(v.rc, dims=1, polarity=9)
        sink()
        est.sim.oc[,i] <- oc1$legislators[,1]
    }    
    true.prob <- apply(replicate(num.sim, pxf(ideal.rank, sample.polarity=FALSE)),1,mean)
    mse.new <- mean(apply((est.sim.new-(ideal.rank))^2,2,sum))
    mse.oc <- mean(apply((est.sim.oc-(ideal.rank))^2,2,sum))
    mean.est.new <- apply(est.sim.new,1,mean)
    mean.est.oc <- apply(est.sim.oc,1,mean)
    library("xtable")
    out <- cbind("True prob. of cons. vote"=true.prob, "True rank-order"=ideal.rank, "New estimator (mean est.)"=mean.est.new, "OC (Mean est.)"=mean.est.oc)
    rownames(out) <- paste("Legislator", 1:k)
    out <- rbind(out,"Mean Squared Error"=c(NA,NA,mse.new,mse.oc))
    save.image("mc-1b.RData", compress=TRUE)
}
print(xtable(out, digits=c(2,3,0,2,2), caption="Estimate means for lopsided bill distributions"), booktabs=TRUE, hline.after = c(-1,0,k,k+1), file="table-sim-asym.tex")


if (file.exists("mc-1c.RData")) {
    load("mc-1c.RData")
} else {
    num.votes.vec <- c(50,100,500,1000)
    mse.new <- mse.oc <- NULL
    pxf <- pxf.sym
    for (num.votes in num.votes.vec) {
        est.sim.oc <- est.sim.new <- matrix(NA,k,num.sim)
        for (i in 1:num.sim) {
            v <- sim.data(pxf, ideal.rank, num.votes)
            est.sim.new[,i] <- voteest(v)
            v.rc <- rollcall(t(v),yea=1,nay=-1,missing=0)
            sink("/dev/null")
            oc1 <- oc(v.rc, dims=1, polarity=9)
            sink()
            est.sim.oc[,i] <- oc1$legislators[,1]
        }
        mse.new <- c(mse.new, mean(apply((est.sim.new-(ideal.rank))^2,2,sum)))
        mse.oc <- c(mse.oc, mean(apply((est.sim.oc-(ideal.rank))^2,2,sum)))
    }

    mse.new.asym <- mse.oc.asym <- NULL
    pxf <- pxf.asym
    for (num.votes in num.votes.vec) {
        est.sim.oc <- est.sim.new <- matrix(NA,k,num.sim)
        for (i in 1:num.sim) {
            v <- sim.data(pxf, ideal.rank, num.votes)
            est.sim.new[,i] <- voteest(v)
            v.rc <- rollcall(t(v),yea=1,nay=-1,missing=0)
            sink("/dev/null")
            oc1 <- oc(v.rc, dims=1, polarity=9)
            sink()
            est.sim.oc[,i] <- oc1$legislators[,1]
        }
        mse.new.asym <- c(mse.new.asym, mean(apply((est.sim.new-(ideal.rank))^2,2,sum)))
        mse.oc.asym <- c(mse.oc.asym, mean(apply((est.sim.oc-(ideal.rank))^2,2,sum)))
    }

    out <- data.frame("Num. votes"=num.votes.vec,
                      "New est."=mse.new, "OC"=mse.oc,
                      "New est."=mse.new.asym, "OC"=mse.oc.asym,
                      check.names=FALSE)
    save.image("mc-1c.RData", compress=TRUE)
}

print(xtable(out, digits=c(0,0,2,2,2,2)), include.rownames=FALSE, file="table-sim-mse.tex", booktabs=TRUE)

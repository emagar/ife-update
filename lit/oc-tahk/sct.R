library(Rcpp)
library(RcppEigen)
## install.packages("npideal", repos="https://r.tahk.us/")
## ## To install from source:
## ## devtools::install_github("atahk/npideal", ref="v0.1.1")
library(npideal)

library(pscl)     # simon jackman's package
library(xtable)
library(parallel)

load("sct-all.RData")
sct.all$issueAreaFull <-
    factor(sct.all$issueArea,
           levels=1:14,
           labels=c("Criminal Procedure",
               "Civil Rights",
               "First Amendment",
               "Due Process",
               "Privacy",
               "Attorneys",
               "Unions",
               "Economic Activity",
               "Judicial Power",
               "Federalism",
               "Interstate Relations",
               "Federal Taxation",
               "Miscellaneous",
               "Private Action"))
sct.all$issueArea <- factor(c("Criminal Procedure"="Criminal Procedure",
               "Civil Rights"="Civil Liberties",
               "First Amendment"="Civil Liberties",
               "Due Process"="Civil Liberties",
               "Privacy"="Civil Liberties",
               "Attorneys"="Other",
               "Unions"="Economics",
               "Economic Activity"="Economics",
               "Judicial Power"="Other",
               "Federalism"="Other",
               "Interstate Relations"="Other",
               "Federal Taxation"="Economics",
               "Miscellaneous"="Other",
               "Private Action"="Other")[sct.all$issueAreaFull])

split.year <- function(split.year, ls, bs, natural.ct=NULL, focus=NULL, exclude=NULL, issue=NULL, split.issue=list(), window=5) {
    if (!is.null(natural.ct))
        sct.all.sub <- subset(sct.all, naturalCourt%in%natural.ct)
    else
        sct.all.sub <- sct.all
    if (length(split.year) == 0)
        split.year <- c(1945,2016)
    if (length(split.year)==1)
        {
            test.years <- c(split.year-window,split.year,split.year+window)
            test.years <- c(1900,test.years,2020)
            sitting.justices <-
                intersect(intersect(unique(sct.all.sub$justice[sct.all.sub$term==test.years[2]]),
                                    unique(sct.all.sub$justice[sct.all.sub$term==test.years[3]])),
                          unique(sct.all.sub$justice[sct.all.sub$term==test.years[4]]))
        }
    else
        {
            split.year <- range(split.year)
            jc <- unique(sct.all.sub[(sct.all.sub$term >= split.year[1]) & (sct.all.sub$term <= split.year[2]),c("justice","justiceName","term")])
            jct <- table(jc$justice)
            sitting.justices <- as.numeric(names(jct)[jct > (diff(split.year)+1)/2])
            ## sitting.justices <- unique(sct.all.sub$justice[(sct.all.sub$term >= split.year[1]) & (sct.all.sub$term <= split.year[2])])
            test.years <- c(split.year[1],split.year,split.year[2],split.year[2])
        }
    jlist <- unique(sct.all.sub[,c("justice","justiceName")])
    sitting.justice.names <-
        jlist[match(sitting.justices, jlist[,1]),2]
    sitting.justices <- sitting.justices[!(sitting.justice.names %in% exclude)]
    sitting.justice.names <- sitting.justice.names[!(sitting.justice.names %in% exclude)]
    f <- match(focus, sitting.justice.names)
    sct.data <- subset(sct.all.sub, justice %in% sitting.justices,
                       select=c("majority","justiceName","caseId"))
    sct.desc <- subset(sct.all.sub, justice %in% sitting.justices,
                       select=c("caseId","issue","issueArea","term"))
    out <- reshape(sct.data, direction="wide",
                   v.names="majority", idvar="caseId",
                   timevar="justiceName")
    sct.caseid <- out$caseId
    sct.vote <- 2*out[,grep("^majority[.]", colnames(out))] - 3
    rm(out)
    colnames(sct.vote) <-
        gsub("^majority[.][A-Z][A-Z]?([A-Z])", "\\1", colnames(sct.vote))
    colnames(sct.vote) <-
        gsub("^([A-Z])([A-Z])", "\\1'\\2", colnames(sct.vote))
    sct.vote <- as.matrix(sct.vote)
    sct.vote[is.na(sct.vote)] <- 0
    sct.desc <- sct.desc[!duplicated(sct.desc),]
    sct.desc <- sct.desc[match(sct.caseid, sct.desc$caseId),]
    ## sct.desc$issueArea <-
    ##     factor(sct.desc$issueArea,
    ##            levels=1:14,
    ##            labels=c("Criminal Procedure",
    ##                "Civil Rights",
    ##                "First Amendment",
    ##                "Due Process",
    ##                "Privacy",
    ##                "Attorneys",
    ##                "Unions",
    ##                "Economic Activity",
    ##                "Judicial Power",
    ##                "Federalism",
    ##                "Interstate Relations",
    ##                "Federal Taxation",
    ##                "Miscellaneous",
    ##                "Private Action"))
    table(sct.desc$issueArea)
    if (length(split.year) == 1) {
        choice.sel.A <- sct.desc$term < test.years[3]
        choice.sel.B <- sct.desc$term >= test.years[3]
    } else if (length(split.issue) > 0) {
        choice.sel.A <- sct.desc$issueArea %in% split.issue[[1]]
        if (length(split.issue) > 1)
            choice.sel.B <- sct.desc$issueArea %in% split.issue[[2]]
        else
            choice.sel.B <- !choice.sel.A
    }
    if (!is.null(issue)) {
        choice.sel.A <- choice.sel.A & (sct.desc$issueArea %in% issue)
        choice.sel.B <- choice.sel.B & (sct.desc$issueArea %in% issue)
    }
    choice.sel.A <- choice.sel.A & (sct.desc$term >= test.years[1]) & (sct.desc$term <= test.years[5])
    choice.sel.B <- choice.sel.B & (sct.desc$term >= test.years[1]) & (sct.desc$term <= test.years[5])
    s <- list(v1=sct.vote[choice.sel.A,],
              v2=sct.vote[choice.sel.B,],
              j=sitting.justice.names,
              f=f,
              i=table(sct.desc$issueArea[choice.sel.A | choice.sel.B])
              )
    ## return(s)
    s2 <- votetest(s$v1,s$v2,s$f-1,ls,bs-1)
    s2$i <- table(sct.desc$issueArea[choice.sel.A | choice.sel.B])
    s2$f <- f
    s2$j <- sitting.justice.names
    s2$e1 <- voteest(s$v1)
    s2$e2 <- voteest(s$v2)
    s2$en1 <- sitting.justice.names[order(s2$e1)]
    s2$en2 <- sitting.justice.names[order(s2$e2)]
    s2$n1 <- dim(s$v1)
    s2$n2 <- dim(s$v2)
    return(s2)
}

pv1 <- 
    split.year(1975, 3000, 1000)$p
pv2 <- 
    split.year(1977, 3000, 1000, focus=c("TMarshall","WEBurger","HABlackmun"))$p
pv3 <- 
    split.year(1977, 3000, 1000, exclude="HABlackmun")$p

pvs <- 
list(blackmunpone=pv1,
     blackmunptwo=pv2,
     blackmunpthree=pv3)

siminfo <- "blackmunp.tex"
cat("", file=siminfo)
for (i in names(pvs)) {
    px <- 4
    if (pvs[[i]] < 10^(-px))
        fout <- sprintf(paste0("p < %0.",px,"f"),10^(-px))
    else if (pvs[[i]] < .01)
        fout <- sprintf(paste0("p = %0.",min(px,1+ceiling(-log10(pvs[[i]]))),"f"),pvs[[i]])
    else
        fout <- sprintf("p = %0.2f",pvs[[i]])
    cat("\\newcommand\\",i,"{",fout,"}\n",sep="",file=siminfo,append=TRUE)
}





decades <- lapply(seq(1950,2000,10),function(x) x:(x+9))

if (file.exists("sct-decade.RData")) {
    load("sct-decade.RData")
} else {
    comparison.sets <- apply(combn(c("Criminal Procedure","Civil Liberties","Economics"),2),2,as.list)
    models <- list()
    p.vals <- NULL
    for (comparison.set in comparison.sets) {
        models[[length(models)+1]] <-
            mclapply(decades, split.year, ls=10000, bs=1000,
                     split.issue=comparison.set)
        p.vals <- cbind(p.vals,
                        sapply(models[[length(models)]], function(x) x$p))
    }

    comparison.set <- list(c("Criminal Procedure","Civil Liberties"),c("Economics"))
    comparison.sets[[length(comparison.sets)+1]] <- comparison.set

    models[[length(models)+1]] <-
        mclapply(decades, split.year, ls=10000, bs=1000,
                 split.issue=comparison.set)
    p.vals <- cbind(p.vals,
                    sapply(models[[length(models)]], function(x) x$p))
    save(comparison.sets, decades, models, p.vals, file="sct-decade.RData", compress=TRUE)
}

out <- t(p.vals)
colnames(out) <- paste0(sapply(decades, min),"s")
rownames(out) <- 
    sapply(comparison.sets, function(x) tolower(paste(sapply(x,paste,collapse=" & "),collapse=" v. ")))
rownames(out)[4] <- gsub("([Cc]rim)inal ([Pp]roc)edure","\\1. \\2.",rownames(out)[4])
rownames(out)[4] <- gsub("([Cc]iv)il ([Ll]ib)erties","\\1. \\2.",rownames(out)[4])
out <- 
data.frame(Comparison=rownames(out),
           out, check.names=FALSE)
out <- out[c(4,2,3,1),]
print(xtable(out, caption="Tests of the hypothesis that the ideal points of Supreme Court justices are in the same order on cases on two different issue areas", label="tab:pval-comparisons}{\\smallskip", digits=3), booktabs=TRUE, caption.placement="top", include.rownames=FALSE, table.placement=NULL, file="sct-pvals.tex",floating=FALSE)

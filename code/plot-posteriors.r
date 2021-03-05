
# workdir
rm(list = ls())
workdir <- c("/home/eric/Dropbox/data/rollcall/ife_cg/ife-update/data/")
setwd(workdir)


# load posterior sims
load(file = "posterior-samples/wold23-window-results-compress.RData") # woldenberg
load(file = "posterior-samples/ugal45-window-results-compress.RData") # ugalde 4-5
load(file = "posterior-samples/ugal4567-window-results-compress.RData") # ugalde 4-7
ls()
x

# determine which posterior draws to plot
window.results <- window.results.23
window.results <- window.results.45
window.results <- window.results.4567

ideal.points <- as.data.frame(matrix (NA, nrow=S, ncol=J))
colnames(ideal.points) <- ids$column[1:J]
name <- ids$name[1:J]
column <- ids$column[1:J]
color <- ids$color[1:J]
for (i in 1:S){
#	i <- 290 # debug
	sel <- which(name %in% window.results[[i]]$councilors) # select columns with member councilors in window sessions
#	ideal.points[i,sel] <- apply (rbind (window.results[[i]][[1]]$BUGSoutput$sims.list$x, window.results[[i]][[2]]$BUGSoutput$sims.list$x), 2, median) # merge both chains, report median
	ideal.points[i,sel] <- apply (window.results[[i]]$BUGSoutput$sims.list$x, 2, median) # merge both chains, report median
}


#colors <- c("red","black","orange","blue","gold","green","gray","brown","purple")


tit <- "Woldenberg, Bonica method"
fil <- "../plots/wold23-bonica.pdf"
tit <- "Ugalde-Valdés 456, Bonica method"
fil <- "../plots/ugal456-bonica.pdf"
#
pdf(file = fil)
plot(c(1:S), ideal.points[1:S,1], main=tit, ylim=c(-4,4), type="n", xlab="Vote window", ylab="Ideal points")
#abline (h=summary (EstVarCases)[[1]][,1], lty=3, lwd=2, col=colors)
for (j in 1:J){
	sel.s <- which(!is.na(IsCouncilor[,j])) # drops NAs that spline rejects
	lines(smooth.spline(sel.s, ideal.points[sel.s,j], df=10), lwd=3, col=color[j])
}
dev.off()



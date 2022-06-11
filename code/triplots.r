############################################################################
## Function transforming ternary compositional into cartesian coordinates ##
## https://stackoverflow.com/questions/11623602/shaded-triplot-in-r       ##
############################################################################
tern2cart <- function(coord){
    coord[1] -> x
    coord[2] -> y
    coord[3] -> z
    x+y+z -> tot
    x/tot -> x
    y/tot -> y
    z/tot -> z
    (2*y + z)/(2*(x+y+z)) -> x1
    sqrt(3)*z/(2*(x+y+z)) -> y1
    return(c(x=x1,y=y1))
    }
# function to add vertex labels
add.lab <- function(labels, add.sign=TRUE){
    left  <- labels[2];
    right <- labels[1];
    up    <- labels[3];
    text(0, 0, labels=left, pos=1)         # left
    text(1, 0, labels=right, pos=1)        # right
    text(0.5, sqrt(3)/2, labels=up, pos=3) # up
    if (add.sign==TRUE) text(0.5,0, labels="@emagar", pos=1, cex=.75, col="gray")
}

###############################
## function wrapping triplot ##
###############################
la.ternera <- function(datos, color = rgb(.55,.27,.07, alpha = .2), cex.pts = .15, main = NA, labs=c("","",""), left.right.up=c("one","two","three"), add.sign=TRUE){
    # Prepare data: re-arrange so three is lower right, two lower left, one is above 
    #datos <- c(one=.2,two=.38,three=.42) tmp[,1] #debug
    #left.right.up <- c("two","three","one") #debug
    datos <- datos[left.right.up] # subset and sort according to left.right.up
    #Then transformed into cartesian coordinates:
    # datos <- t(apply(datos,2,tern2cart))
    datos <- t(tern2cart(datos))
    # Draw the empty ternary diagram:
    par(mar=c(2.1, 2.1, 4.1, 2.1)) ## SETS B L U R MARGIN SIZES
    plot(NA, NA, xlim=c(0,1), ylim=c(0,sqrt(3)/2), asp=1, bty="n", axes=F, xlab="", ylab="", main = main)
    segments(0,0,0.5,sqrt(3)/2)
    segments(0.5,sqrt(3)/2,1,0)
    segments(1,0,0,0)
    # add vertex labels
    add.lab(labs, add.sign=add.sign)
    ## # add a grid:
    ## a <- seq(0.9,0.1,by=-0.1)
    ## b <- rep(0,9)
    ## c <- seq(0.1,0.9,by=0.1)
    ## grid <- data.frame(x=c(a, b, c, a, c, b),y=c(b, c, a, c, b, a),z=c(c, a, b, b, a, c))
    ## t(apply(grid,1,tern2cart)) -> grid.tern
    ## cbind(grid.tern[1:27,],grid.tern[28:54,]) -> grid
    ## apply(grid,1,function(x){segments(x0=x[1],y0=x[2],x1=x[3],y1=x[4],lty=2,col="grey80")})
    ## # axis labels
    ## paste(seq(10,90,by=10),"%")->lab
    ## text(grid.tern[9:1,],paste(lab,"\n(PAN)"),col="grey80",cex=0.7, pos=2)
    ## text(grid.tern[18:10,],paste(lab,"\n(PRI)"),col="grey80",cex=0.7, pos=4)
    ## text(grid.tern[27:19,],paste(lab,"\n(Morena)"),col="grey80",cex=0.7, pos=1)
    # or 50-50 to 33-33-33 lines instead
    a <- c(1,1,0)
    b <- c(1,0,1)
    c <- c(0,1,1)
    d <- c(1,1,1)
    grid <- data.frame(matrix(c(a,b,c,d),nrow=4,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:3){
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[4,1],y1=grid.tern[4,2],lty=2,col="grey10")
    }
    ## # or 10 percent bands
    ## a <- c( 57.5,42.5,   0)
    ## b <- c( 57.5,   0,42.5)
    ## c <- c(130/3,85/3,85/3)
    ## grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    ## grid.tern <- t(apply(grid,1,tern2cart))
    ## for (i in 1:2){
    ##     #i <- 1 # debug
    ##     segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey10")
    ## }
    ## a <- c(42.5, 57.5,   0)
    ## b <- c(   0, 57.5,42.5)
    ## c <- c(85/3,130/3,85/3)
    ## grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    ## grid.tern <- t(apply(grid,1,tern2cart))
    ## for (i in 1:2){
    ##     #i <- 1 # debug
    ##     segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey10")
    ## }
    ## a <- c(42.5,   0, 57.5)
    ## b <- c(   0,42.5, 57.5)
    ## c <- c(85/3,85/3,130/3)
    ## grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    ## grid.tern <- t(apply(grid,1,tern2cart))
    ## for (i in 1:2){
    ##     #i <- 1 # debug
    ##     segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey10")
    ## }
    # Plot points:
    points(datos, pch = 20, cex = cex.pts, col = color)
}

##################
## party colors ##
##################
col.pan <-    rgb(.18,.24,.73, alpha = .2) # moderate blue
col.vxm <-    rgb(  0,.75,  1, alpha = .2) # deepskyblue
col.pri <-    rgb(.89,.17,.17, alpha = .2) # brightred
col.morena <- rgb(.55,.27,.07, alpha = .2) # saddlebrown
col.prd <-    rgb(  1,.84,  0, alpha = .2) # gold
col.pvem   <- rgb(  0,.80,  0, alpha = .2) # green 3
col.mc     <- rgb(.93,.46,  0, alpha = .2) # darkorange2
col.oth    <- rgb(.55,.55,.55, alpha = .2) # gray 59
#col.oth    <- rgb(.94, .5, .5, alpha = .2) # lightcoral
#19/255

########################################################
## ################################################## ##
## ## Load data or use elec-data-for-maps for plot ## ##
## ################################################## ##
########################################################

####################
## ############## ##
## ## triplots ## ##
## ############## ##
####################



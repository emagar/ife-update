# working dir
setwd("~/Dropbox/data/rollcall/ife_cg/ife-update/")

# read data
d <- read.csv(file = "data/base_ife_eric_feb2021.csv", stringsAsFactors = FALSE)
str(d)

# save full vote set
sel.r <- which(d$term %in% c(2:14))
sel.c <- c(
    c("woldenberg", "barragan", "cantu", "cardenas", "lujambio", "merino", "molinar", "peschard", "zebadua", "rivera", "luken"), 
    c("ugalde", "albo", "andrade", "alcantar", "glezluna", "latapi", "lopezflores", "morales", "sanchez", "valdes", "banos", "nacif", "elizondo", "figueroa", "guerrero", "cordova", "garcia", "marvan"),
    c("cordova", "banos", "andrade2", "favela", "galindo", "murayama", "nacif", "ruiz", "sanchez", "santiago", "snmartin", "ravel", "rivera2", "zavala"),
    info.cols
)
sel.c <- sel.c[duplicated(sel.c)==FALSE] # drop duplicates
tmp <- d[sel.r, sel.c]
write.csv(tmp, file = "data/v23456789abcde.csv", row.names = FALSE)

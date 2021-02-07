# working dir
setwd("~/Dropbox/data/rollcall/ife_cg/ife-update/")

# some libraries
library(lubridate) # easier dates

# read data
d <- read.csv(file = "data/base_ife_eric_feb2021.csv", stringsAsFactors = FALSE)
str(d)

# fix logroll w vote at end not start
d$folio [d$folio==2299] <- 99999
d$folio [d$folio==2315] <- 2299
d$folio[d$folio==99999] <- 2315
# sort
d <- d[order(d$folio),]
# fix another logroll
d$dlogroll[d$folio==2623] <- 0
d$nconjunto[d$folio>=2503 & d$folio<=2514] <- 1

# ayes
d$tmp00 <- 0
d$tmp01 <- 0
d$tmp02 <- 0
d$tmp03 <- 0
d$tmp04 <- 0
d$tmp05 <- 0
d$tmp06 <- 0
d$tmp07 <- 0
d$tmp08 <- 0
d$tmp09 <- 0
d$tmp10 <- 0
d$tmp11 <- 0
d$tmp12 <- 0
d$tmp13 <- 0
d$tmp14 <- 0
d$tmp15 <- 0
d$tmp16 <- 0
d$tmp17 <- 0
d$tmp18 <- 0
d$tmp19 <- 0
d$tmp20 <- 0
d$tmp21 <- 0
d$tmp22 <- 0
d$tmp23 <- 0
d$tmp24 <- 0
d$tmp25 <- 0
d$tmp26 <- 0
d$tmp27 <- 0
d$tmp28 <- 0
d$tmp29 <- 0
d$tmp30 <- 0
d$tmp31 <- 0
d$tmp32 <- 0
d$tmp33 <- 0
d$tmp34 <- 0
d$tmp35 <- 0
d$tmp36 <- 0
d$tmp37 <- 0
d$tmp38 <- 0

d$tmp00[d$merino==1] <- 1
d$tmp01[d$woldenberg==1] <- 1
d$tmp02[d$barragan==1] <- 1
d$tmp03[d$cantu==1] <- 1
d$tmp04[d$cardenas==1] <- 1
d$tmp05[d$lujambio==1] <- 1
d$tmp06[d$molinar==1] <- 1
d$tmp07[d$peschard==1] <- 1
d$tmp08[d$zebadua==1] <- 1

d$tmp09[d$rivera==1] <- 1
d$tmp10[d$luken==1] <- 1

d$tmp11[d$ugalde==1] <- 1
d$tmp12[d$albo==1] <- 1
d$tmp13[d$andrade==1] <- 1
d$tmp14[d$alcantar==1] <- 1
d$tmp15[d$glezluna==1] <- 1
d$tmp16[d$latapi==1] <- 1
d$tmp17[d$lopezflores==1] <- 1
d$tmp18[d$morales==1] <- 1
d$tmp19[d$sanchez==1] <- 1

d$tmp20[d$segob==1] <- 1
d$tmp21[d$senpri==1] <- 1
d$tmp22[d$senprd==1] <- 1
d$tmp23[d$dippri==1] <- 1
d$tmp24[d$dippan==1] <- 1
d$tmp25[d$creel==1] <- 1
d$tmp26[d$granados==1] <- 1
d$tmp27[d$zertuche==1] <- 1
d$tmp28[d$pinchetti==1] <- 1
d$tmp29[d$pozas==1] <- 1

d$tmp30[d$valdes==1] <- 1
d$tmp31[d$banos==1] <- 1
d$tmp32[d$nacif==1] <- 1
d$tmp33[d$elizondo==1] <- 1
d$tmp34[d$figueroa==1] <- 1
d$tmp35[d$guerrero==1] <- 1

d$tmp36[d$marvan==1] <- 1
d$tmp37[d$cordova==1] <- 1
d$tmp38[d$garcia==1] <- 1

d <- within(d, ayes <- tmp00+tmp01+tmp02+tmp03+tmp04+tmp05+tmp06+tmp07+tmp08+tmp09+tmp10+tmp11+tmp12+tmp13+tmp14+tmp15+tmp16+tmp17+tmp18+tmp19+tmp20+tmp21+tmp22+tmp23+tmp24+tmp25+tmp26+tmp27+tmp28+tmp29+tmp30+tmp31+tmp32+tmp33+tmp34+tmp35+tmp36+tmp37+tmp38)

# uncount logroll elements
tmp <- c(NA, d$nconjunto) # lag one obs (assumes numconj consecutive)
d$tmp <- tmp[-length(tmp)]
sel <- which(d$dlogroll==1 & d$nconjunto==d$tmp)
d$ayes[sel] <- NA
# drop them
d <- d[-sel,]
# and clean
sel <- grep("^tmp.*", colnames(d))
d <- d[,-sel] # clean

# drops 4 votes (with logrolls) where all 9 abstained
sel <- which(d$folio %in% c(5013, 5024, 5031, 5037, 2503))
d <- d[,-sel] # clean
rm(tmp,sel)

# total votes cast
d <- within(d, {
    segob1 <- as.numeric(segob==1 | segob==2);
    senpri1 <- as.numeric(senpri==1 | senpri==2);
    senprd1 <- as.numeric(senprd==1 | senprd==2);
    dippri1 <- as.numeric(dippri==1 | dippri==2);
    dippan1 <- as.numeric(dippan==1 | dippan==2);
    creel1 <- as.numeric(creel==1 | creel==2);
    granados1 <- as.numeric(granados==1 | granados==2);
    woldenberg1 <- as.numeric(woldenberg==1 | woldenberg==2);
    zertuche1 <- as.numeric(zertuche==1 | zertuche==2);
    pinchetti1 <- as.numeric(pinchetti==1 | pinchetti==2);
    pozas1 <- as.numeric(pozas==1 | pozas==2);
    peschard1 <- as.numeric(peschard==1 | peschard==2);
    lujambio1 <- as.numeric(lujambio==1 | lujambio==2);
    cardenas1 <- as.numeric(cardenas==1 | cardenas==2);
    cantu1 <- as.numeric(cantu==1 | cantu==2);
    merino1 <- as.numeric(merino==1 | merino==2);
    barragan1 <- as.numeric(barragan==1 | barragan==2);
    zebadua1 <- as.numeric(zebadua==1 | zebadua==2);
    molinar1 <- as.numeric(molinar==1 | molinar==2);
    rivera1 <- as.numeric(rivera==1 | rivera==2);
    luken1 <- as.numeric(luken==1 | luken==2);
    ugalde1 <- as.numeric(ugalde==1 | ugalde==2);
    albo1 <- as.numeric(albo==1 | albo==2);
    andrade1 <- as.numeric(andrade==1 | andrade==2);
    alcantar1 <- as.numeric(alcantar==1 | alcantar==2);
    glezluna1 <- as.numeric(glezluna==1 | glezluna==2);
    latapi1 <- as.numeric(latapi==1 | latapi==2);
    lopezflores1 <- as.numeric(lopezflores==1 | lopezflores==2);
    morales1 <- as.numeric(morales==1 | morales==2);
    sanchez1 <- as.numeric(sanchez==1 | sanchez==2);
    valdes1 <- as.numeric(valdes==1 | valdes==2);
    banos1 <- as.numeric(banos==1 | banos==2);
    nacif1 <- as.numeric(nacif==1 | nacif==2);
    elizondo1 <- as.numeric(elizondo==1 | elizondo==2);
    figueroa1 <- as.numeric(figueroa==1 | figueroa==2);
    guerrero1 <- as.numeric(guerrero==1 | guerrero==2);
    marvan1 <- as.numeric(marvan==1 | marvan==2);
    cordova1 <- as.numeric(cordova==1 | cordova==2);
    garcia1 <- as.numeric(garcia==1 | garcia==2);
})
#
d$vtot <- NA
d <- within(d[term==1,], vtot <- segob1+senpri1+senprd1+dippri1+dippan1+creel1+granados1+woldenberg1+zertuche1+pinchetti1+pozas1)
d <- within(d[term==2,], vtot <- woldenberg1+peschard1+lujambio1+cardenas1+cantu1+merino1+barragan1+zebadua1+molinar1)
d <- within(d[term==3,], vtot <- woldenberg1+peschard1+lujambio1+cardenas1+cantu1+merino1+barragan1+rivera1+luken1)
d <- within(d[term==4,], vtot <- ugalde1+albo1+andrade1+alcantar1+glezluna1+latapi1+lopezflores1+morales1+sanchez1)
d <- within(d[term==5,], vtot <- albo1+andrade1+alcantar1+glezluna1+latapi1+lopezflores1+morales1+sanchez1)
d <- within(d[term==6,], vtot <- albo1+andrade1+alcantar1+glezluna1+lopezflores1+sanchez1+valdes1+banos1+nacif1)
d <- within(d[term==7,], vtot <- andrade1+alcantar1+sanchez1+valdes1+banos1+nacif1+elizondo1+figueroa1+guerrero1)
d <- within(d[term==8,], vtot <-                             valdes1+banos1+nacif1+elizondo1+figueroa1+guerrero1)
d <- within(d[term==9,], vtot <- valdes1+banos1+nacif1+elizondo1+figueroa1+guerrero1+marvan1+cordova1+garcia1)
#
d$absten <- NA
gen absten=11-vtot if term==1
replace absten=9-vtot if term>1 & term<5
replace absten=8-vtot if term==5
replace absten=9-vtot if term>5 & term<8
replace absten=6-vtot if term==8
replace absten=9-vtot if term==9
gen nays=vtot-ayes

gen tmp=ayes-nays
gen result2=.
move result2 result
replace result2=1 if tmp>0
replace result2=0 if tmp<=0
drop tmp
drop result
rename result2 result




# recode votes


# summarize contested votes by month
d <- within(d, date <- ymd(yr*10000+mo*100+dy))

with(d[d$dunan==0,], plot(as.factor(year(date)+quarter(date)/10)))





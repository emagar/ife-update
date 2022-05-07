# -*- coding: utf-8 -*-

#########################################################
## script prepares data and exports files for analysis ##
#########################################################

# clean memory
rm(list=ls())

# working dir
setwd("~/Dropbox/data/rollcall/ife_cg/ife-update/")

# some libraries
library(lubridate) # easier dates

# read data
d <- read.csv(file = "data/base_ife_eric_oct1990-may2017.csv", stringsAsFactors = FALSE)
str(d)

# drop endline if any
sel <- which(d$proposer=="endline-drop")
if (length(sel)>0) d <- d[-sel,]
rm(sel)

# fix logroll w vote at end not start
d$folio [d$folio==2299] <- 99999
d$folio [d$folio==2315] <- 2299
d$folio[d$folio==99999] <- 2315
# sort
d <- d[order(d$folio),]
# fix another logroll
d$dlogroll[d$folio==2623] <- 0
d$nconjunto[d$folio>=2503 & d$folio<=2514] <- 1

# turn NAs to vote=0
sel <- which(colnames(d) %in%  c("segob", "senpri", "senprd", "dippri", "dippan", "creel", "granados", "pinchetti", "pozas", "zertuche", "woldenberg", "barragan", "cantu", "cardenas", "lujambio", "merino", "molinar", "peschard", "zebadua", "luken", "rivera", "albo", "alcantar", "andrade", "glezluna", "latapi", "lopezflores", "morales", "sanchez", "ugalde", "banos", "nacif", "valdes", "elizondo", "figueroa", "guerrero", "cordova", "garcia", "marvan", "andrade2", "favela", "galindo", "murayama", "ruiz", "snmartin", "santiago", "ravel", "rivera2", "zavala", "magana", "faz", "humphrey", "kib"))
tmp <- d[,sel]
tmp[is.na(tmp)] <- 0
d[,sel] <- tmp

# compute total ayes
d$tmp00 <- d$tmp01 <- d$tmp02 <- d$tmp03 <- d$tmp04 <- d$tmp05 <- d$tmp06 <- d$tmp07 <- d$tmp08 <- d$tmp09 <- d$tmp10 <- d$tmp11 <- d$tmp12 <- d$tmp13 <- d$tmp14 <- d$tmp15 <- d$tmp16 <- d$tmp17 <- d$tmp18 <- d$tmp19 <- d$tmp20 <- d$tmp21 <- d$tmp22 <- d$tmp23 <- d$tmp24 <- d$tmp25 <- d$tmp26 <- d$tmp27 <- d$tmp28 <- d$tmp29 <- d$tmp30 <- d$tmp31 <- d$tmp32 <- d$tmp33 <- d$tmp34 <- d$tmp35 <- d$tmp36 <- d$tmp37 <- d$tmp38 <- d$tmp39 <- d$tmp40 <- d$tmp41 <- d$tmp42 <- d$tmp43 <- d$tmp44 <- d$tmp45 <- d$tmp46 <- d$tmp47 <- d$tmp48 <- d$tmp49 <- d$tmp50 <- d$tmp51 <- d$tmp52 <- d$tmp53 <- d$tmp54 <- d$tmp55 <- d$tmp56 <- d$tmp57 <- d$tmp58 <- d$tmp59 <- d$tmp60 <- d$tmp61 <- d$tmp62 <- d$tmp63 <- d$tmp64 <- d$tmp65 <- d$tmp66 <- d$tmp67 <- d$tmp68 <- 0
#
d$tmp00[d$segob==1]                  <- 1           
d$tmp01[d$senpri==1]                 <- 1
d$tmp02[d$senprd==1]                 <- 1           
d$tmp03[d$dippri==1]                 <- 1
d$tmp04[d$dippan==1]                 <- 1
d$tmp05[d$cm.alcantara.magos==1]     <- 1
d$tmp06[d$cm.barquin==1]             <- 1
d$tmp07[d$cm.espinosa==1]            <- 1
d$tmp08[d$cm.hdz.e==1]               <- 1
d$tmp09[d$cm.german.perez==1]        <- 1
d$tmp10[d$cm.tirado.ledesma==1]      <- 1
d$tmp11[d$rp.pan.fdez.cevallos==1]   <- 1
d$tmp12[d$rp.pan.alcantara.soria==1] <- 1
d$tmp13[d$rp.pri.c.a.santiago==1]    <- 1
d$tmp14[d$rp.pri.netz.delavega==1]   <- 1
d$tmp15[d$rp.pri.perez.jacome==1]    <- 1
d$tmp16[d$rp.pri.luis.medina==1]     <- 1
d$tmp17[d$rp.pps.lujan.gtz==1]       <- 1
d$tmp18[d$rp.prd.mtz.verdugo==1]     <- 1
d$tmp19[d$rp.pfcrn.pineiro.l==1]     <- 1
d$tmp20[d$rp.parm.kunz==1]           <- 1
#
d$tmp21[d$creel==1]                  <- 1
d$tmp22[d$granados==1]               <- 1
d$tmp23[d$pinchetti==1]              <- 1
d$tmp24[d$pozas==1]                  <- 1
d$tmp25[d$zertuche==1]               <- 1
d$tmp26[d$woldenberg==1]             <- 1
#
d$tmp27[d$barragan==1]               <- 1
d$tmp28[d$cantu==1]                  <- 1
d$tmp29[d$cardenas==1]               <- 1
d$tmp30[d$lujambio==1]               <- 1
d$tmp31[d$merino==1]                 <- 1
d$tmp32[d$molinar==1]                <- 1
d$tmp33[d$peschard==1]               <- 1
d$tmp34[d$zebadua==1]                <- 1
#
d$tmp35[d$luken==1]                  <- 1
d$tmp36[d$rivera==1]                 <- 1
#
d$tmp37[d$albo==1]                   <- 1
d$tmp38[d$alcantar==1]               <- 1
d$tmp39[d$andrade==1]                <- 1
d$tmp40[d$glezluna==1]               <- 1
d$tmp41[d$latapi==1]                 <- 1
d$tmp42[d$lopezflores==1]            <- 1
d$tmp43[d$morales==1]                <- 1
d$tmp44[d$sanchez==1]                <- 1
d$tmp45[d$ugalde==1]                 <- 1
#
d$tmp46[d$banos==1]                  <- 1
d$tmp47[d$nacif==1]                  <- 1
d$tmp48[d$valdes==1]                 <- 1
#
d$tmp49[d$elizondo==1]               <- 1
d$tmp50[d$figueroa==1]               <- 1
d$tmp51[d$guerrero==1]               <- 1
#
d$tmp52[d$cordova==1]                <- 1
d$tmp53[d$garcia==1]                 <- 1
d$tmp54[d$marvan==1]                 <- 1
#
d$tmp55[d$andrade2==1]                  <- 1
d$tmp56[d$favela==1]                    <- 1
d$tmp57[d$galindo==1]                   <- 1
d$tmp58[d$murayama==1]                  <- 1
d$tmp59[d$ruiz==1]                      <- 1
d$tmp60[d$snmartin==1]                  <- 1
d$tmp61[d$santiago==1]                  <- 1
#
d$tmp62[d$ravel==1]                     <- 1
d$tmp63[d$rivera2==1]                   <- 1
d$tmp64[d$zavala==1]                    <- 1
#
d$tmp65[d$magana==1]                    <- 1
d$tmp66[d$faz==1]                       <- 1
d$tmp67[d$humphrey==1]                  <- 1
d$tmp68[d$kib==1]                       <- 1
#
d <- within(d, ayes <- tmp00 + tmp01 + tmp02 + tmp03 + tmp04 + tmp05 + tmp06 + tmp07 + tmp08 + tmp09 + tmp10 + tmp11 + tmp12 + tmp13 + tmp14 + tmp15 + tmp16 + tmp17 + tmp18 + tmp19 + tmp20 + tmp21 + tmp22 + tmp23 + tmp24 + tmp25 + tmp26 + tmp27 + tmp28 + tmp29 + tmp30 + tmp31 + tmp32 + tmp33 + tmp34 + tmp35 + tmp36 + tmp37 + tmp38 + tmp39 + tmp40 + tmp41 + tmp42 + tmp43 + tmp44 + tmp45 + tmp46 + tmp47 + tmp48 + tmp49 + tmp50 + tmp51 + tmp52 + tmp53 + tmp54 + tmp55 + tmp56 + tmp57 + tmp58 + tmp59 + tmp60 + tmp61 + tmp62 + tmp63 + tmp64 + tmp65 + tmp66 + tmp67 + tmp68)
#
# uncount logroll elements
tmp <- c(NA, d$nconjunto) # lag one obs (assumes numconj consecutive)
d$tmp <- tmp[-length(tmp)]
sel <- which(d$dlogroll==1 & d$nconjunto==d$tmp)
d$ayes[sel] <- NA
# and drop them (making uncounting redundant)
d <- d[-sel,]
# clean
sel <- grep("^tmp.*", colnames(d))
d <- d[,-sel] # clean
#
# drops 4 votes (with logrolls) where all 9 abstained
sel <- which(d$folio %in% c(5013, 5024, 5031, 5037, 2503))
d <- d[,-sel] # clean
rm(tmp,sel)

#######################
## drop unused votes ##
#######################
# drop pre-1994
sel <- which(d$term==0)
d <- d[-sel,]
sel <- which(colnames(d) %in% c("cm.alcantara.magos", "cm.barquin", "cm.espinosa", "cm.hdz.e", "cm.german.perez", "cm.tirado.ledesma", "rp.pan.fdez.cevallos", "rp.pan.alcantara.soria", "rp.pri.c.a.santiago", "rp.pri.netz.delavega", "rp.pri.perez.jacome", "rp.pri.luis.medina", "rp.pps.lujan.gtz", "rp.prd.mtz.verdugo", "rp.pfcrn.pineiro.l", "rp.parm.kunz"))
d <- d[,-sel]
rm(sel)
#
# drop carpizo
sel <- which(d$term==1)
d <- d[-sel,]
sel <- which(colnames(d) %in% c("segob", "senpri", "senprd", "dippri", "dippan", "creel", "granados", "pinchetti", "pozas", "zertuche"))
d <- d[,-sel]
rm(sel)
#
# drop recent consejeros without coded votes (last vote may 2017, before they were appointed)
sel <- which(colnames(d) %in%  c("magana", "faz", "humphrey", "kib"))
d <- d[,-sel]

str(d)
colnames(d)

# total votes cast
d <- within(d, {
    ## segob1       <- as.numeric(      segob==1 | segob==2);
    ## senpri1      <- as.numeric(     senpri==1 | senpri==2);
    ## senprd1      <- as.numeric(     senprd==1 | senprd==2);
    ## dippri1      <- as.numeric(     dippri==1 | dippri==2);
    ## dippan1      <- as.numeric(     dippan==1 | dippan==2);
    ## creel1       <- as.numeric(      creel==1 | creel==2);
    ## granados1    <- as.numeric(   granados==1 | granados==2);
    ## pinchetti1   <- as.numeric(  pinchetti==1 | pinchetti==2);
    ## pozas1       <- as.numeric(      pozas==1 | pozas==2);
    ## zertuche1    <- as.numeric(   zertuche==1 | zertuche==2);
    woldenberg1  <- as.numeric( woldenberg==1 | woldenberg==2);
    barragan1    <- as.numeric(   barragan==1 | barragan==2);
    cardenas1    <- as.numeric(   cardenas==1 | cardenas==2);
    cantu1       <- as.numeric(      cantu==1 | cantu==2);
    lujambio1    <- as.numeric(   lujambio==1 | lujambio==2);
    merino1      <- as.numeric(     merino==1 | merino==2);
    molinar1     <- as.numeric(    molinar==1 | molinar==2);
    peschard1    <- as.numeric(   peschard==1 | peschard==2);
    zebadua1     <- as.numeric(    zebadua==1 | zebadua==2);
    luken1       <- as.numeric(      luken==1 | luken==2);
    rivera1      <- as.numeric(     rivera==1 | rivera==2);
    albo1        <- as.numeric(       albo==1 | albo==2);
    andrade1     <- as.numeric(    andrade==1 | andrade==2);
    alcantar1    <- as.numeric(   alcantar==1 | alcantar==2);
    glezluna1    <- as.numeric(   glezluna==1 | glezluna==2);
    latapi1      <- as.numeric(     latapi==1 | latapi==2);
    lopezflores1 <- as.numeric(lopezflores==1 | lopezflores==2);
    morales1     <- as.numeric(    morales==1 | morales==2);
    sanchez1     <- as.numeric(    sanchez==1 | sanchez==2);
    ugalde1      <- as.numeric(     ugalde==1 | ugalde==2);
    banos1       <- as.numeric(      banos==1 | banos==2);
    nacif1       <- as.numeric(      nacif==1 | nacif==2);
    valdes1      <- as.numeric(     valdes==1 | valdes==2);
    elizondo1    <- as.numeric(   elizondo==1 | elizondo==2);
    figueroa1    <- as.numeric(   figueroa==1 | figueroa==2);
    guerrero1    <- as.numeric(   guerrero==1 | guerrero==2);
    cordova1     <- as.numeric(    cordova==1 | cordova==2);
    garcia1      <- as.numeric(     garcia==1 | garcia==2);
    marvan1      <- as.numeric(     marvan==1 | marvan==2);
    andrade21    <- as.numeric(   andrade2==1 | andrade2==2);
    favela1      <- as.numeric(     favela==1 | favela==2);
    galindo1     <- as.numeric(    galindo==1 | galindo==2);
    murayama1    <- as.numeric(   murayama==1 | murayama==2);
    ruiz1        <- as.numeric(       ruiz==1 | ruiz==2);
    snmartin1    <- as.numeric(   snmartin==1 | snmartin==2);
    santiago1    <- as.numeric(   santiago==1 | santiago==2);
    ravel1       <- as.numeric(      ravel==1 | ravel==2);
    rivera21     <- as.numeric(    rivera2==1 | rivera2==2);
    zavala1      <- as.numeric(     zavala==1 | zavala==2);
    ## magana1      <- as.numeric(     magana==1 | magana==2);
    ## faz1         <- as.numeric(        faz==1 | faz==2);
    ## humphrey1    <- as.numeric(   humphrey==1 | humphrey==2);
    ## kib1         <- as.numeric(        kib==1 | kib==2);
})
#
d$vtot <- NA
## tmp <- d[d$term==1,]
## tmp <- within(tmp, vtot <- segob1 + senpri1 + senprd1 + dippri1 + dippan1 + creel1 + granados1 + pinchetti1 + pozas1 + woldenberg1 + zertuche1) 
## d[d$term==1,] <- tmp
## #
tmp <- d[d$term==2,]
tmp <- within(tmp, vtot <- woldenberg1 + peschard1 + lujambio1 + cardenas1 + cantu1 + merino1 + barragan1 + zebadua1 + molinar1)
d[d$term==2,] <- tmp
#
tmp <- d[d$term==3,]
tmp <- within(tmp, vtot <- woldenberg1 + peschard1 + lujambio1 + cardenas1 + cantu1 + merino1 + barragan1 + rivera1 + luken1)
d[d$term==3,] <- tmp
#
tmp <- d[d$term==4,]
tmp <- within(tmp, vtot <- ugalde1 + albo1 + andrade1 + alcantar1 + glezluna1 + latapi1 + lopezflores1 + morales1 + sanchez1)
d[d$term==4,] <- tmp
#
tmp <- d[d$term==5,]
tmp <- within(tmp, vtot <- albo1 + andrade1 + alcantar1 + glezluna1 + latapi1 + lopezflores1 + morales1 + sanchez1)
d[d$term==5,] <- tmp
#
tmp <- d[d$term==6,]
tmp <- within(tmp, vtot <- albo1 + andrade1 + alcantar1 + glezluna1 +         lopezflores1 +          sanchez1 + valdes1 + banos1 + nacif1)
d[d$term==6,] <- tmp
#
tmp <- d[d$term==7,]
tmp <- within(tmp, vtot <-       andrade1 + alcantar1 +                                         sanchez1 + valdes1 + banos1 + nacif1 + elizondo1 + figueroa1 + guerrero1)
d[d$term==7,] <- tmp
#
tmp <- d[d$term==8,]
tmp <- within(tmp, vtot <- valdes1 + banos1 + nacif1 + elizondo1 + figueroa1 + guerrero1)
d[d$term==8,] <- tmp
#
tmp <- d[d$term==9,]
tmp <- within(tmp, vtot <- valdes1 + banos1 + nacif1 + elizondo1 + figueroa1 + guerrero1 + marvan1 + cordova1 + garcia1)
d[d$term==9,] <- tmp
#
tmp <- d[d$term==10,]
tmp <- within(tmp, vtot <- valdes1 + banos1 + nacif1 + elizondo1 + figueroa1 + guerrero1 + marvan1 + cordova1)
d[d$term==10,] <- tmp
#
tmp <- d[d$term==11,]
tmp <- within(tmp, vtot <-           banos1 + nacif1 + marvan1 + cordova1)
d[d$term==11,] <- tmp
#
tmp <- d[d$term==12,]
tmp <- within(tmp, vtot <- sanchez1 + banos1 + nacif1 + marvan1 + cordova1 + andrade21 + favela1 + galindo1 + murayama1 + ruiz1 + snmartin1 + santiago1)
d[d$term==12,] <- tmp
#
tmp <- d[d$term==13,]
tmp <- within(tmp, vtot <- banos1 + nacif1 + cordova1 + andrade21 + favela1 + murayama1 + ruiz1 + snmartin1 + ravel1 + rivera21 + zavala1)
d[d$term==13,] <- tmp
#
table(d$vtot)
#
d$absten <- NA
d$absten[d$term==1]               <- 11 - d$vtot[d$term==1]
d$absten[d$term>1 & d$term<5]     <-  9 - d$vtot[d$term>1 & d$term<5]
d$absten[d$term==5]               <-  8 - d$vtot[d$term==5]
d$absten[d$term>5 & d$term<8]     <-  9 - d$vtot[d$term>5 & d$term<8]
d$absten[d$term==8]               <-  6 - d$vtot[d$term==8]
d$absten[d$term==9]               <-  9 - d$vtot[d$term==9]
d$absten[d$term==10]              <-  8 - d$vtot[d$term==10]
d$absten[d$term==11]              <-  4 - d$vtot[d$term==11]
d$absten[d$term==12 | d$term==13] <- 11 - d$vtot[d$term==12 | d$term==13]
#
d <- within(d, nays <- vtot - ayes)
#
# verify
table(d$ayes, useNA = "always")
table(d$nays, useNA = "always")
table(d$absten, useNA = "always")
#
# recompute result
d$tmp <- 1 - d$result # for comparison (was coded 0 pass 1 fail)
d <- within(d, result <- as.numeric(ayes - nays > 0))
# there were lots of inconsistent codings
## table(d$result, d$tmp, useNA = "always")
## sel <- which(d$result!=d$tmp)
## d[sel[2],]
# clean
sel <- grep(".*1$", colnames(d))
d <- d[,-sel]

# unanimous v contested votes
#
###############################################
## version up to US-Mex presentation in 2010 ##
###############################################
## replace unanime=1 if absten==0 & (ayes==9 | ayes==0) & term>1
## replace unanime=1 if absten==0 & (ayes==11 | ayes==0) & term==1
## replace unanime=1 if absten==1 & segob==. & (ayes==10 | ayes==0) & term==1
## replace unanime=1 if absten==9 & term>1
## replace unanime=1 if absten==0 & (ayes==8 | ayes==0) & term==5
## replace unanime=1 if absten==0 & (ayes==6 | ayes==0) & term==8
d$dunan.old <- 0
#with(d, table(ayes, nays, absten))
d$dunan.old[d$absten==0 & (d$ayes==0 | d$nays==0)]  <- 1 

#############################################################
## versión que excluye ausentes de la cuenta de unanimidad ##
#############################################################
# falta para carpizo
d <- within(d, {
    barragangone <-    as.numeric(   barragan==4 | barragan==5);
    cantugone <-       as.numeric(      cantu==4 | cantu==5);
    cardenasgone <-    as.numeric(   cardenas==4 | cardenas==5);
    molinargone <-     as.numeric(    molinar==4 | molinar==5);
    lujambiogone <-    as.numeric(   lujambio==4 | lujambio==5);
    merinogone <-      as.numeric(     merino==4 | merino==5);
    peschardgone <-    as.numeric(   peschard==4 | peschard==5);
    woldenberggone <-  as.numeric( woldenberg==4 | woldenberg==5);
    zebaduagone <-     as.numeric(    zebadua==4 | zebadua==5);
    lukengone <-       as.numeric(      luken==4 | luken==5);
    riveragone <-      as.numeric(     rivera==4 | rivera==5);
    albogone <-        as.numeric(       albo==4 | albo==5);
    alcantargone <-    as.numeric(   alcantar==4 | alcantar==5);
    andradegone <-     as.numeric(    andrade==4 | andrade==5);
    glezlunagone <-    as.numeric(   glezluna==4 | glezluna==5);
    latapigone <-      as.numeric(     latapi==4 | latapi==5);
    lopezfloresgone <- as.numeric(lopezflores==4 | lopezflores==5);
    moralesgone <-     as.numeric(    morales==4 | morales==5);
    sanchezgone <-     as.numeric(    sanchez==4 | sanchez==5);
    ugaldegone <-      as.numeric(     ugalde==4 | ugalde==5);
    banosgone <-       as.numeric(      banos==4 | banos==5);
    nacifgone <-       as.numeric(      nacif==4 | nacif==5);
    valdesgone <-      as.numeric(     valdes==4 | valdes==5);
    elizondogone <-    as.numeric(   elizondo==4 | elizondo==5);
    figueroagone <-    as.numeric(   figueroa==4 | figueroa==5);
    guerrerogone <-    as.numeric(   guerrero==4 | guerrero==5);
    cordovagone <-     as.numeric(    cordova==4 | cordova==5);
    garciagone <-      as.numeric(     garcia==4 | garcia==5);
    marvangone <-      as.numeric(     marvan==4 | marvan==5);
    andrade2gone <-    as.numeric(   andrade2==4 | andrade2==5);
    favelagone <-      as.numeric(     favela==4 | favela==5);
    galindogone <-     as.numeric(    galindo==4 | galindo==5);
    murayamagone <-    as.numeric(   murayama==4 | murayama==5);
    ruizgone <-        as.numeric(       ruiz==4 | ruiz==5);
    snmartingone <-    as.numeric(   snmartin==4 | snmartin==5);
    santiagogone <-    as.numeric(   santiago==4 | santiago==5);
    ravelgone <-       as.numeric(      ravel==4 | ravel==5);
    rivera2gone <-     as.numeric(    rivera2==4 | rivera2==5);
    zavalagone <-      as.numeric(     zavala==4 | zavala==5);
    ## maganagone <-      as.numeric(     magana==4 | magana==5);
    ## fazgone <-         as.numeric(        faz==4 | faz==5);
    ## humphreygone <-    as.numeric(   humphrey==4 | humphrey==5);
    ## kibgone <-         as.numeric(        kib==4 | kib==5);
})
d <- within(d, noshow <- woldenberggone + molinargone + lujambiogone + peschardgone + merinogone + cardenasgone + barragangone + cantugone + zebaduagone + lukengone + riveragone + albogone + glezlunagone + sanchezgone + moralesgone + ugaldegone + latapigone + andradegone + lopezfloresgone + alcantargone + valdesgone + banosgone + nacifgone + elizondogone + figueroagone + guerrerogone + marvangone + cordovagone + garciagone + andrade2gone + favelagone + galindogone + murayamagone + ruizgone + snmartingone + santiagogone + ravelgone + rivera2gone + zavalagone)
## + maganagone + fazgone + humphreygone + kibgone
# check
#table(d$noshow)
# clean
sel <- grep(".*gone$", colnames(d))
d <- d[,-sel]
d$tmp <- NULL
#
# noshows must be deducted from abstentions
d$absten <- d$absten - d$noshow
#
# "A vote qualifies as contested when, ignoring absences, at least one councilor voted contrary to the rest or abstained" ifedyn06
d$dunan <- 0
d$dunan[d$absten==0 & (d$ayes==0 | d$nays==0)]  <- 1 
#
########################################################
## COULD GET OTHER UNANIMITY MEASURE FROM ife.do HERE ##
########################################################
#
# inspect
table(factor(d$dunan, labels = c("contested","not")), factor(d$dunan.old, labels = c("old-contested","not")), useNA = "always")
table(factor(d$dunan, labels = c("contested","not")), term=d$term, useNA = "ifany")

# sort agg vote count columns
sel <- which(colnames(d) %in% c("vtot","ayes","nays","absten","noshow"))
tmp <- d[,c("vtot","ayes","nays","absten","noshow")]
d[,sel] <- tmp
d <- d[,-sel]
d <- cbind(d, tmp)

# add date
d <- within(d, date <- ymd(yr*10000+mo*100+dy))
# add quarter/semester in calendar year (ojo: no longer our october-to-march version)
d <- within(d, {
    qtr <- year(date) + quarter(date)/10;
    sem <- year(date) + semester(date)/10;
})


#################
## export data ##
#################
#info.cols <- c("folio", "date", "qtr", "sem", "term")
info.cols <- c("folio", "date", "yr", "mo", "dy", "qtr", "sem", "term", "dunan")
#
sel.r <- which(d$term %in% c(2,3))
sel.c <- c("woldenberg", "barragan", "cantu", "cardenas", "lujambio", "merino", "molinar", "peschard", "zebadua", "rivera", "luken", info.cols)
tmp <- d[sel.r, sel.c]
write.csv(tmp, file = "data/v23.csv", row.names = FALSE)
#
## sel.r <- which(d$term %in% 4:8)
## c("ugalde", "albo", "andrade", "alcantar", "glezluna", "latapi", "lopezflores", "morales", "sanchez", "valdes", "banos", "nacif", "elizondo", "figueroa", "guerrero", info.cols)
## tmp <- d[sel.r, sel.c]
## write.csv(tmp, file = "data/v45678.csv", row.names = FALSE)
## #
## sel.r <- which(d$term %in% 4:9)
## c("ugalde", "albo", "andrade", "alcantar", "glezluna", "latapi", "lopezflores", "morales", "sanchez", "valdes", "banos", "nacif", "elizondo", "figueroa", "guerrero", "marvan", "cordova", "garcia", info.cols)
## tmp <- d[sel.r, sel.c]
## write.csv(tmp, file = "data/v456789.csv", row.names = FALSE)
#
sel.r <- which(d$term %in% 4:11)
sel.c <- c("ugalde", "albo", "andrade", "alcantar", "glezluna", "latapi", "lopezflores", "morales", "sanchez", "valdes", "banos", "nacif", "elizondo", "figueroa", "guerrero", "cordova", "garcia", "marvan", info.cols)
tmp <- d[sel.r, sel.c]
write.csv(tmp, file = "data/v456789ab.csv", row.names = FALSE)
#
sel.r <- which(d$term %in% 12:13)
sel.c <- c("cordova", "banos", "andrade2", "favela", "galindo", "murayama", "nacif", "ruiz", "sanchez", "santiago", "snmartin", "ravel", "rivera2", "zavala", info.cols)
tmp <- d[sel.r, sel.c]
write.csv(tmp, file = "data/vcd.csv", row.names = FALSE)


# summarize contested votes
with(d[d$dunan==0,], plot(as.factor(year(date)+quarter(date)/10), main = "N monthly contested votes"))
with(d[d$dunan==0,], plot(as.factor(date), main = "N contested votes by session"))

cuts <- c(
    ymd("19961031"), # 1  to 2
    ymd("20001211"), # 2  to 3
    ymd("20031105"), # 3  to 4
    ymd("20071217"), # 4  to 5
    ymd("20080215"), # 5  to 6
    ymd("20080821"), # 6  to 7
    ymd("20101031"), # 7  to 8
    ymd("20111215"), # 8  to 9
    ymd("20130220"), # 9  to 10
    ymd("20131031")  # 10 to 11
)

############################################
## explore issues to is search of anchors ##
############################################
sel.r <- which(d$term %in% 4:11)
sel.c <- c("term", "noCG", "date", "folio", "dlogroll", "result", "dunan", "vtot", "ayes", "nays", "absten", "noshow", "acuerdo", "ugalde", "albo", "andrade", "alcantar", "glezluna", "latapi", "lopezflores", "morales", "sanchez", "valdes", "banos", "nacif", "elizondo", "figueroa", "guerrero", "cordova", "garcia", "marvan")
tmp <- d[sel.r, sel.c]
# drop unanimous
sel <- which(tmp$dunan==1)
tmp <- tmp[-sel,]
dim(tmp)
table(tmp$term)
with(tmp, table(ayes=ayes[term==6], nays=nays[term==6]))
with(tmp, table(ayes=ayes[term==7], nays=nays[term==7]))
with(tmp, table(ayes=ayes[term==8], nays=nays[term==8]))
with(tmp, table(ayes=ayes[term==9], nays=nays[term==9]))
with(tmp, table(ayes=ayes[term==10], nays=nays[term==10]))
with(tmp, table(ayes=ayes[term==11], nays=nays[term==11]))

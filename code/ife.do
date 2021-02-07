version 8

*cd "d:\01\Dropbox\data\rollcall\ife_cg\"
cd "C:\Users\emagarm\Documents\Dropbox\data\rollcall\ife_cg\"

use "ife_cg_2012.dta", clear      /* ESTA ES LA QUE USAMOS PARA EL MOD. DINAMICO HASTA OCT 2012 */
*use "d:\01\Dropbox\data\rollcall\ife_cg\ife_cg_2012.dta", clear      /* ESTA ES LA QUE USAMOS PARA EL MOD. DINAMICO HASTA OCT 2012 */
*use "d:\01\Dropbox\data\rollcall\ife_cg\ife_cg_2011.dta", clear      /* ESTA ES LA QUE USAMOS PARA EL MOD. DINAMICO HASTA 2011 */
*use "C:\data\rollcall\ife_cg\ife_cg.dta", clear      /* ESTA ES LA QUE USAMOS PARA EL MOD. DINAMICO APSA 2009 */
*use "C:\data\rollcall\ife_cg\ife_cg_sg.dta", clear  /* ESTA ES LA QUE USAMOS PARA EL ARTICULO DE EL. STUD. */

** RECODIFICA LAS ABSTENCIONES DE MUÑOZ LEDO ET AL (EXCEPTO CARPIZO) COMO VOTOS EN CONTRA
** Añadido 16abr2013 a sugerencia de FEE: parece justificado para Muñoz Ledo, pero el cambio incide en ausencias 
** y abstenciones de todos los demás consejeros. OJO
recode woldenberg 3=2 if term==1
recode senpri 3=2 if term==1
recode senprd 3=2 if term==1
recode dippri 3=2 if term==1
recode dippan 3=2 if term==1
recode creel 3=2 if term==1
recode granados 3=2 if term==1
recode zertuche 3=2 if term==1
recode ortizpinchetti 3=2 if term==1
recode pozas 3=2 if term==1

gen tmp=(folio>3601 & yr>=2007)
gen dapsa=1-tmp /* drop dapsa==0 to replicate APSA 2009 dynamic model */
drop tmp

gen yrmody=yr*10000+mo*100+dy

*habia que homologar los codigos de mariana con los de Sergio y Gustavo... BUT NOT ANYMORE
*recode woldenberg 0=2 1=. 2=1 if term==1
*recode senpri 0=2 1=. 2=1 if term==1
*recode senprd 0=2 1=. 2=1 if term==1
*recode dippri 0=2 1=. 2=1 if term==1
*recode dippan 0=2 1=. 2=1 if term==1
*recode creel 0=2 1=. 2=1 if term==1
*recode granados 0=2 1=. 2=1 if term==1
*recode zertuche 0=2 1=. 2=1 if term==1
*recode pinchetti 0=2 1=. 2=1 if term==1
*recode pozas 0=2 1=. 2=1 if term==1

*corrige un logroll cuyo voto aparece al final en vez del principio
replace folio=99999 if folio==2299
replace folio=2299 if folio==2315
replace folio=2315 if folio==99999
sort folio
*corrige otro no-logroll mal codificado
replace logroll=0 if folio==2623
*corrige elementos de un Segundo logroll que pertenecen al primero...
replace numconjun=1 if folio>=2503 & folio <=2514

*para que los folios 5000 a 5046 aparezcan en sus lugares cronológicos
*gsort yrmody folio
*replace folio=55.5 if folio==5000
*replace folio=84.5 if folio==5001
*replace folio=132.5 if folio==5002
*replace folio=1561.5 if folio==5003
*replace folio=180.1 if folio==5004
*replace folio=180.2 if folio==5005
*replace folio=180.3 if folio==5006
*replace folio=180.4 if folio==5007
*replace folio=180.5 if folio==5008
*replace folio=180.6 if folio==5009
*sort folio

rename ortizpinchetti pinchetti

gen tmp=.
move tmp segob
replace tmp=. if segob=="No vota"
replace tmp=1 if segob=="2"
drop segob
rename tmp segob

*gen tmp=.
*move tmp granados
*replace tmp=0 if granados=="0"
*replace tmp=1 if granados=="1"
*drop granados
*rename tmp granados

*gen tmp=.
*move tmp pozas
*replace tmp=0 if pozas=="0"
*replace tmp=1 if pozas=="1"
*drop pozas
*rename tmp pozas

gen yrmo=yr*100+mo

*gen qr=0
*replace qr=1 if mo==1 | mo==2 | mo==3
*replace qr=2 if mo==4 | mo==5 | mo==6
*replace qr=3 if mo==7 | mo==8 | mo==9
*replace qr=4 if mo==10 | mo==11 | mo==12

gen sm=0
*replace sm=1 if mo<7
*replace sm=2 if mo>6
*usar estos semestres que empiezan en octubre 25 y abril 25 para que concuerden con los cambio de cg
replace sm=1 if mo==10 & dy>=25
replace sm=1 if mo==11 | mo==12 | mo==1 | mo==2 | mo==3
replace sm=1 if mo==4 & dy<25
replace sm=2 if mo==4 & dy>=25
replace sm=2 if mo==5 | mo==6 | mo==7 | mo==8 | mo==9
replace sm=2 if mo==10 & dy<25

gen yrsm=yr+(sm-1)/2
replace yrsm=yrsm+1 if sm==1 & mo>=10

gen trim=0
replace trim=1 if mo==10 & dy>=25
replace trim=1 if mo==11 | mo==12
replace trim=1 if mo==1 & dy<25
replace trim=2 if mo==1 & dy>=25
replace trim=2 if mo==2 | mo==3
replace trim=2 if mo==4 & dy<25
replace trim=3 if mo==4 & dy>=25
replace trim=3 if mo==5 | mo==6
replace trim=3 if mo==7 & dy<25
replace trim=4 if mo==7 & dy>=25
replace trim=4 if mo==8 | mo==9
replace trim=4 if mo==10 & dy<25

gen yrtrim=yr+(trim-1)/4
replace yrtrim=yrtrim+1 if trim==1 & mo>=10

***OTRA ALTERNATIVA SERA DEJAR LOS SEMESTRES INTACTOS. SI UN CONSEJERO SALE O ENTRA A LA MITAD,
***TENDRA UN PUNTO ESTIMADO CON MUCHAS ABSTENCIONES---PERO SÓLO LO AFECTA A ÉL, MIENTRAS QUE LA
***SOLUCIÓN DE CAMBIAR LA FRONERA DE LOS TRIMESTRES AFECTA A TODOS LOS DEMÁS. SI LLEGAMOS A USAR
***TRIMESTRES EN VEZ DE SEMESTRES, ESTO SE VERÁ MENOS RARO.
**
***cambia semestres de 2000 para que frontera se ubique en salida molinar y zebadua
**gen tmp=(sm==1 & yr==2000 & mo==10 & dy>=25)
**replace tmp=1 if sm==1 & yr==2000 & mo==11
**replace sm=2 if tmp==1
**replace yrsm=yr+(sm-1)/2 if tmp==1
**drop tmp
**
*****cambia semestres de 2007 para que frontera se ubique en renuncia Ugalde (14dic07)
****gen tmp=(sm==1 & yr==2007 & mo==10 & dy>=25)
****replace tmp=1 if sm==1 & yr==2007 & mo==11
****replace tmp=1 if sm==1 & yr==2007 & mo==12
****replace sm=2 if tmp==1
****replace yrsm=yr+(sm-1)/2 if tmp==1
****drop tmp
**
***cambia semestres de 2007 para que frontera se ubique en entrada Valdés (15feb08)
**gen tmp=(sm==1 & yr==2007 & mo==10 & dy>=25)
**replace tmp=1 if sm==1 & yr==2007 & mo==11
**replace tmp=1 if sm==1 & yr==2007 & mo==12
**replace tmp=1 if sm==1 & yr==2008 & mo==1
**replace tmp=1 if sm==1 & yr==2008 & mo==2 & dy<15
**replace sm=2 if tmp==1
**replace yrsm=yr+(sm-1)/2 if tmp==1 & yr==2007
**replace yrsm=yr-1+(sm-1)/2 if tmp==1 & yr==2008
**drop tmp
**
***cambia semestres de 2008 para que frontera se ubique en salida albo+glezluna+lpzflores (15ago08)
**gen tmp=(sm==2 & yr==2008 & mo==8 & dy>=15)
**replace tmp=1 if sm==2 & yr==2008 & mo==9
**replace tmp=1 if sm==2 & yr==2008 & mo==10
**replace sm=1 if tmp==1
**replace yrsm=yr+1+(sm-1)/2 if tmp==1
**drop tmp
**
***cambia semestres de 2010 para que frontera se ubique en salida sanchez+andrade+alcantar (27oct10)
**replace yrsm=2010.5 if term==7 & yr==2010 & mo==10 & dy<=27
**
*** INTERREGNO ALBO (TERM=5) 17dic2007 -- 14feb2008  Ugalde OUT (un solo contested vote)
*** VALDES I (TERM=6)        15feb2008 -- 14ago2008  Latapi+Morales OUT / Baños+Nacif+Valdes IN
*** VALDES II (TERM=7)       15ago2008 -- 27oct2010  Albo+GlezLuna+LpzFlores OUT / Elizondo+Figueroa+Guerrero IN
*** VALDES III (TERM=8)      28oct2010 -- 14dic2011  Sanchez+Alcantar+Andrade OUT / IN none
*** VALDES IV (TERM=9)       15dic2011 --            none OUT / IN Cordova+Marvan+GarciaRmz
***
*** SOBRE LA RECODIFICACION DE SEMESTRES UG-OUT VAL-IN QUE HICE EN SEPT DE 2010 Y USAMOS EN US-MEX
***                                         3in                   3in
***                                1out    2out                  3out
***    2007                 2007   2007    2008   2008           2008   2008                2009
***    25/4                25/10  17/12    15/2   25/4           15/8  25/10                25/4
***      +--------------------+------+-------+------+--------------+------+--------------------+
***real  |       2007.5       |       2008.0        |       2008.5        |       2009.0       |
***      +--------------------+---------------------+---------------------+--------------------+
***in use|              2007.5               |2008.0|    2008.5    |          2009.0           |
***      +--------------------+---------------------+---------------------+--------------------+
***better|      2007.5        |    2008.0    |       2008.5        |          2009.0           |
***      +--------------------+---------------------+---------------------+--------------------+
***trimes| 2007.5  | 2007.75  | 2008.0   | 2008.25  | 2008.5   | 2008.75  | 2009.0  | 2009.25  |
***      +--------------------+---------------------+---------------------+--------------------+
***
***
***                                                                     3out
***    2009                 2009                2010                    2010                 2011
***    25/4                25/10                25/4                   27/10                 25/4
***      +--------------------+---------------------+---------------------+--------------------+
***real  |       2009.5       |       2010.0        |       2010.5        |       2011.0       |
***      +--------------------+---------------------+---------------------+--------------------+
***in use|       2009.5       |       2010.0        |       2010.5        |       2011.0       |
***      +--------------------+---------------------+---------------------+--------------------+
***better|       2009.5       |       2010.0        |       2010.5        |       2011.0       |
***      +--------------------+---------------------+---------------------+--------------------+
***trimes| 2009.5 | 2009.75   | 2010.0   | 2010.25  | 2010.5  | 2010.75   | 2011.0  | 2011.25  |
***      +--------------------+---------------------+---------------------+--------------------+
***
***
***                                  3in
***    2011                 2011                  2012                   2012
***    25/4                25/10    15/12         25/4                  25/10
***      +--------------------+-------+-------------+---------------------+
***real  |       2011.5       |       2012.0        |       2012.5        |
***      +--------------------+---------------------+---------------------+
***in use|       2011.5       |2012.0 |              2012.5               |
***      +--------------------+---------------------+---------------------+
***better|
***      +--------------------+---------------------+---------------------+
***trimes| 2011.5  | 2011.75  | 2012.0  | 2012.25   | 2012.5  | 2012.75   |
***      +--------------------+---------------------+---------------------+

gen tmp00=0
gen tmp01=0
gen tmp02=0
gen tmp03=0
gen tmp04=0
gen tmp05=0
gen tmp06=0
gen tmp07=0
gen tmp08=0
gen tmp09=0
gen tmp10=0
gen tmp11=0
gen tmp12=0
gen tmp13=0
gen tmp14=0
gen tmp15=0
gen tmp16=0
gen tmp17=0
gen tmp18=0
gen tmp19=0
gen tmp20=0
gen tmp21=0
gen tmp22=0
gen tmp23=0
gen tmp24=0
gen tmp25=0
gen tmp26=0
gen tmp27=0
gen tmp28=0
gen tmp29=0
gen tmp30=0
gen tmp31=0
gen tmp32=0
gen tmp33=0
gen tmp34=0
gen tmp35=0
gen tmp36=0
gen tmp37=0
gen tmp38=0

replace tmp00=1 if merino==1
replace tmp01=1 if woldenberg==1
replace tmp02=1 if barragan==1
replace tmp03=1 if cantu==1
replace tmp04=1 if cardenas==1
replace tmp05=1 if lujambio==1
replace tmp06=1 if molinar==1
replace tmp07=1 if peschard==1
replace tmp08=1 if zebadua==1

replace tmp09=1 if rivera==1
replace tmp10=1 if luken==1

replace tmp11=1 if ugalde==1
replace tmp12=1 if albo==1
replace tmp13=1 if andrade==1
replace tmp14=1 if alcantar==1
replace tmp15=1 if glezluna==1
replace tmp16=1 if latapi==1
replace tmp17=1 if lopezflores==1
replace tmp18=1 if morales==1
replace tmp19=1 if sanchez==1

replace tmp20=1 if segob==1
replace tmp21=1 if senpri==1
replace tmp22=1 if senprd==1
replace tmp23=1 if dippri==1
replace tmp24=1 if dippan==1
replace tmp25=1 if creel==1
replace tmp26=1 if granados==1
replace tmp27=1 if zertuche==1
replace tmp28=1 if pinchetti==1
replace tmp29=1 if pozas==1

replace tmp30=1 if valdes==1
replace tmp31=1 if banos==1
replace tmp32=1 if nacif==1
replace tmp33=1 if elizondo==1
replace tmp34=1 if figueroa==1
replace tmp35=1 if guerrero==1

replace tmp36=1 if marvan==1
replace tmp37=1 if cordova==1
replace tmp38=1 if garcia==1

gen ayes=tmp00+tmp01+tmp02+tmp03+tmp04+tmp05+tmp06+tmp07+tmp08+tmp09+tmp10+tmp11+tmp12+tmp13+tmp14+tmp15+tmp16+tmp17+tmp18+tmp19+tmp20+tmp21+tmp22+tmp23+tmp24+tmp25+tmp26+tmp27+tmp28+tmp29+tmp30+tmp31+tmp32+tmp33+tmp34+tmp35+tmp36+tmp37+tmp38
replace ayes=. if logroll==1 & numconjun==numconjun[_n-1]

drop tmp00-tmp38

*Habra que crear variable numitems de un logroll... esto quita las observaciones redundantes de un logroll
drop if logroll==1 & numconjun==numconjun[_n-1]
*drops 4 votes (with logrolls) where all 9 abstained
drop if folio==5013 | folio==5024 | folio==5031 | folio==5037 | folio==2503

gen segob1=0
replace segob1=1 if segob==1 | segob==2
gen senpri1=0
replace senpri1=1 if senpri==1 | senpri==2
gen senprd1=0
replace senprd1=1 if senprd==1 | senprd==2
gen dippri1=0
replace dippri1=1 if dippri==1 | dippri==2
gen dippan1=0
replace dippan1=1 if dippan==1 | dippan==2
gen creel1=0
replace creel1=1 if creel==1 | creel==2
gen granados1=0
replace granados1=1 if granados==1 | granados==2
gen woldenberg1=0
replace woldenberg1=1 if woldenberg==1 | woldenberg==2
gen zertuche1=0
replace zertuche1=1 if zertuche==1 | zertuche==2
gen pinchetti1=0
replace pinchetti1=1 if pinchetti==1 | pinchetti==2
gen pozas1=0
replace pozas1=1 if pozas==1 | pozas==2
gen peschard1=0
replace peschard1=1 if peschard==1 | peschard==2
gen lujambio1=0
replace lujambio1=1 if lujambio==1 | lujambio==2
gen cardenas1=0
replace cardenas1=1 if cardenas==1 | cardenas==2
gen cantu1=0
replace cantu1=1 if cantu==1 | cantu==2
gen merino1=0
replace merino1=1 if merino==1 | merino==2
gen barragan1=0
replace barragan1=1 if barragan==1 | barragan==2
gen zebadua1=0
replace zebadua1=1 if zebadua==1 | zebadua==2
gen molinar1=0
replace molinar1=1 if molinar==1 | molinar==2
gen rivera1=0
replace rivera1=1 if rivera==1 | rivera==2
gen luken1=0
replace luken1=1 if luken==1 | luken==2
gen ugalde1=0
replace ugalde1=1 if ugalde==1 | ugalde==2
gen albo1=0
replace albo1=1 if albo==1 | albo==2
gen andrade1=0
replace andrade1=1 if andrade==1 | andrade==2
gen alcantar1=0
replace alcantar1=1 if alcantar==1 | alcantar==2
gen glezluna1=0
replace glezluna1=1 if glezluna==1 | glezluna==2
gen latapi1=0
replace latapi1=1 if latapi==1 | latapi==2
gen lopezflores1=0
replace lopezflores1=1 if lopezflores==1 | lopezflores==2
gen morales1=0
replace morales1=1 if morales==1 | morales==2
gen sanchez1=0
replace sanchez1=1 if sanchez==1 | sanchez==2
gen valdes1=0
replace valdes1=1 if valdes==1 | valdes==2
gen banos1=0
replace banos1=1 if banos==1 | banos==2
gen nacif1=0
replace nacif1=1 if nacif==1 | nacif==2
gen elizondo1=0
replace elizondo1=1 if elizondo==1 | elizondo==2
gen figueroa1=0
replace figueroa1=1 if figueroa==1 | figueroa==2
gen guerrero1=0
replace guerrero1=1 if guerrero==1 | guerrero==2
gen marvan1=0
replace marvan1=1 if marvan==1 | marvan==2
gen cordova1=0
replace cordova1=1 if cordova==1 | cordova==2
gen garcia1=0
replace garcia1=1 if garcia==1 | garcia==2

gen vtot=segob1+senpri1+senprd1+dippri1+dippan1+creel1+granados1+woldenberg1+zertuche1+pinchetti1+pozas1 if term==1
replace vtot=woldenberg1+peschard1+lujambio1+cardenas1+cantu1+merino1+barragan1+zebadua1+molinar1 if term==2
replace vtot= woldenberg1+peschard1+lujambio1+cardenas1+cantu1+merino1+barragan1+rivera1+luken1 if term==3
replace vtot=ugalde1+albo1+andrade1+alcantar1+glezluna1+latapi1+lopezflores1+morales1+sanchez1 if term==4
replace vtot=albo1+andrade1+alcantar1+glezluna1+latapi1+lopezflores1+morales1+sanchez1 if term==5
replace vtot=albo1+andrade1+alcantar1+glezluna1+lopezflores1+sanchez1+valdes1+banos1+nacif1 if term==6
replace vtot=andrade1+alcantar1+sanchez1+valdes1+banos1+nacif1+elizondo1+figueroa1+guerrero1 if term==7
replace vtot=                            valdes1+banos1+nacif1+elizondo1+figueroa1+guerrero1 if term==8
replace vtot=valdes1+banos1+nacif1+elizondo1+figueroa1+guerrero1+marvan1+cordova1+garcia1 if term==9

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

** ESTA ES LA BATERIA QUE CODIFICA LA VAR unanime QUE USAMOS HASTA US-MEX
gen unanime=0
replace unanime=1 if absten==0 & (ayes==9 | ayes==0) & term>1
replace unanime=1 if absten==0 & (ayes==11 | ayes==0) & term==1
replace unanime=1 if absten==1 & segob==. & (ayes==10 | ayes==0) & term==1
move unanime ayes
replace unanime=1 if absten==9 & term>1

*replace unanime=1 if dunan==1     /* Estas las codificó Eric MAL (CORREGIR CUANDO LIMPIE) desde la base excel */
replace unanime=1 if absten==0 & (ayes==8 | ayes==0) & term==5
replace unanime=1 if absten==0 & (ayes==6 | ayes==0) & term==8

**** ESTA BATERIA QUE CODIFICA UNA VAR unanime ALTERNATIVA, CREO QUE CONSIGUE DESCONTAR ABSTENCION DE NO UNANIME
**gen unanime=0
**replace unanime=1 if dunan==1     /* Estas las codificó Eric desde la base excel */
**replace unanime=1 if absten==0 & (ayes==11 | ayes==0) & term==1
**replace unanime=1 if absten==1 & segob==. & (ayes==10 | ayes==0) & term==1
**replace unanime=1 if absten==0 & (ayes==9 | ayes==0) & term>1 & term<5
**replace unanime=1 if absten==0 & (ayes==8 | ayes==0) & term==5
**replace unanime=1 if absten==0 & (ayes==9 | ayes==0) & term>5
**replace unanime=1 if absten==1 & (ayes==8 | ayes==0) & term>1 & term<5
**replace unanime=1 if absten==1 & (ayes==7 | ayes==0) & term==5
**replace unanime=1 if absten==1 & (ayes==8 | ayes==0) & term>5
**replace unanime=1 if absten==2 & (ayes==7 | ayes==0) & term>1 & term<5
**replace unanime=1 if absten==2 & (ayes==6 | ayes==0) & term==5
**replace unanime=1 if absten==2 & (ayes==7 | ayes==0) & term>5
**replace unanime=1 if absten==3 & (ayes==6 | ayes==0) & term>1 & term<5
**replace unanime=1 if absten==3 & (ayes==5 | ayes==0) & term==5
**replace unanime=1 if absten==3 & (ayes==6 | ayes==0) & term>5
**replace unanime=1 if absten==4 & (ayes==5 | ayes==0) & term>1 & term<5
**replace unanime=1 if absten==4 & (ayes==4 | ayes==0) & term==5
**replace unanime=1 if absten==4 & (ayes==5 | ayes==0) & term>5
**replace unanime=1 if absten==5 & (ayes==4 | ayes==0) & term>1 & term<5
**replace unanime=1 if absten==5 & (ayes==3 | ayes==0) & term==5
**replace unanime=1 if absten==5 & (ayes==4 | ayes==0) & term>5
**replace unanime=1 if absten==6 & (ayes==3 | ayes==0) & term>1 & term<5
**replace unanime=1 if absten==6 & (ayes==2 | ayes==0) & term==5
**replace unanime=1 if absten==6 & (ayes==3 | ayes==0) & term>5
**replace unanime=1 if absten==7 & (ayes==2 | ayes==0) & term>1 & term<5
**replace unanime=1 if absten>=7 & term==5
**replace unanime=1 if absten==7 & (ayes==2 | ayes==0) & term>5
**replace unanime=1 if absten==11 & term==1
**replace unanime=1 if absten>=8 & term>1 & term<5
**replace unanime=1 if absten>=8 & term>5
**move unanime ayes

*esto incluye los casos de ausencias en el conteo de unanimidad
*falta para carpizo
gen woldenberggone=0
gen molinargone=0
gen lujambiogone=0
gen peschardgone=0
gen merinogone=0
gen cardenasgone=0
gen barragangone=0
gen cantugone=0
gen zebaduagone=0
gen lukengone=0
gen riveragone=0
gen albogone=0
gen glezlunagone=0
gen sanchezgone=0
gen moralesgone=0
gen ugaldegone=0
gen latapigone=0
gen andradegone=0
gen lopezfloresgone=0
gen alcantargone=0
gen valdesgone=0
gen banosgone=0
gen nacifgone=0
gen elizondogone=0
gen figueroagone=0
gen guerrerogone=0
gen marvangone=0
gen cordovagone=0
gen garciagone=0

replace woldenberggone=1 if woldenberg==4 | woldenberg==5
replace molinargone=1 if molinar==4 | molinar==5
replace lujambiogone=1 if lujambio==4 | lujambio==5
replace peschardgone=1 if peschard==4 | peschard==5
replace merinogone=1 if merino==4 | merino==5
replace cardenasgone=1 if cardenas==4 | cardenas==5
replace barragangone=1 if barragan==4 | barragan==5
replace cantugone=1 if cantu==4 | cantu==5
replace zebaduagone=1 if zebadua==4 | zebadua==5
replace lukengone=1 if luken==4 | luken==5
replace riveragone=1 if rivera==4 | rivera==5
replace albogone=1 if albo==4 | albo==5
replace glezlunagone=1 if glezluna==4 | glezluna==5
replace sanchezgone=1 if sanchez==4 | sanchez==5
replace moralesgone=1 if morales==4 | morales==5
replace ugaldegone=1 if ugalde==4 | ugalde==5
replace latapigone=1 if latapi==4 | latapi==5
replace andradegone=1 if andrade==4 | andrade==5
replace lopezfloresgone=1 if lopezflores==4 | lopezflores==5
replace alcantargone=1 if alcantar==4 | alcantar==5
replace valdesgone=1 if valdes==4 | valdes==5
replace banosgone=1 if banos==4 | banos==5
replace nacifgone=1 if nacif==4 | nacif==5
replace elizondogone=1 if elizondo==4 | elizondo==5
replace figueroagone=1 if figueroa==4 | figueroa==5
replace guerrerogone=1 if guerrero==4 | guerrero==5
replace marvangone=1 if marvan==4 | marvan==5
replace cordovagone=1 if cordova==4 | cordova==5
replace garciagone=1 if garcia==4 | garcia==5

gen ngone=woldenberggone+molinargone+lujambiogone+peschardgone+merinogone+cardenasgone+barragangone+cantugone+zebaduagone+lukengone+riveragone+albogone+glezlunagone+sanchezgone+moralesgone+ugaldegone+latapigone+andradegone+lopezfloresgone+alcantargone+valdesgone+banosgone+nacifgone+elizondogone+figueroagone+guerrerogone+marvangone+cordovagone+garciagone
*drop woldenberggone molinargone lujambiogone peschardgone merinogone cardenasgone barragangone cantugone zebaduagone lukengone riveragone albogone glezlunagone sanchezgone moralesgone ugaldegone latapigone andradegone lopezfloresgone alcantargone valdesgone banosgone nacifgone elizondogone figueroagone guerrerogone marvangone cordovagone garciagone
replace unanime=1 if ngone>0 & ngone==absten & (ayes==0 | nays==0)

drop segob1 senpri1 senprd1 dippri1 dippan1 creel1 granados1 woldenberg1 zertuche1 pinchetti1 pozas1 peschard1 lujambio1 cardenas1 cantu1 merino1 barragan1 zebadua1 molinar1 rivera1 luken1 ugalde1 albo1 andrade1 alcantar1 glezluna1 latapi1 lopezflores1 morales1 sanchez1 valdes1 banos1 nacif1 elizondo1 figueroa1 guerrero1 marvan1 cordova1 garcia1

*para trabajar con abstenciones como missing (abajo está batería alternativa)
recode woldenberg 2=0 3=. 4=. 5=. 6=.
recode barragan 2=0 3=. 4=. 5=. 6=.
recode cantu  2=0 3=. 4=. 5=. 6=.
recode cardenas  2=0 3=. 4=. 5=. 6=.
recode lujambio  2=0 3=. 4=. 5=. 6=.
recode merino  2=0 3=. 4=. 5=. 6=.
recode molinar  2=0 3=. 4=. 5=. 6=.
recode peschard  2=0 3=. 4=. 5=. 6=.
recode zebadua  2=0 3=. 4=. 5=. 6=.
recode rivera  2=0 3=. 4=. 5=. 6=.
recode luken  2=0 3=. 4=. 5=. 6=.
recode ugalde  2=0 3=. 4=. 5=. 6=.
recode albo  2=0 3=. 4=. 5=. 6=.
recode andrade  2=0 3=. 4=. 5=. 6=.
recode alcantar  2=0 3=. 4=. 5=. 6=.
recode glezluna  2=0 3=. 4=. 5=. 6=.

recode latapi  2=0 3=. 4=. 5=. 6=.
recode lopezflores  2=0 3=. 4=. 5=. 6=.
recode morales  2=0 3=. 4=. 5=. 6=.
recode sanchez  2=0 3=. 4=. 5=. 6=.
recode segob  2=0 3=. 4=. 5=. 6=.
recode senpri  2=0 3=. 4=. 5=. 6=.
recode senprd  2=0 3=. 4=. 5=. 6=.
recode dippri  2=0 3=. 4=. 5=. 6=.
recode dippan  2=0 3=. 4=. 5=. 6=.
recode creel  2=0 3=. 4=. 5=. 6=.
recode granados  2=0 3=. 4=. 5=. 6=.
recode zertuche  2=0 3=. 4=. 5=. 6=.
recode pinchetti  2=0 3=. 4=. 5=. 6=.
recode pozas 2=0 3=. 4=. 5=. 6=.

recode valdes 2=0 3=. 4=. 5=. 6=.
recode banos 2=0 3=. 4=. 5=. 6=.
recode nacif 2=0 3=. 4=. 5=. 6=.
recode elizondo 2=0 3=. 4=. 5=. 6=.
recode figueroa 2=0 3=. 4=. 5=. 6=.
recode guerrero 2=0 3=. 4=. 5=. 6=.

recode marvan 2=0 3=. 4=. 5=. 6=.
recode cordova 2=0 3=. 4=. 5=. 6=.
recode garcia 2=0 3=. 4=. 5=. 6=.

*para trabajar con abstenciones como voto con minoria (no incluye consejo carpizo ya que aún no se codifican las abstenciones distinto de ausencias)
*recode woldenberg 2=0 4=. 5=. 6=.
*recode barragan 2=0 4=. 5=. 6=.
*recode cantu  2=0 4=. 5=. 6=.
*recode cardenas  2=0 4=. 5=. 6=.
*recode lujambio  2=0 4=. 5=. 6=.
*recode merino  2=0 4=. 5=. 6=.
*recode molinar  2=0 4=. 5=. 6=.
*recode peschard  2=0 4=. 5=. 6=.
*recode zebadua  2=0 4=. 5=. 6=.
*recode rivera  2=0 4=. 5=. 6=.
*recode luken  2=0 4=. 5=. 6=.
*recode ugalde  2=0 4=. 5=. 6=.
*recode albo  2=0 4=. 5=. 6=.
*recode andrade  2=0 4=. 5=. 6=.
*recode alcantar  2=0 4=. 5=. 6=.
*recode glezluna  2=0 4=. 5=. 6=.
*recode latapi  2=0 4=. 5=. 6=.
*recode lopezflores  2=0 4=. 5=. 6=.

*recode morales  2=0 4=. 5=. 6=.
*recode sanchez  2=0 4=. 5=. 6=.
*recode segob  2=0 4=. 5=. 6=.
*recode senpri  2=0 4=. 5=. 6=.
*recode senprd  2=0 4=. 5=. 6=.
*recode dippri  2=0 4=. 5=. 6=.
*recode dippan  2=0 4=. 5=. 6=.
*recode creel  2=0 4=. 5=. 6=.
*recode granados  2=0 4=. 5=. 6=.
*recode zertuche  2=0 4=. 5=. 6=.
*recode pinchetti  2=0 4=. 5=. 6=.
*recode pozas 2=0 4=. 5=. 6=.
*recode woldenberg 3=0 if result==1
*recode barragan 3=0 if result==1
*recode cantu 3=0 if result==1
*recode cardenas 3=0 if result==1
*recode lujambio 3=0 if result==1
*recode merino 3=0 if result==1
*recode molinar 3=0 if result==1
*recode peschard 3=0 if result==1
*recode zebadua  3=0 if result==1
*recode rivera 3=0 if result==1
*recode luken 3=0 if result==1
*recode ugalde 3=0 if result==1
*recode albo 3=0 if result==1
*recode andrade  3=0 if result==1
*recode alcantar 3=0 if result==1
*recode glezluna 3=0 if result==1
*recode latapi 3=0 if result==1
*recode lopezflores 3=0 if result==1
*recode morales 3=0 if result==1
*recode sanchez 3=0 if result==1
*recode woldenberg 3=1 if result==0
*recode barragan 3=1 if result==0
*recode cantu 3=1 if result==0
*recode cardenas 3=1 if result==0
*recode lujambio 3=1 if result==0
*recode merino 3=1 if result==0
*recode molinar 3=1 if result==0

*recode peschard 3=1 if result==0
*recode zebadua  3=1 if result==0
*recode rivera 3=1 if result==0
*recode luken 3=1 if result==0
*recode ugalde 3=1 if result==0
*recode albo 3=1 if result==0
*recode andrade  3=1 if result==0
*recode alcantar 3=1 if result==0
*recode glezluna 3=1 if result==0
*recode latapi 3=1 if result==0
*recode lopezflores 3=1 if result==0
*recode morales 3=1 if result==0
*recode sanchez 3=1 if result==0

gen margin=ayes-nays if ayes>nays
replace margin=nays-ayes if nays>=ayes
gen winsize=max(ayes, nays, absten)

* FALTA ADAPTAR blocvote.do PARA LA ENTRADA DE valdes banos nacif elizondo figueroa guerrero etc
*bloc votes (left, center, right)
do blocvote

gen pandiv=.
gen pridiv=.
gen prddiv=.
replace pandiv=1 if molinar==1-lujambio & term==2
replace pandiv=0 if molinar==lujambio  & term==2
replace pandiv=1 if molinar==. & lujambio==1 & term==2
replace pandiv=1 if molinar==1 & lujambio==. & term==2
*this considers abstentions as nay votes
replace pandiv=0 if molinar==. & lujambio==0 & term==2
replace pandiv=0 if molinar==0 & lujambio==. & term==2
replace pandiv=1 if luken==1-lujambio & term==3
replace pandiv=0 if luken==lujambio & term==3
replace pandiv=1 if luken==. & lujambio==1 & term==3
replace pandiv=1 if luken==1 & lujambio==. & term==3
*this considers abstentions as nay votes
replace pandiv=0 if luken==. & lujambio==0 & term==3
replace pandiv=0 if luken==0 & lujambio==. & term==3
* falta pandiv etc para 2004-  replace pandiv=0 if albo==sanchez==
* NECESITO EL PARTIDO DE valdes banos nacif elizondo figueroa guerrero PARA ADAPTAR ESTO

replace pridiv=1 if merino==1-peschard
replace pridiv=0 if merino==peschard
replace pridiv=1 if merino==. & peschard==1
replace pridiv=1 if merino==1 & peschard==.
*this considers abstentions as nay votes
replace pridiv=0 if merino==. & peschard==0

replace pridiv=0 if merino==0 & peschard==.
replace prddiv=1 if cantu==1-zebadua & term==2
replace prddiv=0 if cantu==zebadua & term==2
replace prddiv=1 if cantu==. & zebadua==1 & term==2
replace prddiv=1 if cantu==1 & zebadua==. & term==2
*this considers abstentions as nay votes
replace prddiv=0 if cantu==. & zebadua==0 & term==2
replace prddiv=0 if cantu==0 & zebadua==. & term==2
gen twoplusdiv=0
replace twoplusdiv=1 if pandiv==1 & pridiv==1
replace twoplusdiv=1 if pandiv==1 & prddiv==1
replace twoplusdiv=1 if pridiv==1 & prddiv==1
replace twoplusdiv=1 if pandiv==1 & pridiv==1 & prddiv==1

gen naysayer=0
replace naysayer=1 if nays==1 & (senprd==0 | cardenas==0 | barragan==0)
replace naysayer=1 if nays==2 & cardenas==0 & barragan==0

gen vfaccpri=0
replace vfaccpri=vfaccpri+segob if term==1 & segob~=.
replace vfaccpri=vfaccpri+pozas if term==1 & pozas~=.
replace vfaccpri=vfaccpri+dippri if term==1 & dippri~=.
replace vfaccpri=vfaccpri+senpri if term==1 & senpri~=.
replace vfaccpri=vfaccpri+woldenberg if (term==2 | term==3) & woldenberg~=.
replace vfaccpri=vfaccpri+peschard if (term==2 | term==3) & peschard~=.
replace vfaccpri=vfaccpri+merino if (term==2 | term==3) & merino~=.
replace vfaccpri=vfaccpri+rivera if (term==2 | term==3) & rivera~=.
replace vfaccpri=vfaccpri+ugalde if term==4 & ugalde~=.
replace vfaccpri=vfaccpri+latapi if term==4 & latapi~=.
replace vfaccpri=vfaccpri+lopezflores if term==4 & lopezflores~=.
replace vfaccpri=vfaccpri+andrade if term==4 & andrade~=.

gen vfaccpan=0
replace vfaccpan=vfaccpan+creel if term==1 & creel~=.
replace vfaccpan=vfaccpan+woldenberg if term==1 & woldenberg~=.
replace vfaccpan=vfaccpan+dippan if term==1 & dippan~=.
replace vfaccpan=vfaccpan+lujambio if (term==2 | term==3) & lujambio~=.
replace vfaccpan=vfaccpan+molinar if term==2 & molinar~=.
replace vfaccpan=vfaccpan+luken if term==3 & luken~=.
replace vfaccpan=vfaccpan+albo if term==4 & albo~=.
replace vfaccpan=vfaccpan+sanchez if term==4 & sanchez~=.
replace vfaccpan=vfaccpan+glezluna if term==4 & glezluna~=.
replace vfaccpan=vfaccpan+morales if term==4 & morales~=.

gen vfaccprd=0
replace vfaccprd=vfaccprd+granados if term==1 & granados~=.
replace vfaccprd=vfaccprd+zertuche if term==1 & zertuche~=.
replace vfaccprd=vfaccprd+pinchetti if term==1 & pinchetti~=.
replace vfaccprd=vfaccprd+senprd if term==1 & senprd~=.
replace vfaccprd=vfaccprd+cardenas if (term==2 | term==3) & cardenas~=.
replace vfaccprd=vfaccprd+barragan if (term==2 | term==3) & barragan~=.
replace vfaccprd=vfaccprd+zebadua if term==2 & zebadua~=.


*OJO: TENGO UN ERROR EN TODO ESTO YA QUE CONSIDERO A CANTU COMO INTEGRANTE DE LA FACCION DEL PRD, LO CUAL NO ES CIERTO (AL MENOS FORMALMENTE, CANTU ERA DEL PT)
*para entender esta codificacion hay que saber cuántos votos son una mayoria en una facción, y esto cambia de consejo a consejo
*      c1   c2   c3  c4
*pri  3ó4  2ó3  2ó3   3ó4ó5
*pan  2ó3    2    2   3ó4
*prd  3ó4  3ó4  3ó4   --

gen cardbarr_v_all=0
replace cardbarr_v_all=1 if cardenas==0 & barragan==0 & woldenberg==1 & cantu==1 & lujambio==1 & merino==1 & molinar==1 & peschard==1 & zebadua==1 & term==2
replace cardbarr_v_all=1 if cardenas==1 & barragan==1 & woldenberg==0 & cantu==0 & lujambio==0 & merino==0 & molinar==0 & peschard==0 & zebadua==0 & term==2
replace cardbarr_v_all=1 if cardenas==0 & barragan==0 & woldenberg==1 & cantu==1 & lujambio==1 & merino==1 & luken==1 & peschard==1 & rivera==1 & term==3
replace cardbarr_v_all=1 if cardenas==1 & barragan==1 & woldenberg==0 & cantu==0 & lujambio==0 & merino==0 & luken==0 & peschard==0 & rivera==0 & term==3

gen pripan_v_prd=0
replace pripan_v_prd=1 if vfaccpri>=3 & vfaccpan>=2 & vfaccprd<3 & term==1
replace pripan_v_prd=1 if unanime==0 & vfaccpri<3 & vfaccpan<2 & vfaccprd>=3 & term==1
replace pripan_v_prd=1 if unanime==0 & vfaccpri>=2 & vfaccpan==2 & vfaccprd<3 & (term==2 | term==3)
replace pripan_v_prd=1 if unanime==0 & vfaccpri<2 & vfaccpan<2 & vfaccprd>=3 & (term==2 | term==3)

gen priprd_v_pan=0
replace priprd_v_pan=1 if vfaccpri>=3 & vfaccpan<2 & vfaccprd>=3 & term==1
replace priprd_v_pan=1 if unanime==0 & vfaccpri<3 & vfaccpan==2 & vfaccprd<3 & term==1
replace priprd_v_pan=1 if unanime==0 & vfaccpri>=2 & vfaccpan<2 & vfaccprd>=3 & (term==2 | term==3)
replace priprd_v_pan=1 if unanime==0 & vfaccpri<2 & vfaccpan==2 & vfaccprd<3 & (term==2 | term==3)

gen panprd_v_pri=0
replace panprd_v_pri=1 if vfaccpri>=3 & vfaccpan<2 & vfaccprd<3 & term==1
replace panprd_v_pri=1 if unanime==0 & vfaccpri<3 & vfaccpan>=2 & vfaccprd>=3 & term==1
replace panprd_v_pri=1 if unanime==0 & vfaccpri>=2 & vfaccpan<2 & vfaccprd<3 & (term==2 | term==3)
replace panprd_v_pri=1 if unanime==0 & vfaccpri<2 & vfaccpan==2 & vfaccprd>=3 & (term==2 | term==3)

gen pan_v_pri=0
replace pan_v_pri=1 if unanime==0 & vfaccpri<3 & vfaccpan>2 & term==4
replace pan_v_pri=1 if unanime==0 & vfaccpri>=3 & vfaccpan<2 & term==4

*definiciones de rolls con unanimidad de la facción... en caso de usarlas tendré que quitar variables con el mismo nombre que defino en commandos posteriores
*gen panroll=0
*replace panroll=1 if vfaccpan==0 & result==1
*replace panroll=1 if vfaccpan==2 & result==0 & (term==2 | term==3)
*replace panroll=1 if vfaccpan==1 & result==0 & term==2 & molinargone==1
*replace panroll=1 if vfaccpan==1 & result==0 & (term==2 | term==3) & lujambiogone==1
*replace panroll=1 if vfaccpan==1 & result==0 & term==3 & lukengone==1
*replace panroll=1 if vfaccpan==4 & result==0 & term==4
**corrige 2 casos que aparecen como panroll==1:
*replace panroll=0 if luken==. & lujambio==. & term==3
*por el momento no hay ausencias en ugalde

*gen priroll=0
*replace priroll=1 if vfaccpri==0 & result==1
*replace priroll=1 if vfaccpri==3 & result==0 & term==2
*replace priroll=1 if vfaccpri==4 & result==0 & term==3
*replace priroll=1 if vfaccpri==2 & result==0 & term==2 & woldenberggone==1
*replace priroll=1 if vfaccpri==2 & result==0 & term==2 & merinogone==1
*replace priroll=1 if vfaccpri==2 & result==0 & term==2 & peschardgone==1
*replace priroll=1 if vfaccpri==3 & result==0 & term==3 & woldenberggone==1
*replace priroll=1 if vfaccpri==3 & result==0 & term==3 & merinogone==1
*replace priroll=1 if vfaccpri==3 & result==0 & term==3 & peschardgone==1
*replace priroll=1 if vfaccpri==3 & result==0 & term==3 & riveragone==1
*replace priroll=1 if vfaccpri==2 & result==0 & term==3 & peschardgone==1 & woldenberggone==1
*replace priroll=1 if vfaccpri==2 & result==0 & term==3 & peschardgone==1 & merinogone==1
*replace priroll=1 if vfaccpri==4 & result==0 & term==4

**esta dummy aún tiene errores, se deben a que vfaccprd considera a cantú como parte de la faccion prd
*gen prdroll=0
*replace prdroll=1 if vfaccprd==0 & result==1
*replace prdroll=1 if vfaccprd==3 & result==0 & term==2
*replace prdroll=1 if vfaccprd==2 & result==0 & term==3
*replace prdroll=1 if vfaccprd==2 & result==0 & term==2 & zebaduagone==1
*replace prdroll=1 if vfaccprd==2 & result==0 & term==2 & cardenasgone==1
*replace prdroll=1 if vfaccprd==2 & result==0 & term==2 & barragangone==1
*replace prdroll=1 if vfaccprd==1 & result==0 & term==3 & cardenasgone==1
*replace prdroll=1 if vfaccprd==1 & result==0 & term==3 & barragangone==1

*no entiendo para que creé unantmp (en todo caso debiera estar condicionada a
*ayes==10 ó ayes==0 en vez de vtot==10...) Cambié por unanime en el commando
*subsecuente...
*gen unantmp=unanime
*replace unantmp=1 if vtot==10 & segob==. & term==1
gen non_rolling=0
replace non_rolling=1 if unanime==0 & vfaccpri>=3 & vfaccpan>=2 & vfaccprd>=3 & term==1
replace non_rolling=1 if unanime==0 & vfaccpri<3 & vfaccpan<2 & vfaccprd<3 & term==1
replace non_rolling=1 if unanime==0 & vfaccpri<2 & vfaccpan<2 & vfaccprd<3 & (term==2 | term==3)
replace non_rolling=1 if unanime==0 & vfaccpri>=2 & vfaccpan==2 & vfaccprd>=3 & (term==2 | term==3)
replace non_rolling=1 if unanime==0 & vfaccpri>=3 & vfaccpan>=3 & term==4
replace non_rolling=1 if unanime==0 & vfaccpri<3 & vfaccpan<3 & term==4
*drop unantmp

gen vfaccpoli=0
replace vfaccpoli=vfaccpoli+peschard if (term==2 | term==3) & peschard~=.
replace vfaccpoli=vfaccpoli+woldenberg if (term==2 | term==3) & woldenberg~=.
replace vfaccpoli=vfaccpoli+lujambio if (term==2 | term==3) & lujambio~=.
replace vfaccpoli=vfaccpoli+molinar if term==2 & molinar~=.
replace vfaccpoli=vfaccpoli+zebadua if term==2 & zebadua~=.
replace vfaccpoli=vfaccpoli+merino if (term==2 | term==3) & merino~=.
* if needed replace for 2004-

gen vfaccabog=0
replace vfaccabog=vfaccabog+cardenas if (term==2 | term==3) & cardenas~=.
replace vfaccabog=vfaccabog+barragan if (term==2 | term==3) & barragan~=.

replace vfaccabog=vfaccabog+rivera if term==3 & rivera~=.
* if needed replace for 2004-

gen poli_v_abog=0
replace poli_v_abog=1 if vfaccpoli>=4 & vfaccabog==0 & term==2 & unanim==0
replace poli_v_abog=1 if vfaccpoli>=3 & vfaccabog<2 & term==3 & unanim==0
replace poli_v_abog=1 if vfaccpoli<4 & vfaccabog==2 & term==2 & unanim==0
replace poli_v_abog=1 if vfaccpoli<3 & vfaccabog>=2 & term==2 & unanim==0
* if needed replace for 2004-

gen poliunity=0
replace poliunity=5 if vfaccpoli==5

*si se consideran las abstenciones como votos en contra (abstain==nay), el panorama lo dan las variables de voto seguidas por un cero
*gen segob0=0
*replace segob0=1 if segob==1
*gen senpri0=0
*replace senpri0=1 if senpri==1
*gen senprd0=0
*replace senprd0=1 if senprd==1
*gen dippri0=0
*replace dippri0=1 if dippri==1
*gen dippan0=0
*replace dippan0=1 if dippan==1
*gen creel0=0
*replace creel0=1 if creel==1
*gen granados0=0
*replace granados0=1 if granados==1
*gen woldenberg0=0
*replace woldenberg0=1 if woldenberg==1
*gen zertuche0=0
*replace zertuche0=1 if zertuche==1
*gen pinchetti0=0
*replace pinchetti0=1 if pinchetti==1
*gen pozas0=0
*replace pozas0=1 if pozas==1
*gen peschard0=0
*replace peschard0=1 if peschard==1
*gen lujambio0=0
*replace lujambio0=1 if lujambio==1
*gen cardenas0=0
*replace cardenas0=1 if cardenas==1
*gen cantu0=0
*replace cantu0=1 if cantu==1
*gen merino0=0
*replace merino0=1 if merino==1
*gen barragan0=0
*replace barragan0=1 if barragan==1
*gen zebadua0=0

*replace zebadua0=1 if zebadua==1
*gen molinar0=0
*replace molinar0=1 if molinar==1
*gen rivera0=0
*replace rivera0=1 if rivera==1
*gen luken0=0
*replace luken0=1 if luken==1
*gen ugalde0=0
*replace ugalde0=1 if ugalde==1
*gen albo0=0
*replace albo0=1 if albo==1
*gen andrade0=0
*replace andrade0=1 if andrade==1
*gen alcantar0=0
*replace alcantar0=1 if alcantar==1
*gen glezluna0=0
*replace glezluna0=1 if glezluna==1
*gen latapi0=0
*replace latapi0=1 if latapi==1
*gen lopezflores0=0
*replace lopezflores0=1 if lopezflores==1
*gen morales0=0
*replace morales0=1 if morales==1
*gen sanchez0=0
*replace sanchez0=1 if sanchez==1

*gen vtot0=segob0+senpri0+senprd0+dippri0+dippan0+creel0+granados0+woldenberg0+zertuche0+pinchetti0+pozas0 if term==1
*replace vtot0=woldenberg0+peschard0+lujambio0+cardenas0+cantu0+merino0+barragan0+zebadua0+molinar0 if term==2
*replace vtot0=woldenberg0+peschard0+lujambio0+cardenas0+cantu0+merino0+barragan0+rivera0+luken0 if term==3

* FALTA adaptar vots.do para valdes banos nacif elizondo figueroa guerrero etc
do vots

*esto hace histogramas de votos aye-nay para cada consejo
*do aynay

*this variable considers only ayes and nays, not abstentions
gen twoplusvsmaj=1
replace twoplusvsmaj=0 if unanime==1
replace twoplusvsmaj=0 if nays<2
replace twoplusvsmaj=0 if ayes<2

*agregados por term
sort term
by term: egen vtott=count(result)
by term: egen unant=sum(unanime)
gen contest=vtott-unant
by term: egen pandivt=sum(pandiv)
by term: egen pridivt=sum(pridiv)
by term: egen prddivt=sum(prddiv)
by term: egen twoplusdivt=sum(twoplusdiv)

sort yrsm

by yrsm: egen votsm=count(result)
by yrsm: egen unansm=sum(unanime)
gen empbase=votsm-unansm
gen unanpctsm=unansm/votsm
by yrsm: egen twoplusvsmajsm=sum(twoplusvsmaj)
by yrsm: egen pandivsm=sum(pandiv)

by yrsm: egen pridivsm=sum(pridiv)
by yrsm: egen prddivsm=sum(prddiv)
gen pandivrelsm=pandivsm/empbase
gen pridivrelsm=pridivsm/empbase
gen prddivrelsm=prddivsm/empbase
by yrsm: egen twoplusdivsm=sum(twoplusdiv)
by yrsm: egen naysayersm=sum(naysayer)
by yrsm: egen pripan_v_prdsm=sum(pripan_v_prd)
by yrsm: egen priprd_v_pansm=sum(priprd_v_pan)
by yrsm: egen panprd_v_prism=sum(panprd_v_pri)
by yrsm: egen poli_v_abogsm=sum(poli_v_abog)
replace poli_v_abogsm=. if term==1
replace poli_v_abogsm=. if term==4
by yrsm: egen non_rollingsm=sum(non_rolling)
*replace non_rollingsm=. if term==4

*gr7 votsm empbase yrsm, s(..) c(ll) xlab(1994[1]2004) xtick(1994.5[1]2003.5) b2(Semester) l1(Number of roll-call votes) ylab ytick(10[10]160) t1("Top line - all votes;  bottom line - contested votes") xline(1996.25 2000.75 2003.75) saving(graph1, replace)

*en cuanto complete la codificación de la variable pandiv podré quitar el gen-drop y hacer una gráfica para todo el período
gen tmpvar=pandivrelsm
replace tmpvar=. if term==1 | term==4
*gr7 tmpvar yrsm, s([pandivsm]) c(l) xlab(1994[1]2004) xtick(1994.5[1]2003.5)  ylab b2(Semester) l1("Share of semester's contested divisions")  xline(1996.25 2000.75 2003.75) t1("Divisionss where Lujambio voted different from Molinar/Luken") t2("(Number of votes appears on the line)") saving(tmp02, replace)
drop tmpvar

gen tmpvar=pridivrelsm
replace tmpvar=. if term==1 | term==4
*gr7 tmpvar yrsm, s([pridivsm]) c(l) xlab(1994[1]2004) xtick(1994.5[1]2003.5)  ylab b2(Semester) l1("Share of semester's contested divisions")  xline(1996.25 2000.75 2003.75) t1("Divisionss where Merino voted different from Peschard") t2("(Number of votes appears on the line)") saving(tmp03, replace)
drop tmpvar

gen tmpvar=prddivrelsm
replace tmpvar=. if term==1 | term==4
*gr7 tmpvar yrsm, s([prddivsm]) c(l) xlab(1994[1]2004) xtick(1994.5[1]2003.5)  ylab b2(Semester) l1("Share of semester's contested divisions")  xline(1996.25 2000.75 2003.75) t1("Divisions where Cantú voted different from Zebadúa") t2("(Number of votes appears on the line)") saving(tmp04, replace)
drop tmpvar

gen tmpvar=naysayersm
replace tmpvar=. if term==4
*gr7 tmpvar empbase yrsm, s(..) c(ll) xlab(1994[1]2004) xtick(1994.5[1]2003.5) b2(Semester) l1(Number of divisions) ylab ytick(10[10]100) t1("Top line - all contested divisions") t2("bottom line - only naysayers (PML/Cárdenas/Barragán) contested") xline(1996.25 2000.75 2003.75) saving(tmp05, replace)
drop tmpvar

gen tmpvar=pripan_v_prdsm
replace tmpvar=. if term==4
*gr7 tmpvar empbase yrsm, s(..) c(ll) xlab(1994[1]2004) xtick(1994.5[1]2003.5) b2(Semester) l1(Number of roll-call votes) ylab(0[10]60) ytick(0[5]60) t1("Top line - all contested votes;  bottom line - PRD was rolled") xline(1996.25 2000.75 2003.75) saving(graph6, replace)
drop tmpvar

gen tmpvar=priprd_v_pansm
replace tmpvar=. if term==4
*gr7 tmpvar empbase yrsm, s(..) c(ll) xlab(1994[1]2004) xtick(1994.5[1]2003.5) b2(Semester) l1(Number of roll-call votes) ylab(0[10]60) ytick(0[5]60) t1("Top line - all contested votes;  bottom line - PAN was rolled") xline(1996.25 2000.75 2003.75) saving(graph4, replace)
drop tmpvar

gen tmpvar=panprd_v_prism
replace tmpvar=. if term==4
*gr7 tmpvar empbase yrsm, s(..) c(ll) xlab(1994[1]2004) xtick(1994.5[1]2003.5) b2(Semester) l1(Number of roll-call votes) ylab(0[10]60) ytick(0[5]60) t1("Top line - all contested votes;  bottom line - PRI was rolled") xline(1996.25 2000.75 2003.75) saving(graph5, replace)
drop tmpvar

*gr7 poli_v_abogsm empbase yrsm, s(..) c(ll) xlab(1994[1]2004) xtick(1994.5[1]2003.5) b2(Semester) l1(Number of divisions) ylab ytick(10[10]100) t1("Top line - contested divisions;  bottom - PoliSci v. Law School") xline(1996.25 2000.75 2003.75) saving(tmp09, replace)

*gr7 empbase non_rollingsm yrsm, s(..) c(ll) xlab(1994[1]2004) xtick(1994.5[1]2003.5) b2(Semester) l1(Number of roll-call votes) ylab(0[10]60) ytick(0[5]60) t1("Top line - contested votes;  bottom - contested but non-rolling") xline(1996.25 2000.75 2003.75) saving(graph3, replace)

gen rollingsm=empbase-non_rollingsm
*gr7 votsm empbase rollingsm yrsm if term>1, s(...) c(lll) xlab(1997[1]2005) xtick(1996.5[1]2004.5) b2(Semester) l1(Number of roll-call votes) ylab(0[50]150) ytick(0[10]160) t1("Gap between top & middle lines - unanimity; between middle") t2("& bottom - tripartite consensus; below bottom - majority rolls") xline(2000.75 2003.75)
*line votsm empbase rollingsm yrsm if term>1, s(...) xlab(1997[1]2005) xtick(1996.5[1]2004.5) xtitle("Semester") ytitle("Number of roll-call votes") ylab(0[50]150) ytick(0[10]160) xline(2000.75 2003.75) scheme(s1mono) legend(pos(6) col(3) lab(1 "all votes") lab(2 "contested votes") lab(3 "majority rolls")) note("Note: vertical bars indicate change in Council membership")

*replace t = yrsm*2 - 3993
*replace t = yrtrim*4-7987
*gen elQtr = t==2 | t==3 | t==4 | t==14 | t==15 | t==16 | t==26 | t==27 | t==28 | t==38  | t==39  | t==40  | t==50  | t==51  | t==52  | t==62  | t==63  | t==64


*********************************************
* EXPORTA LOS VOTOS PARA ANALISIS EN R/BUGS *
*********************************************
*preserve
recode woldenberg segob senpri senprd dippri dippan creel granados zertuche pinchetti pozas barragan cantu cardenas lujambio merino molinar peschard zebadua rivera luken ugalde albo andrade alcantar glezluna latapi lopezflores morales sanchez valdes banos nacif elizondo figueroa guerrero marvan cordova garcia (0 = -1) (. = 0)
*gen t = yr - 1993 /* t = 1 en 1994 */
*replace t = 4 if yr==1996 & term == 2 /* pone estos meses en arranque consejo Wold1 */
*replace t = 8 if yr==2000 & term == 3 /* pone estos meses en arranque consejo Wold2 */
gen t = yrsm*2 - 3993 if term==2 /* t = 1 en 1997 */
replace t = yrsm*2 - 4000 if term==3 /* t = 1 en 2000.5 */
replace t = t-1 if term==3 /* No hay votos en 1er semestre */
replace t = yrsm*2 - 4007 if term==4 /* t = 1 en 2004 */
replace t = 1 if yrsm==float(2003.5) & term==4 & unanime==0 /* pone el único voto dividido de 2003.5 en 2004 */
replace t = 1 if term==5 /* t = 1 en 2008 */
replace t = yrsm*2 - 4015 if term==6 /* t = 1 en 2008 */
replace t = yrsm*2 - 4017 if term==7 /* t = 1 en 2008.5 */
replace t = yrsm*2 - 4021 if term==8 /* t = 1 en 2011 */
sort term yr mo dy folio
outsheet woldenberg segob senpri senprd dippri dippan creel granados zertuche pinchetti pozas folio yr mo dy t yrsm yrtrim term using tmp1.csv if unanime==0 & term==1, comma replace
*outsheet woldenberg barragan cantu cardenas lujambio merino molinar peschard zebadua          folio yr mo dy t yrsm yrtrim term using tmp2.csv if unanime==0 & term==2, comma replace
*outsheet woldenberg barragan cantu cardenas lujambio merino peschard rivera luken             folio yr mo dy t yrsm yrtrim term using tmp3.csv if unanime==0 & term==3, comma replace
*outsheet ugalde albo andrade alcantar glezluna latapi lopezflores morales sanchez             folio yr mo dy t yrsm yrtrim term using tmp4.csv if unanime==0 & term==4, comma replace
*outsheet albo andrade alcantar glezluna latapi lopezflores morales sanchez                    folio yr mo dy t yrsm yrtrim term using tmp5.csv if unanime==0 & term==5, comma replace
*outsheet valdes albo andrade alcantar banos glezluna lopezflores nacif sanchez                folio yr mo dy t yrsm yrtrim term using tmp6.csv if unanime==0 & term==6, comma replace
*outsheet valdes andrade alcantar banos elizondo figueroa guerrero nacif sanchez               folio yr mo dy t yrsm yrtrim term using tmp7.csv if unanime==0 & term==7, comma replace
*outsheet valdes banos elizondo figueroa guerrero nacif                                        folio yr mo dy t yrsm yrtrim term using tmp8.csv if unanime==0 & term==8, comma replace
*drop t /* se necesita un t fresco para terms 23 juntos */
replace t = yrsm*2 - 3993 if term==2 | term==3
outsheet woldenberg barragan cantu cardenas lujambio merino molinar peschard zebadua rivera luken folio yr mo dy t yrsm yrtrim term using tmp23.csv if unanime==0 & (term==2 | term==3), comma replace
*drop t /* se necesita un t fresco para terms 45678 juntos */
replace t = yrsm*2 - 4007 if term==4 | term==5 | term==6 | term==7 | term==8 | term==9
outsheet ugalde albo andrade alcantar glezluna latapi lopezflores morales sanchez valdes banos nacif elizondo figueroa guerrero folio yr mo dy t yrsm yrtrim term using tmp4567.csv if unanime==0 & (term==4 | term==5 | term==6 | term==7), comma replace
outsheet ugalde albo andrade alcantar glezluna latapi lopezflores morales sanchez valdes banos nacif elizondo figueroa guerrero folio yr mo dy t yrsm yrtrim term using tmp45678.csv if unanime==0 & (term==4 | term==5 | term==6 | term==7 | term==8), comma replace
outsheet ugalde albo andrade alcantar glezluna latapi lopezflores morales sanchez valdes banos nacif elizondo figueroa guerrero marvan cordova garcia folio yr mo dy t yrsm yrtrim term using tmp456789.csv if unanime==0 & (term==4 | term==5 | term==6 | term==7 | term==8 | term==9), comma replace
*restore

xx


line votsm empbase twoplusvsmajsm yrsm if term>1, s(...) xlab(1997[1]2006) xtick(1996.5[1]2006.5) yscale(range(0 200)) xtitle("Semester") ytitle("Number of roll-call votes") ylab(0[50]200) ytick(0[10]190) xline(2000.75 2003.75) scheme(s1mono) legend(pos(6) col(3) lab(1 "all votes") lab(2 "contested votes") lab(3 "2+ against majority")) note("Note: vertical bars indicate change in Council membership.") text(198 1998.5 "Woldenberg I") text(198 2002.25 "Woldenberg II") text(198 2005.25 "Ugalde") saving(fig1, replace)


xx
gen panrollsm=priprd_v_pansm
gen prirollsm=panrollsm+ panprd_v_prism
gen prdrollsm=prirollsm+ pripan_v_prdsm
replace panrollsm=. if term==4
replace prdrollsm=. if term==4
replace prirollsm=. if term==4
gen panroll=0
replace panroll=1 if vfaccpan>2 & result==0 & term==4
replace panroll=1 if vfaccpan<2 & result==1 & term==4
gen priroll=0
replace priroll=1 if vfaccpri>2 &result==0 & term==4
replace priroll=1 if vfaccpri<2 &result==1 & term==4
by yrsm: egen tmp1=sum(panroll)
by yrsm: egen tmp2=sum(priroll)
replace panrollsm=tmp1 if term==4 & yr>=2004
replace prirollsm=panrollsm+tmp2 if term==4 & yr>=2004
gr7 prirollsm prdrollsm panrollsm yrsm if term>1, s(...) c(lll) xlab(1997[1]2005) xtick(1996.5[1]2004.5) b2(Semester) l1(Number of roll-call votes) ylab(0[10]80) ytick(0[5]75) t1("Gap between top & middle lines - PRD rolled; between") t2("middle & bottom - PRI rolled; below bottom - PAN rolled") xline(2000.75 2003.75) pen(326)
line prirollsm prdrollsm panrollsm yrsm if term>1, s(...) xlab(1997[1]2005) xtick(1996.5[1]2004.5) xtitle(Semester) ytitle(Number of roll-call votes) ylab(0[10]70) ytick(0[5]65) xline(2000.75 2003.75) scheme(s1mono) legend(off) note("Note: PRD had no representation in Council General from 2004 onwards;" "vertical lines indicate change in Council membership") t2("Gap between top & middle lines - PRD rolled;" "between middle & bottom - PRI rolled;" "below bottom - PAN rolled")
*drop panrollsm prirollsm prdrollsm

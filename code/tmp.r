


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




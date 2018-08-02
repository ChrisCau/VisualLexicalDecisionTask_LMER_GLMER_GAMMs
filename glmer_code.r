require(rms)
require(lme4)
require(languageR)
require(multcomp)
require(lsmeans)
require(multcompView)
require(pbkrtest)
require(lmerTest)
require(randomForest)
require(party)
require(MASS)
require(ggplot2)
library(car)
require(itsadug)

dat=read.table("vldfinalno.txt",, sep="\t",header=TRUE)

dim(dat)
#[1] 2023   18

colnames(dat)

# [1] "Ispitanik"            "TrialOrder"           "Grupa"               
# [4] "CorrectResponse"      "TrialNumber"          "Sufiks"              
# [7] "DuzinaSufiksa"        "Response"             "Tacnost"             
#[10] "RT"                   "NV"                   "Stimulus"            
#[13] "FrekLemKostic"        "PovFrekWac"           "FrekLemWac"          
#[16] "DuzinaReci"           "FrekvencijaSufiksa"   "ProduktivnostSufiksa"

_____________________________________________
__________________________________________________________

# Vizulena inspekcija podataka, grafik VIP1

par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))

powerTransform(dat$RT)
#Estimated transformation parameters 
#   dat$RT 
#-1.314563  

# Log frekvencija leme, frekvencija sufiksa, duzina sufiksa i duzina leme

dat$dlem=log(dat$DuzinaReci)
dat$flemk=log(dat$FrekLemKostic)
dat$flemw=log(dat$FrekLemWac)
dat$fpov=log(dat$PovFrekWac)
dat$fsuf=log(dat$FrekvencijaSufiksa)
dat$dsuf=log(dat$DuzinaSufiksa)
dat$psuf=log(dat$ProduktivnostSufiksa)

# Normalizujem kontinuirane prediktore 

dat$trial.z = scale(dat$TrialOrder)
dat$len.z = scale(dat$dlem)
dat$flemk.z=scale(dat$flemk)
dat$flemw.z=scale(dat$flemw)
dat$fpov.z=scale(dat$fpov)
dat$fsuf.z = scale(dat$fsuf)
dat$dsuf.z = scale(dat$dsuf)
dat$psuf.z = scale(dat$psuf)

# Pobrinem se da je faktor tretiran kao faktor

as.factor(as.character(dat$NV))
levels(dat$NV)
table(dat$NV)

as.factor(as.character(dat$Ispitanik))
levels(dat$Ispitanik)
table(dat$Ispitanik)

as.factor(as.character(dat$Stimulus))
levels(dat$Stimulus)
table(dat$Stimulus)
__________________________________________________________
________________________________________________________________

#Kontinuirane prediktore -- da vidim kako vizuelno to sve izgleda

-------------------------
#Trial Order
-------------------------

par(mfrow=c(2,2))
plot(sort(dat$TrialOrder))
plot(density(dat$TrialOrder))
qqnorm(dat$TrialOrder)
par(mfrow=c(1,1))

-------------------------
#Duzina Reci
-------------------------

par(mfrow=c(2,2))
plot(sort(dat$dlem))
plot(density(dat$dlem))
qqnorm(dat$dlem)
par(mfrow=c(1,1))

-------------------------
#Frekvencija Leme (Kostic)
-------------------------

par(mfrow=c(2,2))
plot(sort(dat$flemk))
plot(density(dat$flemk))
qqnorm(dat$flemk)
par(mfrow=c(1,1))

-------------------------
#Frekvencija Leme (srWac)
-------------------------

par(mfrow=c(2,2))
plot(sort(dat$flemw))
plot(density(dat$flemw))
qqnorm(dat$flemw)
par(mfrow=c(1,1))

-------------------------
#Frekvencija Reci (srWac)
-------------------------

par(mfrow=c(2,2))
plot(sort(dat$fpov))
plot(density(dat$fpov))
qqnorm(dat$fpov)
par(mfrow=c(1,1))

-------------------------
#Frekvencija Sufiksa 
-------------------------

par(mfrow=c(2,2))
plot(sort(dat$fsuf))
plot(density(dat$fsuf))
qqnorm(dat$fsuf)
par(mfrow=c(1,1))

-------------------------
#Duzina Sufiksa 
-------------------------

par(mfrow=c(2,2))
plot(sort(dat$dsuf))
plot(density(dat$dsuf))
qqnorm(dat$dsuf)
par(mfrow=c(1,1))

-------------------------
#Produktivnost Sufiksa 
-------------------------

par(mfrow=c(2,2))
plot(sort(dat$psuf))
plot(density(dat$psuf))
qqnorm(dat$psuf)
par(mfrow=c(1,1))

__________________________________________________________
____________________________________________________________

# Vizuelna inspekcija slucajnih efekata

qqmath(~RT|Ispitanik,data=dat)

qqmath(~RT|TrialNumber,data=dat)

xylowess.fnc (RT~TrialOrder | Ispitanik, data=dat, ylab= "RT")
____________________________________________________________
____________________________________________________________

table(dat$FrekvencijaSufiksa)

   46    73    81    88    91   170   424   464   537   868   913  2140  2280 
   21    41    21    23    24    38    43    24    46   217    24    92    68 
 3001  3200  3764  3765  5345  6155  6585 23567 
  181    40    91    24   173   129   224    89
  _________________________________________________________
________________________________________________________________

# Kolinearnost medju prediktorima

C=cov(dat[,c("RT", "flemk","flemw", "fpov", "fsuf", "dlem","dsuf", "psuf")], y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
Cor=cov2cor(C)
Cor

#              RT       flemk        flemw        fpov       fsuf        dlem
#RT     1.0000000 -0.25270268 -0.340134744 -0.33576441 -0.2088414  0.11718241
#flemk -0.2527027  1.00000000  0.737172303  0.81199371  0.4640850 -0.05401538
#flemw -0.3401347  0.73717230  1.000000000  0.92810403  0.5037532  0.02816801
#fpov  -0.3357644  0.81199371  0.928104033  1.00000000  0.4884931  0.01208878
#fsuf  -0.2088414  0.46408501  0.503753210  0.48849305  1.0000000 -0.16956594
#dlem   0.1171824 -0.05401538  0.028168006  0.01208878 -0.1695659  1.00000000
#dsuf   0.1101909 -0.06037767  0.008320418 -0.05125295 -0.3250745  0.64594653
#psuf  -0.1763057  0.36969063  0.394772138  0.41680563  0.9190694 -0.11839922
#              dsuf       psuf
#RT     0.110190853 -0.1763057
#flemk -0.060377670  0.3696906
#flemw  0.008320418  0.3947721
#fpov  -0.051252949  0.4168056
#fsuf  -0.325074463  0.9190694
#dlem   0.645946526 -0.1183992
#dsuf   1.000000000 -0.3125367
#psuf  -0.312536700  1.0000000

collin.fnc(dat[,c("flemk","flemw", "fpov", "fsuf", "dlem","dsuf", "psuf")])$cnumber

#Naravno, prevelik zbog ovih frekvencija.

collin.fnc(dat[,c("flemk","fsuf", "dlem")])$cnumber
#28.65964

_____________________________________________
____________________________________________________________

################################################################# GLMER MODEL BROJ 1: FREKVENCIJA LEME (KOSTIC)

########## ########## Ako pamtim dobro dogovor, rekla si mi samo da uzmem onaj lmer model.

----------------------------
---------------------------------------------
---------------------------------------------------------

########################## LMER KONACNI #######
################### SA NJE #################

glmer.dat6 <- glmer(Tacnost ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat, family="binomial")
summary (glmer.dat6)

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 2.01336  1.4189  
# Ispitanik   len.z       0.02806  0.1675  
# Ispitanik.1 (Intercept) 0.22182  0.4710  
#Number of obs: 2023, groups:  Stimulus, 88; Ispitanik, 46
#
#Fixed effects:
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            3.7379     0.3923   9.528  < 2e-16 ***
#poly(TrialOrder, 2)1   9.6344     4.1242   2.336  0.01949 *  
#poly(TrialOrder, 2)2   0.7953     4.1678   0.191  0.84866    
#len.z                  0.2887     0.2091   1.380  0.16748    
#fsuf.z                 0.7625     0.2792   2.731  0.00632 ** 
#NVvise                 2.0246     1.0345   1.957  0.05033 .  
#flemk.z                1.0501     0.2493   4.213 2.52e-05 ***
#fsuf.z:NVvise         -4.0227     1.3319  -3.020  0.00253 ** 


---------------------
dat$NV <- relevel(dat$NV, ref = "jedno")

glmer.dat6 <- glmer(Tacnost ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat, family="binomial")
summary (glmer.dat6)

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 2.01336  1.4189  
# Ispitanik   len.z       0.02806  0.1675  
# Ispitanik.1 (Intercept) 0.22182  0.4710  
#Number of obs: 2023, groups:  Stimulus, 88; Ispitanik, 46
#
#Fixed effects:
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            3.7379     0.3923   9.528  < 2e-16 ***
#poly(TrialOrder, 2)1   9.6344     4.1242   2.336  0.01949 *  
#poly(TrialOrder, 2)2   0.7953     4.1678   0.191  0.84866    
#len.z                  0.2887     0.2091   1.380  0.16748    
#fsuf.z                 0.7625     0.2792   2.731  0.00632 ** 
#NVvise                 2.0246     1.0345   1.957  0.05033 .  
#flemk.z                1.0501     0.2493   4.213 2.52e-05 ***
#fsuf.z:NVvise         -4.0227     1.3319  -3.020  0.00253 ** 

________________________________________________________
__________________________________________________________________________________
_________________________________________________________________________________________________________

################################################################# MODEL BROJ 2: FREKVENCIJA LEME (SRWAC)

----------------------------
---------------------------------------------
---------------------------------------------------------

########################## TRENUTNO NAJBOLJI #######
################### SA NJE #################

glmer.dat3 <- glmer(Tacnost ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat, family="binomial")
summary (glmer.dat3)

#Random effects:
# Groups      Name        Variance Std.Dev.
#Stimulus    (Intercept) 1.69397  1.3015  
# Ispitanik   len.z       0.03042  0.1744  
# Ispitanik.1 (Intercept) 0.22666  0.4761  
#Number of obs: 2023, groups:  Stimulus, 88; Ispitanik, 46
#
#Fixed effects:
#                      Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           3.447492   0.349754   9.857  < 2e-16 ***
#poly(TrialOrder, 2)1 10.408652   4.135174   2.517   0.0118 *  
#poly(TrialOrder, 2)2  1.126053   4.171276   0.270   0.7872    
#len.z                 0.185551   0.194708   0.953   0.3406    
#fsuf.z                0.146900   0.252459   0.582   0.5606    
#flemw.z               1.366663   0.217854   6.273 3.53e-10 ***
#NVvise               -0.003134   0.512793  -0.006   0.9951    

_____________________________________________
____________________________________________________________
__________________________________________________________________________________

################################################################# MODEL BROJ 3: FREKVENCIJA POVRSINSKA (SRWAC)

glmer.dat6 <- glmer(Tacnost ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat, family="binomial")
summary (glmer.dat6)

#Random effects:
# Groups      Name        Variance  Std.Dev.
# Stimulus    (Intercept) 1.447e+00 1.20310 
# Ispitanik   len.z       6.789e-05 0.00824 
# Ispitanik.1 (Intercept) 2.228e-01 0.47207 
#Number of obs: 2023, groups:  Stimulus, 88; Ispitanik, 46
#
#Fixed effects:
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           3.42252    0.33428  10.238  < 2e-16 ***
#poly(TrialOrder, 2)1 10.17160    4.11198   2.474   0.0134 *  
#poly(TrialOrder, 2)2  1.07198    4.13636   0.259   0.7955    
#len.z                 0.21659    0.18052   1.200   0.2302    
#fsuf.z                0.17085    0.24129   0.708   0.4789    
#fpov.z                1.40548    0.20808   6.755 1.43e-11 ***
#NVvise               -0.02574    0.49405  -0.052   0.9585     

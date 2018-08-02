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
require(mgcv)

dat=read.table("vldfinalno.txt",T)

dim(dat)
#[1] 2023   18

colnames(dat)

# [1] "Ispitanik"            "TrialOrder"           "Grupa"               
# [4] "CorrectResponse"      "TrialNumber"          "Sufiks"              
# [7] "DuzinaSufiksa"        "Response"             "Tacnost"             
#[10] "RT"                   "NV"                   "Stimulus"            
#[13] "FrekLemKostic"        "PovFrekWac"           "FrekLemWac"          
#[16] "DuzinaReci"           "FrekvencijaSufiksa"   "ProduktivnostSufiksa"
 
sort(tapply(dat$Tacnost, dat$Ispitanik, sum)/44)

#      s28       s31       s40       s22       s33       s29       s32       s39 
#0.7954545 0.7954545 0.7954545 0.8181818 0.8181818 0.8409091 0.8409091 0.8409091 
#      s41       s42        s5        s6       s23       s26       s37       s11 
#0.8409091 0.8409091 0.8409091 0.8409091 0.8636364 0.8636364 0.8636364 0.8863636 
#      s12       s16       s17       s18       s35       s36        s4       s46 
#0.8863636 0.8863636 0.8863636 0.8863636 0.8863636 0.8863636 0.8863636 0.8863636 
#       s7        s1       s19       s27       s44        s3       s30       s43 
#0.8863636 0.9090909 0.9090909 0.9090909 0.9090909 0.9318182 0.9318182 0.9318182 
#      s45        s8       s10       s13       s14       s15        s2       s21 
#0.9318182 0.9318182 0.9545455 0.9545455 0.9545455 0.9545455 0.9545455 0.9545455 
#      s24       s25       s34       s38       s20        s9 
#0.9545455 0.9545455 0.9545455 0.9545455 0.9772727 0.9772727

sort(tapply(dat$Tacnost, dat$TrialNumber, sum)/23)

#      100       109        17       126        37        23        98       115 
#0.1304348 0.1739130 0.3913043 0.4347826 0.4782609 0.5652174 0.6521739 0.6521739 
#       27       117       124       129        96       130        25        35 
#0.6956522 0.6956522 0.6956522 0.6956522 0.7391304 0.7391304 0.7826087 0.7826087 
#       38       103       105       119        10       116         6        91 
#0.7826087 0.7826087 0.7826087 0.7826087 0.8260870 0.8260870 0.8695652 0.8695652 
#       93        19        94        99       106       112       113       114 
#0.8695652 0.9130435 0.9130435 0.9130435 0.9130435 0.9130435 0.9130435 0.9130435 
#      122       123       128       131         2         7        21        89 
#0.9130435 0.9130435 0.9130435 0.9130435 0.9565217 0.9565217 0.9565217 0.9565217 
#       90        92        95        97       101       102       104       107 
#0.9565217 0.9565217 0.9565217 0.9565217 0.9565217 0.9565217 0.9565217 0.9565217 
#      108       110       111       118       120       121       125       127 
#0.9565217 0.9565217 0.9565217 0.9565217 0.9565217 0.9565217 0.9565217 0.9565217 
#      132         1         5         9        13        24        30        31 
#0.9565217 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 
#       34        40        44         3         4         8        11        12 
#1.0000000 1.0000000 1.0000000 1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 
#       14        15        16        18        20        22        26        28 
#1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 
#       29        32        33        36        39        41        42        43 
#1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 1.0434783 

#Izbacivanje stimulusa sa vise od 25% gresaka

dat=dat[dat$TrialNumber!="100",]
dat=dat[dat$TrialNumber!="109",]
dat=dat[dat$TrialNumber!="17",]
dat=dat[dat$TrialNumber!="126",]
dat=dat[dat$TrialNumber!="37",]
dat=dat[dat$TrialNumber!="23",]
dat=dat[dat$TrialNumber!="98",]
dat=dat[dat$TrialNumber!="115",]
dat=dat[dat$TrialNumber!="27",]
dat=dat[dat$TrialNumber!="117",]
dat=dat[dat$TrialNumber!="124",]
dat=dat[dat$TrialNumber!="129",]
dat=dat[dat$TrialNumber!="96",]
dat=dat[dat$TrialNumber!="130",]

dim(dat)
#[1] 1707   18

(2023-1707)/2023
#[1] 0.1562037

-----------------------------------------

#Ispitujemo po kojim kovarijablama se grupe razlikuju. Uzmemo dva ispitanika,
#i napravimo kao neki maleni podskup, i na njemu radimo dalje.

datx=dat[dat$Ispitanik=="s9" | dat$Ispitanik=="s38",]

dim(datx)
#74 16

----------------------
#Duzina reci
----------------------

aov.datx= aov(DuzinaReci~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(DuzinaReci~NV, data=datx)

#            Df Sum Sq Mean Sq F value Pr(>F)  
#NV           1   8.56   8.564   4.088 0.0469 *
#Residuals   72 150.84   2.095                 
        
#    jedno  vise
#     7.76  7.07
#    33.00 41.00

-------------------------
#Frekvencija leme (Kostic)
----------------------

aov.datx= aov(FrekLemKostic~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(FrekLemKostic~NV, data=datx)

#            Df Sum Sq Mean Sq F value Pr(>F)  
#NV           1  27795   27795   6.333 0.0141 *
#Residuals   72 316011    4389   

#    jedno vise
#     15.2 54.2
#     33.0 41.0

-------------------------
#Frekvencija leme (srWac)
----------------------
aov.datx= aov(FrekLemWac~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(FrekLemWac~NV, data=datx)

#            Df    Sum Sq   Mean Sq F value Pr(>F)  
#NV           1 7.865e+08 786503093   3.235 0.0763 .
#Residuals   72 1.750e+10 243114598 

#    jedno  vise
#     4375 10934
#      33    41

-------------------------
#Frekvencija povrsinska (srWac)
----------------------
aov.datx= aov(PovFrekWac~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(PovFrekWac~NV, data=datx)

#            Df    Sum Sq  Mean Sq F value Pr(>F)  
#NV           1 9.317e+07 93169066   2.936 0.0909 .
#Residuals   72 2.285e+09 31733474

#    jedno vise
#     1103 3360
#      33   41

-------------------------
#Frekvencija sufiksa
-------------------------
aov.datx= aov(FrekvencijaSufiksa~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(FrekvencijaSufiksa~NV, data=datx)

#            Df    Sum Sq  Mean Sq F value Pr(>F)
#NV           1 7.809e+06  7808535   0.288  0.593
#Residuals   72 1.950e+09 27089474  
#
#    jedno vise
#     3925 4578
#      33   41

-------------------------
#Duzina sufiksa
-------------------------

aov.datx= aov(DuzinaSufiksasa~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(DuzinaSufiksa~NV, data=datx)

#            Df   Sum Sq Mean Sq F value Pr(>F)  
#NV           1   593524  593524   2.905 0.0926 .
#Residuals   72 14708156  204280    
#
#    jedno vise
#     377  197
#      33   41

-------------------------
#Produktivnost sufiksa
-------------------------

aov.datx= aov(ProduktivnostSufiksa~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(ProduktivnostSufiksa~NV, data=datx)

#            Df   Sum Sq Mean Sq F value Pr(>F)  
#NV           1   593524  593524   2.905 0.0926 .
#Residuals   72 14708156  204280

#    jedno vise
#      377  197
#      33   41
_____________________________________________
__________________________________________________________

# Izbacivanje RT-ova

dat=dat[dat$Tacnost>0,]

dim(dat)
#[1] 1633   18

(2023-1633)/2023
#[1] 0.192783

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

# Inverzna transformacija RT

dat$RT=-1000/dat$RT

#  Vizulena inspekcija podataka, grafik VIP2

par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))

# Normalnost distribucije RT-ova sada testiram

shapiro.test(dat$RT)

#        Shapiro-Wilk normality test
#
#data:  dat$RT
#W = 0.99274, p-value = 3.377e-07

ks.test(jitter(dat$RT),"pnorm",mean(dat$RT),sd(dat$RT))

#        One-sample Kolmogorov-Smirnov test
#
#data:  jitter(dat$RT)
#D = 0.041915, p-value = 0.006443
#alternative hypothesis: two-sided

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

#jedno  vise 
#  722   911

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

#               RT       flemk       flemw         fpov        fsuf        dlem
#RT     1.00000000 -0.24192511 -0.26672498 -0.266128070 -0.09049384  0.13293073
#flemk -0.24192511  1.00000000  0.73042944  0.797089933  0.46913614 -0.10799560
#flemw -0.26672498  0.73042944  1.00000000  0.917228827  0.46238265 -0.02192262
#fpov  -0.26612807  0.79708993  0.91722883  1.000000000  0.44738896 -0.02269643
#fsuf  -0.09049384  0.46913614  0.46238265  0.447388956  1.00000000 -0.11759259
#dlem   0.13293073 -0.10799560 -0.02192262 -0.022696432 -0.11759259  1.00000000
#dsuf   0.07142855 -0.03491216  0.01930949  0.007315201 -0.27074274  0.62876318
#psuf  -0.05949093  0.35513434  0.34034109  0.362376974  0.91055550 -0.02878838
#              dsuf        psuf
#RT     0.071428554 -0.05949093
#flemk -0.034912158  0.35513434
#flemw  0.019309491  0.34034109
#fpov   0.007315201  0.36237697
#fsuf  -0.270742736  0.91055550
#dlem   0.628763177 -0.02878838
#dsuf   1.000000000 -0.20600849
#psuf  -0.206008491  1.00000000

collin.fnc(dat[,c("flemk","flemw", "fpov", "fsuf", "dlem","dsuf", "psuf")])$cnumber

#Naravno, prevelik zbog ovih frekvencija.
#55.79542

collin.fnc(dat[,c("flemk","fsuf", "dlem")])$cnumber
#28.91624

#Kada se izbace frekvencija sufiksa, i produktivnost sufiksa onda je bolje mnogo, dakle ovde ove kovarijable vec otpadaju.

postscript("lanister.pairscor1.ps", width=16, height=16,paper="special",horizontal=FALSE,onefile=FALSE)
png("lanister.pairscor1.png", width=800, height=800)
pairscor.fnc(dat[,c("RT", "NV","flemk","fsuf", "dlem", "flemw", "fpov")], hist=TRUE, smooth=TRUE, cex.point=1, col.points="darkgrey")
dev.off()
_____________________________________________
____________________________________________________________

################################################################# MODEL BROJ 1: FREKVENCIJA LEME (KOSTIC)

########## SLUCAJNI EFEKTI

lmer0 <- lmer(RT ~ 1 + (1|Ispitanik) + (1|Stimulus),	data=dat)
ranefItem <- lmer(RT ~ 1 + (1|Stimulus),	data=dat)
ranefSubj <- lmer(RT ~ 1 + (1|Ispitanik), data=dat)

anova(ranefItem, lmer0)

#object: RT ~ 1 + (1 | Stimulus)
#..1: RT ~ 1 + (1 | Ispitanik) + (1 | Stimulus)
#       Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  3 1043.87 1060.06 -518.93  1037.87                             
#..1     4  502.38  523.98 -247.19   494.38 543.48      1  < 2.2e-16 ***

#Bolji je drugi model (AIC i BIC sto manji, LogLik sto veci)

anova(ranefSubj, lmer0)

#object: RT ~ 1 + (1 | Ispitanik)
#..1: RT ~ 1 + (1 | Ispitanik) + (1 | Stimulus)
#       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  3 730.20 746.39 -362.10   724.20                             
#..1     4 502.38 523.98 -247.19   494.38 229.81      1  < 2.2e-16 ***

#Opet je bolji drugi
__________________________________________________________
________________________________________________________________

############# TRIAL ORDER 

lmer.dat <- lmer(RT ~ trial.z + (1|Ispitanik) + (1|Stimulus),	data=dat)
summary (lmer.dat)

#              Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept) -1.411e+00  3.303e-02  6.970e+01 -42.712   <2e-16 ***
#trial.z     -2.744e-03  6.509e-03  1.528e+03  -0.422    0.673      

# Nije znacajan

lmer.dat1 <- lmer(RT ~ poly(TrialOrder, 2) + (1|Ispitanik) + (1|Stimulus), data=dat)
summary (lmer.dat1)

lmer.dat.a=update(lmer.dat, REML=FALSE)
lmer.dat1.a=update(lmer.dat1, REML=FALSE)

anova(lmer.dat.a,lmer.dat1.a)

#Bolji sa poly ... 

------------------------

################ POLAKO UBACUJEM JEDAN PO JEDAN

lmer.dat2 <- lmer(RT ~ poly(TrialOrder,2) + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat2)

lmer.dat3 <- lmer(RT ~ poly(TrialOrder,2) + len.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat3)

lmer.dat4 <- lmer(RT ~ poly(TrialOrder,2) + len.z + flemk.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat4)

lmer.dat5 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemk.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat5)

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.418e+00  3.605e-02  8.620e+01 -39.326  < 2e-16 ***
#poly(TrialOrder, 2)1 -1.508e-01  2.621e-01  1.532e+03  -0.575  0.56511    
#poly(TrialOrder, 2)2  7.262e-01  2.625e-01  1.536e+03   2.767  0.00573 ** 
#len.z                 4.179e-02  1.356e-02  6.670e+01   3.083  0.00298 ** 
#fsuf.z               -2.967e-03  1.718e-02  6.640e+01  -0.173  0.86342    
#NVvise               -3.259e-02  3.266e-02  6.520e+01  -0.998  0.32207    
#flemk.z              -8.463e-02  1.544e-02  6.680e+01  -5.482 6.92e-07 ***
#fsuf.z:NVvise         9.480e-02  4.042e-02  6.500e+01   2.346  0.02206 * 

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

# Za sada mi je lmer6 najbolji

-------------------------------------------------

############################# POLY

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + poly(dlem, 2) + fsuf.z*NV + flemk.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat6.a,lmer.dat7.a)

#Bolji bez poly

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + len.z + poly(fsuf, 2)*NV + flemk.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat6.a,lmer.dat7.a)

#Bolji bez poly

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + poly(flemk, 2) + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat6.a,lmer.dat7.a)

#Opet bolji bez poly

-----------------------------------------------
---------------------------

################## TRENUTNI POBEDNIK (sa -nje)

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Random effects:
# Groups    Name        Variance Std.Dev.
# Stimulus  (Intercept) 0.009783 0.09891 
# Ispitanik (Intercept) 0.036776 0.19177 
# Residual              0.066544 0.25796 
#Number of obs: 1633, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.418e+00  3.605e-02  8.620e+01 -39.326  < 2e-16 ***
#poly(TrialOrder, 2)1 -1.508e-01  2.621e-01  1.532e+03  -0.575  0.56511    
#poly(TrialOrder, 2)2  7.262e-01  2.625e-01  1.536e+03   2.767  0.00573 ** 
#len.z                 4.179e-02  1.356e-02  6.670e+01   3.083  0.00298 ** 
#fsuf.z               -2.967e-03  1.718e-02  6.640e+01  -0.173  0.86342    
#NVvise               -3.259e-02  3.266e-02  6.520e+01  -0.998  0.32207    
#flemk.z              -8.463e-02  1.544e-02  6.680e+01  -5.482 6.92e-07 ***
#fsuf.z:NVvise         9.480e-02  4.042e-02  6.500e+01   2.346  0.02206 *  
#  
#
#--------------------------

#################### PODESAVANJE NAGIBA

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+poly(TrialOrder, 2)|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat6.a,lmer.dat8.a)

#bolji bez

--------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat6.a,lmer.dat8.a)

#bolji sa

------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+fsuf.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat6.a,lmer.dat8.a)

#bolji bez

------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+flemk.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat6.a,lmer.dat8.a)

#bolji bez

----------------------------
---------------------------------------------
---------------------------------------------------------

########################## TRENUTNO NAJBOLJI #######
################### SA NJE #################

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

----------------------------

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.009827 0.09913 
# Ispitanik   len.z       0.001441 0.03796 
# Ispitanik.1 (Intercept) 0.037050 0.19248 
# Residual                0.065044 0.25504 
#Number of obs: 1633, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.418e+00  3.613e-02  8.610e+01 -39.249  < 2e-16 ***
#poly(TrialOrder, 2)1 -1.756e-01  2.605e-01  1.517e+03  -0.674  0.50042    
#poly(TrialOrder, 2)2  6.739e-01  2.610e-01  1.526e+03   2.582  0.00993 ** 
#len.z                 4.195e-02  1.466e-02  7.450e+01   2.861  0.00548 ** 
#fsuf.z               -2.754e-03  1.717e-02  6.630e+01  -0.160  0.87305    
#NVvise               -3.245e-02  3.264e-02  6.510e+01  -0.994  0.32388    
#flemk.z              -8.455e-02  1.543e-02  6.670e+01  -5.481 6.97e-07 ***
#fsuf.z:NVvise         9.430e-02  4.039e-02  6.500e+01   2.335  0.02266 *    

---------------------
---------------------------

ranef(lmer.dat6)$Ispitanik

#            len.z (Intercept)
#s1  -0.0110231257 -0.14027867
#s10 -0.0086687975  0.06782104
#s11  0.0477483061  0.24569607
#s12  0.0063467909 -0.01433681
#s13 -0.0080823604  0.02919628
#s14  0.0261766582 -0.14529140
#s15  0.0041526815 -0.11208360
#s16  0.0055746673  0.38557609
#s17 -0.0008957840  0.23672677
#s18  0.0168978836 -0.04569008
#s19  0.0078548157 -0.06447185
#s2  -0.0182770858 -0.16133126
#s20 -0.0015260930 -0.10410122
#s21  0.0190292923  0.24920872
#s22  0.0138521445  0.04885379
#s23  0.0346196332  0.20651242
#s24 -0.0278603282 -0.06550308
#s25  0.0036614597  0.28074578
#s26 -0.0312096963 -0.07364909
#s27 -0.0401221587  0.02543576
#s28 -0.0236782733 -0.06883712
#s29 -0.0131442521 -0.20382236
#s3  -0.0333594721 -0.15115939
#s30 -0.0405543478 -0.16177413
#s31  0.0200368256  0.10251858
#s32  0.0358571465 -0.24636431
#s33 -0.0021045936 -0.11815268
#s34  0.0114104948  0.11301616
#s35  0.0212598056 -0.01414419
#s36  0.0120152029  0.15077235
#s37  0.0123099164  0.11344144
#s38  0.0009500907 -0.12519350
#s39  0.0154263262  0.21486434
#s4  -0.0033753858 -0.06069783
#s40  0.0447868061  0.35817620
#s41  0.0480042229 -0.06437942
#s42 -0.0404795860  0.26782672
#s43 -0.0017483155 -0.10649911
#s44 -0.0291578144 -0.25159898
#s45  0.0364123787 -0.24063174
#s46 -0.0120172038  0.44409798
#s5  -0.0064157035 -0.19938559
#s6  -0.0587816656 -0.28223029
#s7   0.0053484704  0.05080296
#s8  -0.0228080574 -0.16883653
#s9  -0.0144419192 -0.20084521

------------------------------
---------------------------------
############ OBRTANJE

dat$NV <- relevel(dat$NV, ref = "vise")

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.009827 0.09913 
# Ispitanik   len.z       0.001441 0.03796 
# Ispitanik.1 (Intercept) 0.037050 0.19248 
# Residual                0.065044 0.25504 
#Number of obs: 1633, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.45053    0.03680   88.90000 -39.413  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.17555    0.26046 1517.00000  -0.674  0.50042    
#poly(TrialOrder, 2)2    0.67393    0.26104 1525.50000   2.582  0.00993 ** 
#len.z                   0.04195    0.01466   74.50000   2.861  0.00548 ** 
#fsuf.z                  0.09154    0.03762   64.80000   2.433  0.01773 *  
#NVjedno                 0.03245    0.03264   65.10000   0.994  0.32388    
#flemk.z                -0.08455    0.01543   66.70000  -5.481 6.97e-07 ***
#fsuf.z:NVjedno         -0.09430    0.04039   65.00000  -2.335  0.02266 *

------------------------
-----------------------------------------

#################### KRITIKA 

confint(lmer.dat6)

#                           2.5 %      97.5 %
#.sig01                0.07560566  0.11776505
#.sig02                0.01909755  0.05677055
#.sig03                0.15537805  0.23949336
#.sigma                0.24595062  0.26439946
#(Intercept)          -1.52195291 -1.37913998
#poly(TrialOrder, 2)1 -0.68892891  0.33254054
#poly(TrialOrder, 2)2  0.16210654  1.18666039
#len.z                 0.01379138  0.07011211
#fsuf.z                0.01946437  0.16355937
#NVjedno              -0.03006532  0.09495119
#flemk.z              -0.11408571 -0.05498757
#fsuf.z:NVjedno       -0.17161429 -0.01691395

par(mfrow=c(2,3))
plot(fitted(lmer.dat6),residuals(lmer.dat6))
plot(fitted(lmer.dat6),scale(residuals(lmer.dat6)))
qqnorm(residuals(lmer.dat6), main=" ")
qqline(residuals(lmer.dat6))
plot(fitted(lmer.dat6),residuals(lmer.dat6))
plot(fitted(lmer.dat6),scale(residuals(lmer.dat6)))
qqnorm(residuals(lmer.dat6), main=" ")
qqline(residuals(lmer.dat6))
par(mfrow=c(1,1))

b1=bootMer(lmer.dat6, FUN = function(x) as.numeric(logLik(x)), nsim = 100)

head(as.data.frame(b1))

#         V1
#1 -196.0608
#2 -182.0687
#3 -236.4169
#4 -245.6825
#5 -249.7045
#6 -265.9767

plot(b1$t)

-------------
------------------------

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

lmer.dat6.a <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat, subset=abs(scale(resid(lmer.dat6)))<2.5)
summary (lmer.dat6.a)

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.010277 0.10138 
# Ispitanik   len.z       0.002172 0.04661 
# Ispitanik.1 (Intercept) 0.037352 0.19327 
# Residual                0.057162 0.23909 
#Number of obs: 1610, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.45755    0.03698   89.70000 -39.419  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.30485    0.24781 1487.20000  -1.230  0.21884    
#poly(TrialOrder, 2)2    0.39290    0.24881 1494.90000   1.579  0.11453    
#len.z                   0.04338    0.01525   80.70000   2.845  0.00563 ** 
#fsuf.z                  0.09527    0.03782   64.70000   2.519  0.01424 *  
#NVjedno                 0.03087    0.03283   65.10000   0.940  0.35063    
#flemk.z                -0.08449    0.01550   66.40000  -5.451  7.9e-07 ***
#fsuf.z:NVjedno         -0.10497    0.04061   64.90000  -2.585  0.01200 *  

---------------------
dat$NV <- relevel(dat$NV, ref = "jedno")

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

lmer.dat6.a <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat, subset=abs(scale(resid(lmer.dat6)))<2.5)
summary (lmer.dat6.a)

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.010277 0.10138 
# Ispitanik   len.z       0.002172 0.04661 
# Ispitanik.1 (Intercept) 0.037352 0.19327 
# Residual                0.057162 0.23909 
#Number of obs: 1610, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.427e+00  3.630e-02  8.700e+01 -39.299  < 2e-16 ***
#poly(TrialOrder, 2)1 -3.048e-01  2.478e-01  1.487e+03  -1.230  0.21884    
#poly(TrialOrder, 2)2  3.929e-01  2.488e-01  1.495e+03   1.579  0.11453    
#len.z                 4.338e-02  1.525e-02  8.070e+01   2.845  0.00563 ** 
#fsuf.z               -9.696e-03  1.727e-02  6.630e+01  -0.562  0.57631    
#NVvise               -3.087e-02  3.283e-02  6.510e+01  -0.940  0.35063    
#flemk.z              -8.449e-02  1.550e-02  6.640e+01  -5.451  7.9e-07 ***
#fsuf.z:NVvise         1.050e-01  4.061e-02  6.490e+01   2.585  0.01200 * 

------------------
------------------------------------

################# ISTI MODEL SAMO BEZ NJE #######################

dat1=dat[dat$FrekvencijaSufiksa<10000,]

lmer.dat1 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat1)
summary (lmer.dat1)

lmer.dat1.a <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat1, subset=abs(scale(resid(lmer.dat1)))<2.5)
summary (lmer.dat1.a)

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.01002  0.10009 
# Ispitanik   len.z       0.00214  0.04626 
# Ispitanik.1 (Intercept) 0.03705  0.19248 
# Residual                0.05771  0.24023 
#Number of obs: 1525, groups:  Stimulus, 70; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.453e+00  4.042e-02  9.820e+01 -35.943  < 2e-16 ***
#poly(TrialOrder, 2)1 -3.489e-01  2.491e-01  1.408e+03  -1.401  0.16156    
#poly(TrialOrder, 2)2  4.169e-01  2.496e-01  1.410e+03   1.670  0.09510 .  
#len.z                 3.871e-02  1.534e-02  7.560e+01   2.523  0.01373 *  
#fsuf.z               -3.102e-02  2.288e-02  6.200e+01  -1.356  0.18008    
#NVvise               -4.408e-03  3.733e-02  6.100e+01  -0.118  0.90640    
#flemk.z              -8.700e-02  1.558e-02  6.260e+01  -5.583 5.43e-07 ***
#fsuf.z:NVvise         1.253e-01  4.274e-02  6.090e+01   2.931  0.00475 ** 
 
---------------------
dat1$NV <- relevel(dat1$NV, ref = "vise")

lmer.dat1 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat1)
summary (lmer.dat1)

lmer.dat1.a <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemk.z + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat1, subset=abs(scale(resid(lmer.dat1)))<2.5)
summary (lmer.dat1.a)

#Random effects:
#Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.01002  0.10009 
# Ispitanik   len.z       0.00214  0.04626 
# Ispitanik.1 (Intercept) 0.03705  0.19248 
# Residual                0.05771  0.24023 
#Number of obs: 1525, groups:  Stimulus, 70; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.457e+00  3.675e-02  8.760e+01 -39.652  < 2e-16 ***
#poly(TrialOrder, 2)1 -3.489e-01  2.491e-01  1.408e+03  -1.401  0.16156    
#poly(TrialOrder, 2)2  4.169e-01  2.496e-01  1.410e+03   1.670  0.09510 .  
#len.z                 3.871e-02  1.534e-02  7.560e+01   2.523  0.01373 *  
#fsuf.z                9.425e-02  3.751e-02  6.080e+01   2.512  0.01467 *  
#NVjedno               4.408e-03  3.733e-02  6.100e+01   0.118  0.90640    
#flemk.z              -8.700e-02  1.558e-02  6.260e+01  -5.583 5.43e-07 ***
#fsuf.z:NVjedno       -1.253e-01  4.274e-02  6.090e+01  -2.931  0.00475 ** 

-------------
---------------------------
############ Nista se veliko ne desi kada se izbaci -nje, ali se koeficijenti malo stabilizuju
_________________________________________________________________
__________________________________________________________________________________
_________________________________________________________________________________________________________
_______________________________________________________________________________________________________________________________

################ INTERAKCIJA (RT inverzno) ############

g6<-ggplot(data=dat1)+geom_smooth(aes(x=fsuf.z,y=RT, group=NV, shape=NV, color=NV), method="lm") + 
     scale_color_manual(values=c("#D92121","grey5"))+
     scale_size_manual(values=c(5, 5)) + 
     theme(axis.text.x = element_text(colour="grey20",size=10)) +
     theme(axis.title.x = element_text(colour="gray20", size=10)) +
     theme(axis.text.y = element_text(colour="grey20",size=10)) +
     theme(axis.title.y = element_text(colour="gray20", size=10))
g6

_________________________________________________________
__________________________________________________________________________________
_________________________________________________________________________________________________________

################################################################# MODEL BROJ 2: FREKVENCIJA LEME (SRWAC)


########## SLUCAJNI EFEKTI

lmer0 <- lmer(RT ~ 1 + (1|Ispitanik) + (1|Stimulus),	data=dat)
ranefItem <- lmer(RT ~ 1 + (1|Stimulus),	data=dat)
ranefSubj <- lmer(RT ~ 1 + (1|Ispitanik), data=dat)

anova(ranefItem, lmer0)

#object: RT ~ 1 + (1 | Stimulus)
#..1: RT ~ 1 + (1 | Ispitanik) + (1 | Stimulus)
#       Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  3 1043.87 1060.06 -518.93  1037.87                             
#..1     4  502.38  523.98 -247.19   494.38 543.48      1  < 2.2e-16 ***

#Bolji je drugi model (AIC i BIC sto manji, LogLik sto veci)

anova(ranefSubj, lmer0)

#object: RT ~ 1 + (1 | Ispitanik)
#..1: RT ~ 1 + (1 | Ispitanik) + (1 | Stimulus)
#       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  3 730.20 746.39 -362.10   724.20                             
#..1     4 502.38 523.98 -247.19   494.38 229.81      1  < 2.2e-16 ***

#Opet je bolji drugi
__________________________________________________________
________________________________________________________________

############# TRIAL ORDER 

lmer.dat <- lmer(RT ~ trial.z + (1|Ispitanik) + (1|Stimulus),	data=dat)
summary (lmer.dat)

#              Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept) -1.411e+00  3.303e-02  6.970e+01 -42.712   <2e-16 ***
#trial.z     -2.744e-03  6.509e-03  1.528e+03  -0.422    0.673      

# Nije znacajan

lmer.dat1 <- lmer(RT ~ poly(TrialOrder, 2) + (1|Ispitanik) + (1|Stimulus), data=dat)
summary (lmer.dat1)

lmer.dat.a=update(lmer.dat, REML=FALSE)
lmer.dat1.a=update(lmer.dat1, REML=FALSE)

anova(lmer.dat.a,lmer.dat1.a)

#Bolji sa poly 

------------------------

################ JEDAN PO JEDAN

lmer.dat2 <- lmer(RT ~ poly(TrialOrder,2) + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat2)

lmer.dat3 <- lmer(RT ~ poly(TrialOrder,2) + len.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat3)

lmer.dat4 <- lmer(RT ~ poly(TrialOrder,2) + len.z + flemw.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat4)

lmer.dat5 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat5)

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + flemw.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.41029    0.03473   78.40000 -40.607  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.14721    0.26190 1536.40000  -0.562 0.574134    
#poly(TrialOrder, 2)2    0.72810    0.26223 1541.20000   2.777 0.005560 ** 
#len.z                   0.04421    0.01224   66.40000   3.611 0.000587 ***
#suf.z                  0.01632    0.01622   66.10000   1.006 0.318091    
#NVvise                 -0.01309    0.02986   64.60000  -0.438 0.662579    
#flemw.z                -0.09973    0.01392   66.90000  -7.165 7.71e-10 ***
#fsuf.z:NVvise           0.01521    0.03754   64.30000   0.405 0.686747  

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

# Za sada mi je lmer5 bolji, bez interakcije dakle. Dalje isprobavam neke druge interakcije.

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV*flemw.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                        Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)           -1.404e+00  3.533e-02  8.100e+01 -39.732  < 2e-16 ***
#poly(TrialOrder, 2)1  -1.458e-01  2.620e-01  1.534e+03  -0.556 0.578093    
#poly(TrialOrder, 2)2   7.265e-01  2.623e-01  1.540e+03   2.770 0.005670 ** 
#len.z                  4.406e-02  1.269e-02  6.320e+01   3.472 0.000938 ***
#fsuf.z                 1.141e-02  1.788e-02  6.290e+01   0.638 0.525521    
#NVvise                -1.877e-02  3.317e-02  6.160e+01  -0.566 0.573610    
#flemw.z               -1.070e-01  2.842e-02  6.280e+01  -3.766 0.000368 ***
#fsuf.z:NVvise          2.335e-02  4.360e-02  6.170e+01   0.535 0.594248    
#fsuf.z:flemw.z        -1.275e-02  1.438e-02  6.270e+01  -0.886 0.378869    
#NVvise:flemw.z         3.036e-03  4.176e-02  6.180e+01   0.073 0.942290    
#fsuf.z:NVvise:flemw.z  2.265e-03  4.556e-02  6.120e+01   0.050 0.960509 

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

##### Bolje bez ove interakcije, dakle i dalje bolje bez interakcija.

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z*fsuf.z*NV + flemw.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.38492    0.03631   86.10000 -38.137  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.14133    0.26190 1536.30000  -0.540  0.58954    
#poly(TrialOrder, 2)2    0.72393    0.26217 1542.00000   2.761  0.00583 ** 
#len.z                   0.01145    0.02213   63.50000   0.517  0.60667    
#fuf.z                  0.06446    0.02515   63.00000   2.563  0.01279 *  
#NVvise                 -0.03602    0.03386   62.10000  -1.064  0.29165    
#flemw.z                -0.11099    0.01437   65.00000  -7.723 9.12e-11 ***
#len.z:fsuf.z           -0.05340    0.02190   62.70000  -2.438  0.01761 *  
#len.z:NVvise            0.02172    0.03478   62.70000   0.625  0.53456    
#fsuf.z:NVvise          -0.03857    0.04610   61.60000  -0.837  0.40600    
#len.z:fsuf.z:NVvise     0.04677    0.05714   62.40000   0.818  0.41620 

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

### Bolje bez interakcija, dakle zvanicno je najbolji lmer5.

-------------------------------------------------

############################# POLY

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + poly(dlem, 2) + fsuf.z + flemw.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat5.a,lmer.dat7.a)

#Bolji bez poly

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + len.z + poly(fsuf, 2) + NV + flemw.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat5.a,lmer.dat7.a)

#Bolji bez poly

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + NV + poly(flemw, 2) + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat5.a,lmer.dat7.a)

#Opet bolji bez poly

-----------------------------------------------
---------------------------

################## TRENUTNI POBEDNIK (sa -nje)

lmer.dat5 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat5)

#Random effects:
# Groups    Name        Variance Std.Dev.
# Stimulus  (Intercept) 0.007259 0.0852  
# Ispitanik (Intercept) 0.037267 0.1930  
# Residual              0.066548 0.2580  
#Number of obs: 1633, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.409e+00  3.453e-02  7.740e+01 -40.812  < 2e-16 ***
#poly(TrialOrder, 2)1 -1.499e-01  2.618e-01  1.538e+03  -0.573 0.567035    
#poly(TrialOrder, 2)2  7.269e-01  2.622e-01  1.542e+03   2.772 0.005632 ** 
#len.z                 4.346e-02  1.202e-02  6.720e+01   3.614 0.000576 ***
#fsuf.z                1.935e-02  1.429e-02  6.680e+01   1.354 0.180153    
#flemw.z              -1.011e-01  1.340e-02  6.800e+01  -7.546 1.46e-10 ***
#NVvise               -8.928e-03  2.785e-02  6.600e+01  -0.321 0.749509  
#  
#--------------------------

#################### PODESAVANJE NAGIBA

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+poly(TrialOrder, 2)|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat5.a,lmer.dat8.a)

#bolji bez

--------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat5.a,lmer.dat8.a)

#bolji sa

------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+fsuf.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat5.a,lmer.dat8.a)

#bolji bez

------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+flemw.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat5.a,lmer.dat8.a)

#bolji bez

----------------------------
---------------------------------------------
---------------------------------------------------------

########################## TRENUTNO NAJBOLJI #######
################### SA NJE #################

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

----------------------------

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.007304 0.08546 
# Ispitanik   len.z       0.001444 0.03799 
# Ispitanik.1 (Intercept) 0.037542 0.19376 
# Residual                0.065043 0.25503 
#Number of obs: 1633, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.409e+00  3.460e-02  7.730e+01 -40.724  < 2e-16 ***
#poly(TrialOrder, 2)1 -1.741e-01  2.602e-01  1.523e+03  -0.669  0.50345    
#poly(TrialOrder, 2)2  6.747e-01  2.608e-01  1.531e+03   2.587  0.00976 ** 
#len.z                 4.368e-02  1.326e-02  7.280e+01   3.294  0.00153 ** 
#fsuf.z                1.949e-02  1.427e-02  6.670e+01   1.366  0.17662    
#flemw.z              -1.010e-01  1.338e-02  6.800e+01  -7.548 1.45e-10 ***
#NVvise               -8.964e-03  2.782e-02  6.600e+01  -0.322  0.74829        

---------------------
---------------------------

ranef(lmer.dat6)$Ispitanik

#            len.z  (Intercept)
#s1  -1.020371e-02 -0.149304889
#s10 -7.856106e-03  0.058597355
#s11  4.858617e-02  0.237648636
#s12  7.552331e-03 -0.023866583
#s13 -8.152397e-03  0.018656838
#s14  2.695981e-02 -0.155339291
#s15  5.324563e-03 -0.122529510
#s16  6.351127e-03  0.375809000
#s17 -1.131826e-04  0.226898189
#s18  1.807829e-02 -0.056099818
#s19  8.693734e-03 -0.073448031
#s2  -1.754196e-02 -0.171385054
#s20 -9.912968e-04 -0.113679486
#s21  1.917068e-02  0.240707251
#s22  1.510283e-02  0.038408126
#s23  3.504900e-02  0.198257181
#s24 -2.735493e-02 -0.075065191
#s25  2.922043e-03  0.291435055
#s26 -3.197357e-02 -0.063188350
#s27 -4.090164e-02  0.035962065
#s28 -2.460938e-02 -0.057573444
#s29 -1.407338e-02 -0.193113810
#s3  -3.285143e-02 -0.160766355
#s30 -4.132643e-02 -0.151369329
#s31  1.922800e-02  0.112660280
#s32  3.498795e-02 -0.235062496
#s33 -2.621619e-03 -0.107258139
#s34  1.068404e-02  0.123595825
#s35  2.046050e-02 -0.004081265
#s36  1.130912e-02  0.161317029
#s37  1.158472e-02  0.124013694
#s38  9.124050e-06 -0.114508791
#s39  1.495974e-02  0.224249305
#s4  -2.623638e-03 -0.070698965
#s40  4.420160e-02  0.369147040
#s41  4.731698e-02 -0.053920962
#s42 -4.121899e-02  0.278593216
#s43 -2.982599e-03 -0.097119836
#s44 -2.991265e-02 -0.240871181
#s45  3.571513e-02 -0.230279654
#s46 -1.277034e-02  0.454871879
#s5  -5.403404e-03 -0.211641154
#s6  -5.888210e-02 -0.291765509
#s7   6.058249e-03  0.041786239
#s8  -2.202161e-02 -0.178198760
#s9  -1.391939e-02 -0.210478348

-------------
-----------------------

#################### KRITIKA 

confint(lmer.dat6)

#                            2.5 %      97.5 %
#.sig01                0.064077211  0.10382380
#.sig02                0.019135553  0.05667846
#.sig03                0.156372755  0.24104199
#.sigma                0.245949518  0.26439857
#(Intercept)          -1.483321275 -1.35312871
#poly(TrialOrder, 2)1 -0.686069927  0.33412379
#poly(TrialOrder, 2)2  0.163629277  1.18705093
#len.z                 0.018035862  0.06935238
#fsuf.z               -0.008056408  0.04705005
#flemw.z              -0.126871071 -0.07519158
#NVjedno              -0.044742312  0.06266327

par(mfrow=c(2,3))
plot(fitted(lmer.dat6),residuals(lmer.dat6))
plot(fitted(lmer.dat6),scale(residuals(lmer.dat6)))
qqnorm(residuals(lmer.dat6), main=" ")
qqline(residuals(lmer.dat6))
plot(fitted(lmer.dat6),residuals(lmer.dat6))
plot(fitted(lmer.dat6),scale(residuals(lmer.dat6)))
qqnorm(residuals(lmer.dat6), main=" ")
qqline(residuals(lmer.dat6))
par(mfrow=c(1,1))

b1=bootMer(lmer.dat6, FUN = function(x) as.numeric(logLik(x)), nsim = 100)

head(as.data.frame(b1))

#         V1
#1 -186.8942
#2 -174.9946
#3 -226.4795
#4 -237.7921
#5 -239.6725
#6 -258.0367

plot(b1$t)

-------------
------------------------
dat$NV <- relevel(dat$NV, ref = "jedno")

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

lmer.dat6.a <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat, subset=abs(scale(resid(lmer.dat6)))<2.5)
summary (lmer.dat6.a)

#Random effects:
# Groups      Name        Variance Std.Dev.
#Stimulus    (Intercept) 0.007762 0.08810 
# Ispitanik   len.z       0.001977 0.04446 
# Ispitanik.1 (Intercept) 0.037704 0.19417 
# Residual                0.057342 0.23946 
#Number of obs: 1610, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.41583    0.03474   78.30000 -40.750  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.32388    0.24788 1493.40000  -1.307  0.19155    
#poly(TrialOrder, 2)2    0.52841    0.24847 1500.80000   2.127  0.03361 *  
#len.z                   0.04461    0.01376   78.20000   3.242  0.00174 ** 
#fsuf.z                  0.01367    0.01438   66.70000   0.950  0.34542    
#flemw.z                -0.10326    0.01347   67.70000  -7.664 9.16e-11 ***
#NVvise                 -0.00459    0.02804   66.00000  -0.164  0.87046    

------------------
------------------------------------

################# ISTI MODEL SAMO BEZ NJE #######################

dat1=dat[dat$FrekvencijaSufiksa<10000,]

dat1$NV <- relevel(dat1$NV, ref = "jedno")

lmer.dat1 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat1)
summary (lmer.dat1)

lmer.dat1.a <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + flemw.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat1, subset=abs(scale(resid(lmer.dat1)))<2.5)
summary (lmer.dat1.a)

#Random effects:
# Groups      Name        Variance Std.Dev.
#Stimulus    (Intercept) 0.007910 0.08894 
#Ispitanik   len.z       0.002125 0.04610 
# Ispitanik.1 (Intercept) 0.037345 0.19325 
# Residual                0.057271 0.23931 
#Number of obs: 1523, groups:  Stimulus, 70; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.424e+00  3.752e-02  9.060e+01 -37.968  < 2e-16 ***
#poly(TrialOrder, 2)1 -3.436e-01  2.482e-01  1.410e+03  -1.384  0.16659    
#poly(TrialOrder, 2)2  4.449e-01  2.488e-01  1.412e+03   1.788  0.07401 .  
#len.z                 4.213e-02  1.424e-02  7.500e+01   2.958  0.00414 ** 
#fsuf.z                7.539e-03  1.827e-02  6.250e+01   0.413  0.68131    
#flemw.z              -1.033e-01  1.371e-02  6.380e+01  -7.531  2.2e-10 ***
#NVvise                5.317e-03  3.375e-02  6.170e+01   0.158  0.87532      

### nikakvih znacajnih promena
_____________________________________________
____________________________________________________________
__________________________________________________________________________________

################################################################# MODEL BROJ 3: FREKVENCIJA POVRSINSKA (SRWAC)


########## SLUCAJNI EFEKTI

lmer0 <- lmer(RT ~ 1 + (1|Ispitanik) + (1|Stimulus),	data=dat)
ranefItem <- lmer(RT ~ 1 + (1|Stimulus),	data=dat)
ranefSubj <- lmer(RT ~ 1 + (1|Ispitanik), data=dat)

anova(ranefItem, lmer0)

#object: RT ~ 1 + (1 | Stimulus)
#..1: RT ~ 1 + (1 | Ispitanik) + (1 | Stimulus)
#       Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  3 1043.87 1060.06 -518.93  1037.87                             
#..1     4  502.38  523.98 -247.19   494.38 543.48      1  < 2.2e-16 ***

#Bolji je drugi model (AIC i BIC sto manji, LogLik sto veci)

anova(ranefSubj, lmer0)

#object: RT ~ 1 + (1 | Ispitanik)
#..1: RT ~ 1 + (1 | Ispitanik) + (1 | Stimulus)
#       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  3 730.20 746.39 -362.10   724.20                             
#..1     4 502.38 523.98 -247.19   494.38 229.81      1  < 2.2e-16 ***

#Opet je bolji drugi
__________________________________________________________
________________________________________________________________

############# TRIAL ORDER 

lmer.dat <- lmer(RT ~ trial.z + (1|Ispitanik) + (1|Stimulus),	data=dat)
summary (lmer.dat)

#              Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept) -1.411e+00  3.303e-02  6.970e+01 -42.712   <2e-16 ***
#trial.z     -2.744e-03  6.509e-03  1.528e+03  -0.422    0.673      

# Nije znacajan

lmer.dat1 <- lmer(RT ~ poly(TrialOrder, 2) + (1|Ispitanik) + (1|Stimulus), data=dat)
summary (lmer.dat1)

lmer.dat.a=update(lmer.dat, REML=FALSE)
lmer.dat1.a=update(lmer.dat1, REML=FALSE)

anova(lmer.dat.a,lmer.dat1.a)

#Bolji sa poly 

------------------------

################ JEDAN PO JEDAN

lmer.dat2 <- lmer(RT ~ poly(TrialOrder,2) + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat2)

lmer.dat3 <- lmer(RT ~ poly(TrialOrder,2) + len.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat3)

lmer.dat4 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fpov.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat4)

lmer.dat5 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat5)

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV + fpov.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.40829    0.03484   79.30000 -40.419  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.14677    0.26200 1535.70000  -0.560 0.575421    
#poly(TrialOrder, 2)2    0.72958    0.26234 1540.40000   2.781 0.005484 ** 
#len.z                   0.04361    0.01242   66.50000   3.511 0.000806 ***
#fsuf.z                  0.01500    0.01646   66.30000   0.911 0.365356    
#NVvise                 -0.01539    0.03027   64.80000  -0.509 0.612805    
#fpov.z                 -0.09649    0.01403   67.40000  -6.878 2.43e-09 ***
#fsuf.z:NVvise           0.01127    0.03831   64.40000   0.294 0.769536  

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

# Za sada mi je lmer5 bolji, bez interakcije dakle. Dalje isprobavam neke druge interakcije.

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z*NV*fpov.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.408e+00  3.586e-02  8.390e+01 -39.273  < 2e-16 ***
#poly(TrialOrder, 2)1 -1.448e-01  2.621e-01  1.533e+03  -0.552 0.580793    
#poly(TrialOrder, 2)2  7.305e-01  2.624e-01  1.539e+03   2.784 0.005437 ** 
#len.z                 4.304e-02  1.302e-02  6.320e+01   3.307 0.001559 ** 
#fsuf.z                1.415e-02  1.871e-02  6.310e+01   0.756 0.452185    
#NVvise               -2.398e-02  3.314e-02  6.180e+01  -0.724 0.472029    
#fpov.z               -9.399e-02  2.702e-02  6.300e+01  -3.479 0.000918 ***
#fsuf.z:NVvise         2.737e-02  4.285e-02  6.190e+01   0.639 0.525324    
#fsuf.z:fpov.z         7.129e-04  1.476e-02  6.320e+01   0.048 0.961640    
#NVvise:fpov.z         1.164e-02  3.724e-02  6.210e+01   0.313 0.755642    
#fsuf.z:NVvise:fpov.z -3.448e-02  3.999e-02  6.150e+01  -0.862 0.391817  

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

##### Bolje bez ove interakcije, dakle i dalje bolje bez interakcija.

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z*fsuf.z*NV + fpov.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                      Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.393e+00  3.684e-02  8.850e+01 -37.806  < 2e-16 ***
#poly(TrialOrder, 2)1 -1.400e-01  2.621e-01  1.535e+03  -0.534  0.59329    
#poly(TrialOrder, 2)2  7.268e-01  2.623e-01  1.540e+03   2.770  0.00567 ** 
#len.z                 2.632e-02  2.284e-02  6.340e+01   1.152  0.25346    
#fsuf.z                4.489e-02  2.521e-02  6.290e+01   1.781  0.07973 .  
#NVvise               -2.353e-02  3.547e-02  6.220e+01  -0.663  0.50960    
#fpov.z               -1.024e-01  1.449e-02  6.500e+01  -7.066 1.32e-09 ***
#len.z:fsuf.z         -3.397e-02  2.222e-02  6.250e+01  -1.529  0.13136    
#len.z:NVvise         -4.238e-03  3.613e-02  6.270e+01  -0.117  0.90700    
#fsuf.z:NVvise        -3.327e-02  4.792e-02  6.170e+01  -0.694  0.49005    
#len.z:fsuf.z:NVvise   5.330e-02  5.942e-02  6.250e+01   0.897  0.37309  

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

### Bolje bez interakcija, dakle zvanicno je najbolji lmer5.

-------------------------------------------------

############################# POLY

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + poly(dlem, 2) + fsuf.z + fpov.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat5.a,lmer.dat7.a)

#Bolji bez poly

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + len.z + poly(fsuf, 2) + NV + fpov.z + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat5.a,lmer.dat7.a)

#Bolji bez poly

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + NV + poly(fpov, 2) + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat7)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat5.a,lmer.dat7.a)

#Opet bolji bez poly

-----------------------------------------------
---------------------------

################## TRENUTNI POBEDNIK (sa -nje)

lmer.dat5 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat5)

#Random effects:
# Groups    Name        Variance Std.Dev.
#Stimulus  (Intercept) 0.00755  0.08689 
# Ispitanik (Intercept) 0.03713  0.19269 
# Residual              0.06658  0.25804 
#Number of obs: 1633, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.40740    0.03463   78.20000 -40.646  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.14891    0.26193 1537.10000  -0.568 0.569781    
#poly(TrialOrder, 2)2    0.72881    0.26230 1541.10000   2.778 0.005528 ** 
#len.z                   0.04306    0.01219   67.30000   3.532 0.000749 ***
#fsuf.z                  0.01727    0.01443   66.90000   1.197 0.235666    
#fpov.z                 -0.09759    0.01342   68.60000  -7.272  4.4e-10 ***
#NVvise                 -0.01230    0.02818   66.20000  -0.436 0.663951   
#  
#--------------------------

#################### PODESAVANJE NAGIBA

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+poly(TrialOrder, 2)|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat5.a,lmer.dat8.a)

#bolji bez

--------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat5.a,lmer.dat8.a)

#bolji sa

------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+fsuf.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat5.a,lmer.dat8.a)

#bolji bez

------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+fpov.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat8)

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat5.a,lmer.dat8.a)

#bolji bez

----------------------------
---------------------------------------------
---------------------------------------------------------

########################## TRENUTNO NAJBOLJI #######
################### SA NJE #################

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

----------------------------

#Random effects:
# Groups      Name        Variance Std.Dev.
#Stimulus    (Intercept) 0.007594 0.08714 
#Ispitanik   len.z       0.001441 0.03796 
# Ispitanik.1 (Intercept) 0.037403 0.19340 
# Residual                0.065082 0.25511 
#Number of obs: 1633, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                      Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.40758    0.03470   78.10000 -40.560  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.17313    0.26027 1522.30000  -0.665  0.50602    
#poly(TrialOrder, 2)2    0.67661    0.26088 1530.10000   2.594  0.00959 ** 
#len.z                   0.04325    0.01341   73.10000   3.225  0.00188 ** 
#fsuf.z                  0.01738    0.01441   66.80000   1.206  0.23221    
#fpov.z                 -0.09746    0.01340   68.50000  -7.271 4.45e-10 ***
#NVvise                 -0.01230    0.02815   66.10000  -0.437  0.66359  
---------------------
---------------------------

ranef(lmer.dat6)$Ispitanik

#            len.z  (Intercept)
#s1  -0.0106722655 -0.147717783
#10 -0.0083444112  0.061341940
#s11  0.0480104103  0.240419870
#s12  0.0071610246 -0.021222549
#s13 -0.0083808796  0.022164221
#s14  0.0260362879 -0.152009568
#s15  0.0044813839 -0.119223148
#s16  0.0053761650  0.379282937
#s17 -0.0006696906  0.229717871
#s18  0.0171930053 -0.052656254
#s19  0.0080837178 -0.070589866
#s2  -0.0183378038 -0.168948404
#s20 -0.0016722080 -0.110592074
#s21  0.0184972275  0.243707657
#s22  0.0145105900  0.040991041
#s23  0.0343164826  0.200184445
#s24 -0.0280072525 -0.071855640
#s25  0.0036314388  0.288430914
#s26 -0.0312094993 -0.066119489
#s27 -0.0401318321  0.033004331
#s28 -0.0239757562 -0.060785798
#s29 -0.0137082776 -0.196092316
#s3  -0.0334149002 -0.158667354
#s30 -0.0405498144 -0.154287293
#s31  0.0199142641  0.109696390
#s32  0.0353519346 -0.237807243
#s33 -0.0021396868 -0.110698039
#s34  0.0113888598  0.120626759
#s35  0.0211547067 -0.007020163
#s36  0.0120746167  0.158174728
#s37  0.0122878398  0.121035440
#s38  0.0005208410 -0.117171500
#s39  0.0155656389  0.221485184
#s4  -0.0034569702 -0.068059518
#s40  0.0447889892  0.365981369
#s41  0.0479852131 -0.057020597
#s42 -0.0405964446  0.275261073
#s43 -0.0022565376 -0.100036423
#s44 -0.0293180903 -0.243977361
#s45  0.0363835039 -0.233170402
#s46 -0.0120575035  0.452053398
#s5  -0.0053430650 -0.209324848
#s6  -0.0590008103 -0.288772968
#s7   0.0055595424  0.043681677
#s8  -0.0224870712 -0.175536812
#s9  -0.0145429132 -0.207877836

-------------
-----------------------

#################### KRITIKA 

confint(lmer.dat6)

#                           2.5 %      97.5 %
#.sig01                0.06562199  0.10566359
#.sig02                0.01907259  0.05668007
#.sig03                0.15608702  0.24058989
#.sigma                0.24602304  0.26447797
#(Intercept)          -1.47536330 -1.33986308
#poly(TrialOrder, 2)1 -0.68532700  0.33530168
#poly(TrialOrder, 2)2  0.16546473  1.18936392
#len.z                 0.01731666  0.06920027
#fsuf.z               -0.01044134  0.04521042
#fpov.z               -0.12334519 -0.07159164
#NVvise               -0.06663960  0.04204709

par(mfrow=c(2,3))
plot(fitted(lmer.dat6),residuals(lmer.dat6))
plot(fitted(lmer.dat6),scale(residuals(lmer.dat6)))
qqnorm(residuals(lmer.dat6), main=" ")
qqline(residuals(lmer.dat6))
plot(fitted(lmer.dat6),residuals(lmer.dat6))
plot(fitted(lmer.dat6),scale(residuals(lmer.dat6)))
qqnorm(residuals(lmer.dat6), main=" ")
qqline(residuals(lmer.dat6))
par(mfrow=c(1,1))

b1=bootMer(lmer.dat6, FUN = function(x) as.numeric(logLik(x)), nsim = 100)

head(as.data.frame(b1))

#         V1
#1 -203.7070
#2 -234.5074
#3 -246.0733
#4 -205.9239
#5 -265.4363
#6 -171.9436

plot(b1$t)

-------------
------------------------
dat$NV <- relevel(dat$NV, ref = "jedno")

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat)
summary (lmer.dat6)

lmer.dat6.a <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat, subset=abs(scale(resid(lmer.dat6)))<2.5)
summary (lmer.dat6.a)

#Random effects:
#Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.008225 0.09069 
# Ispitanik   len.z       0.002102 0.04585 
# Ispitanik.1 (Intercept) 0.037235 0.19296 
# Residual                0.057058 0.23887 
#Number of obs: 1609, groups:  Stimulus, 74; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.414e+00  3.483e-02  7.990e+01 -40.604  < 2e-16 ***
#poly(TrialOrder, 2)1 -3.683e-01  2.475e-01  1.491e+03  -1.488  0.13689    
#poly(TrialOrder, 2)2  4.419e-01  2.480e-01  1.497e+03   1.782  0.07498 .  
#len.z                 4.376e-02  1.408e-02  7.930e+01   3.108  0.00261 ** 
#fsuf.z                1.232e-02  1.463e-02  6.670e+01   0.842  0.40265    
#fpov.z               -9.852e-02  1.359e-02  6.810e+01  -7.251 4.97e-10 ***
#NVvise               -8.986e-03  2.858e-02  6.600e+01  -0.314  0.75418       

------------------
------------------------------------

################# ISTI MODEL SAMO BEZ NJE #######################

dat1=dat[dat$FrekvencijaSufiksa<10000,]

dat1$NV <- relevel(dat1$NV, ref = "jedno")

lmer.dat1 <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat1)
summary (lmer.dat1)

lmer.dat1.a <- lmer(RT ~ poly(TrialOrder,2) + len.z + fsuf.z + fpov.z + NV + (1|Ispitanik) + (0+len.z|Ispitanik) +(1|Stimulus), data=dat1, subset=abs(scale(resid(lmer.dat1)))<2.5)
summary (lmer.dat1.a)

#Random effects:
# Groups      Name        Variance Std.Dev.
# Stimulus    (Intercept) 0.008293 0.09106 
# Ispitanik   len.z       0.002130 0.04615 
# Ispitanik.1 (Intercept) 0.037307 0.19315 
# Residual                0.057518 0.23983 
#Number of obs: 1524, groups:  Stimulus, 70; Ispitanik, 46
#
#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)          -1.428e+00  3.786e-02  9.170e+01 -37.706  < 2e-16 ***
#poly(TrialOrder, 2)1 -3.669e-01  2.486e-01  1.410e+03  -1.476  0.14024    
#poly(TrialOrder, 2)2  4.126e-01  2.490e-01  1.412e+03   1.657  0.09780 .  
#len.z                 4.103e-02  1.444e-02  7.520e+01   2.841  0.00578 ** 
#fsuf.z                2.407e-03  1.842e-02  6.230e+01   0.131  0.89647    
#fpov.z               -9.907e-02  1.385e-02  6.410e+01  -7.155 9.85e-10 ***
#NVvise                7.697e-03  3.448e-02  6.160e+01   0.223  0.82410       

### nikakvih znacajnih promena

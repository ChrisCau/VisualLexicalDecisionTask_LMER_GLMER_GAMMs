# Loading packages.
require(rms)
require(lme4)
require(languageR)
require(multcomp)
require(lsmeans)
require(multcompView)
require(pbkrtest)
require(lmerTest)
require(party)
require(MASS)
require(ggplot2)
require(mgcv)
require(itsadug)
require(psych)
require(car)
require(lattice)


# Ucitavam tabelu, koja mi je smestena u Documents

dat=read.table("viseznacnost_vld_finalno.txt", header=TRUE, quote="\"")

dim(dat)
#[1] 2023   18

# Da proverim jos jednom da li je ucitano sve dobro, proveravam 

colnames(dat)

# [1] "Ispitanik"            "TrialOrder"           "Grupa"               
# [4] "CorrectResponse"      "TrialNumber"          "Sufiks"              
# [7] "DuzinaSufiksa"        "Response"             "Tacnost"             
#[10] "RT"                   "NV"                   "Stimulus"            
#[13] "FrekLemKostic"        "PovFrekWac"           "FrekLemWac"          
#[16] "DuzinaReci"           "FrekvencijaSufiksa"   "ProduktivnostSufiksa"

# Sada polako treba da izbacim greske

# Sto se tice izbacivanje ispitanika, u psiholingvistici bas i ne postoji koncenzus koji procenat gresaka je znak za izbacivanje, ali vecina radi na 20% gresaka. Moje
# licno misljenje je da je to mozda malo labaviji kriterijum, ali kada je ovako mali uzorak (a u psiholingvistici svi su mali) nije student koliko da izbaci, ja cu na 20% gresaka.

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

dat=dat[dat$Ispitanik!="s28",]
dat=dat[dat$Ispitanik!="s31",]
dat=dat[dat$Ispitanik!="s40",]

# Izbacena tri ispitanika.
----
# Ista prica je i sa stimulusima, uzecu kao kriterijum 25% posto je ogromna kolicina gresaka na malom uzorku, mislim da nema bas logike da uzmem nesto stroziji kriterijum.

sort(tapply(dat$Tacnost, dat$TrialNumber, sum)/23)

#       100        109         17        126         37         23         98 
#0.08695652 0.08695652 0.39130435 0.43478261 0.47826087 0.56521739 0.56521739 
#       115        129        130        117        119         27         96 
#0.60869565 0.60869565 0.60869565 0.65217391 0.65217391 0.69565217 0.69565217 
#       105        124         91        103        116         25         35 
#0.69565217 0.69565217 0.73913043 0.73913043 0.73913043 0.78260870 0.78260870 
#        38         94         99        106        113        122        128 
#0.78260870 0.78260870 0.78260870 0.78260870 0.78260870 0.78260870 0.78260870 
#        10         89         90         92         93         95         97 
#0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 
#       101        102        104        107        108        110        111 
#0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 
#       112        114        118        120        121        123        125 
#0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 0.82608696 
#       127        131        132          6         19          2          7 
#0.82608696 0.82608696 0.82608696 0.86956522 0.91304348 0.95652174 0.95652174 
#        21          1          5          9         13         24         30 
#0.95652174 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 
#        31         34         40         44          3          4          8 
#1.00000000 1.00000000 1.00000000 1.00000000 1.04347826 1.04347826 1.04347826 
#        11         12         14         15         16         18         20 
#1.04347826 1.04347826 1.04347826 1.04347826 1.04347826 1.04347826 1.04347826 
#        22         26         28         29         32         33         36 
#1.04347826 1.04347826 1.04347826 1.04347826 1.04347826 1.04347826 1.04347826 
#        39         41         42         43 
#1.04347826 1.04347826 1.04347826 1.04347826

#Izbacivanje stimulusa sa vise od 25% gresaka

dat=dat[dat$TrialNumber!="100",]
dat=dat[dat$TrialNumber!="109",]
dat=dat[dat$TrialNumber!="17",]
dat=dat[dat$TrialNumber!="126",]
dat=dat[dat$TrialNumber!="37",]
dat=dat[dat$TrialNumber!="23",]
dat=dat[dat$TrialNumber!="98",]
dat=dat[dat$TrialNumber!="115",]
dat=dat[dat$TrialNumber!="129",]
dat=dat[dat$TrialNumber!="130",]
dat=dat[dat$TrialNumber!="117",]
dat=dat[dat$TrialNumber!="119",]
dat=dat[dat$TrialNumber!="27",]
dat=dat[dat$TrialNumber!="96",]
dat=dat[dat$TrialNumber!="105",]
dat=dat[dat$TrialNumber!="124",]
dat=dat[dat$TrialNumber!="91",]
dat=dat[dat$TrialNumber!="103",]
dat=dat[dat$TrialNumber!="116",]

dim(dat)
#[1] 1510   18

(2023-1510)/2023
#[1] 0.2535838

# Bas velika kolicina podataka koja je izbacena, ali to je problem eksperimentalnih istrazivanja jezika. :-(

-----------------------------------------

# Ispitujem po kojim kovarijablama se grupe razlikuju (imam dve grupe eksperimentalne, objasnjeno u radu). Uzmemo dva ispitanika (jednog iz jedne grupe, drugog iz druge),
# i napravimo kao neki maleni podskup, i na njemu radimo dalje, da ih ne selektujem sve (to sam negde na netu videla da se tako radi).

datx=dat[dat$Ispitanik=="s9" | dat$Ispitanik=="s38",]

dim(datx)
#69 18

----------------------
#Duzina reci (kovarijabla 1)
----------------------

aov.datx= aov(DuzinaReci~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(DuzinaReci~NV, data=datx)

#            Df Sum Sq Mean Sq F value Pr(>F)  
#NV           1   8.07   8.067   3.913  0.052 .
#Residuals   67 138.14   2.062                
      
#    jedno  vise
#     7.77  7.08
#rep 30.00 39.00

# Duzina reci marginalno statisticki znacajna, to je ok, dakle skoro da se ne razlikuju.

-------------------------
#Frekvencija leme (srWac) (kovarijabla 2)
----------------------

aov.datx= aov(FrekLemWac~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(FrekLemWac~NV, data=datx)

#            Df    Sum Sq   Mean Sq F value Pr(>F)  
#NV           1 7.527e+08 752655036   2.931 0.0915 .
#Residuals   67 1.721e+10 256825280 

#    jedno  vise
#     4812 11474
#      30    39

# Ovde nemamo razliku.

-------------------------
#Frekvencija povrsinska (srWac) (kovarijabla 3)
----------------------

aov.datx= aov(PovFrekWac~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(PovFrekWac~NV, data=datx)

#            Df    Sum Sq  Mean Sq F value Pr(>F)
#NV           1 9.083e+07 90828467   2.694  0.105
#Residuals   67 2.259e+09 33709095  

#    jedno vise
#     1213 3527
#      30   39

# Takodje, ni ovde.

-------------------------
#Frekvencija sufiksa (kovarijabla 4)
-------------------------

aov.datx= aov(FrekvencijaSufiksa~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(FrekvencijaSufiksa~NV, data=datx)

#            Df    Sum Sq  Mean Sq F value Pr(>F)
#NV           1 8.735e+05   873476   0.031  0.861
#Residuals   67 1.904e+09 28411434 

#    jedno vise
#     4280 4507
#      30   39

# Ni ovde nema razlike.

-------------------------
#Duzina sufiksa (kovarijabla 5)
-------------------------

aov.datx= aov(DuzinaSufiksa~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(DuzinaSufiksa~NV, data=datx)

#            Df Sum Sq Mean Sq F value Pr(>F)
#NV           1   0.61  0.6105   0.903  0.345
#Residuals   67  45.30  0.6762  

#    jedno  vise
#     2.93  2.74
#    30.00 39.00

# Nema ni ovde, to je super.

-------------------------
#Produktivnost sufiksa (kovarijabla 6)
-------------------------

aov.datx= aov(ProduktivnostSufiksa~NV, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(ProduktivnostSufiksa~NV, data=datx)

#            Df   Sum Sq Mean Sq F value Pr(>F)  
#NV           1   768986  768986   3.587 0.0626 .
#Residuals   67 14365074  214404

#    jedno vise
#      409  196
#      30   39

# Nema ni ovde.
_____________________________________________
__________________________________________________________

# Nastavljam da sredjujem podatke.
# Izbacivanje netacnih odgovora (dodatno ciscenje, posto ne ociste mi se gore svi).

dat=dat[dat$Tacnost>0,]

dim(dat)
#[1] 1459   18

(2023-1459)/2023
#[1] 0.2787939

# Vizulena inspekcija podataka nakon svih ovih ciscenja, da vidimo kako izgledaju sada.

par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))

# Normalnost distribucije RT-ova sada testiram, iako bez izuzetaka ona nije ni priblizna normalnoj nikada.

shapiro.test(dat$RT)

#        Shapiro-Wilk normality test
#
#data:  dat$RT
#W = 0.81251, p-value < 2.2e-16

ks.test(jitter(dat$RT),"pnorm",mean(dat$RT),sd(dat$RT))

#        One-sample Kolmogorov-Smirnov test
#
#data:  jitter(dat$RT)
#D = 0.12999, p-value < 2.2e-16
#alternative hypothesis: two-sided

---

# U psiholingvistici skoro sve kovarijable (nase sve) se logaritmuju, a kada se RT-ovi mere u zadatku vizuelen leksicke odluke (izolovano prikazvanje) onda se i RT ispegla inverz transformacijom (Baayen&Milin, 2010)
# RT je dosta problematicna varijabla zato sto je distribucija naravno uvek zakrivljena i to bez izuzetaka na levo.

dat$dlem=log(dat$DuzinaReci)
dat$flemw=log(dat$FrekLemWac)
dat$fpov=log(dat$PovFrekWac)
dat$fsuf=log(dat$FrekvencijaSufiksa)
dat$dsuf=log(dat$DuzinaSufiksa)
dat$psuf=log(dat$ProduktivnostSufiksa)

# Inverzna transformacija RT

dat$RT=-1000/dat$RT

# Sada druga vizuelna inspekcija podataka, da vidim kako izgledaju.

par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))

# Priblizili smo je normalnoj, koliko je to bilo moguce.

----

# Normalizujem kontinuirane prediktore, da bismo mogli lepo da ih poredimo.

dat$trial.z = scale(dat$TrialOrder)
dat$len.z = scale(dat$dlem)
dat$flemw.z=scale(dat$flemw)
dat$fpov.z=scale(dat$fpov)
dat$fsuf.z = scale(dat$fsuf)
dat$dsuf.z = scale(dat$dsuf)
dat$psuf.z = scale(dat$psuf)

# Pobrinem se da je NV tretiran kao faktor, isto kao i slucajni efekti (Stimulusi i ispitanici)

as.factor(as.character(dat$NV))
levels(dat$NV)
table(dat$NV)

#jedno  vise 
#  638   821

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

# Mozda nisam gore nigde napisala, Trial order se isto preporucuje da se uvrsti u analize podataka iz psiholingvistickih studija, kada se koristi 
# i slucajni i fiksni efekti.

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

_________________________________________________________
________________________________________________________________

# Kolinearnost medju prediktorima (standardno OGROMNA u psiholingvistickim studijama, ali pokusacemo da izvuceno najboljeza ovaj model).

C=cov(dat[,c("flemw", "fpov", "fsuf", "dlem","dsuf", "psuf")], y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
Cor=cov2cor(C)
Cor

#            flemw         fpov        fsuf         dlem         dsuf
#flemw  1.00000000  0.905425586  0.41265547 -0.013981066  0.038573858
#fpov   0.90542559  1.000000000  0.40709944 -0.031518323  0.004466912
#fsuf   0.41265547  0.407099445  1.00000000 -0.087493564 -0.249219862
#dlem  -0.01398107 -0.031518323 -0.08749356  1.000000000  0.606713809
#dsuf   0.03857386  0.004466912 -0.24921986  0.606713809  1.000000000
#psuf   0.29359685  0.324805426  0.91182233 -0.009582447 -0.192017732
#              psuf
#flemw  0.293596848
#fpov   0.324805426
#fsuf   0.911822333
#dlem  -0.009582447
#dsuf  -0.192017732
#psuf   1.000000000

# Nakon redukcije

collin.fnc(dat[,c("flemw", "dlem", "psuf")])$cnumber

30.07801

######### PODACI SPREMNI ZA MODELOVANJE

_______________________________________________________________________________
_________________________________________________________________________________________________________

################################################################# KRECEM POLAKO DA MODELUJEM -- GAMMs (po onom tutorijalu)

#### 1 korak u tutorijalu (vizualizacija podataka za grupe kategorijalne NV)

aggregate(RT ~ NV, data=dat, mean, na.rm=TRUE)

#     NV        RT
#1 jedno -1.403996
#2  vise -1.467176

plot(RT ~ NV, data = dat)

----

#### Na osnovu onog tutorijala, ja sada polako pravim neke modele i poredim koji je od njih bolji. Koji metod sam koristila za proedjenje pise sve u onom wordu.

gam1 <- gam(RT ~ NV +
            trial.z +
            len.z +
            flemw.z +
            psuf.z +
            s(Stimulus, bs="re") +
            s(trial.z, Ispitanik, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam1)

#Parametric coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.4270977  0.0352053 -40.536  < 2e-16 ***
#NVvise       0.0040909  0.0279693   0.146  0.88374    
#trial.z     -0.0001046  0.0100995  -0.010  0.99174    
#len.z        0.0415640  0.0130459   3.186  0.00148 ** 
#flemw.z     -0.0876636  0.0140483  -6.240 5.88e-10 ***
#psuf.z       0.0120262  0.0134908   0.891  0.37286    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                       edf Ref.df     F p-value    
#s(Stimulus)          45.99     64 2.561  <2e-16 ***
#s(trial.z,Ispitanik) 88.41    386 2.350  <2e-16 ***

######## JEEEEEEE napravila sam prvi model, idem dalje!

--------

#### Hm, ovde sam morala da redukujem k (koji je inace 10 kao default, ali meni nesto nije hteo, moram da razmislim do petka zasto)
gam2 <- gam(RT ~ NV +
            trial.z +
            s(len.z, k = 8) +
            flemw.z +
            psuf.z +
            s(Stimulus, bs="re") +
            s(trial.z, Ispitanik, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam2)

-----

Poredim prvi i drugi gam! Malo mi je ovo citanje ludilo totalno, ali ajde da probam.

compareML(gam1,gam2)
anova(gam1,gam2,test="Chisq")

#Chi-square test of REML scores
#-----
#  Model    Score Edf Difference    Df p.value Sig.
#1  gam1 200.1498   9                              
#2  gam2 200.0753  10      0.074 1.000   0.700     
#
#AIC difference: -3.46, model gam1 has lower AIC.

#  Resid. Df Resid. Dev     Df  Deviance Pr(>Chi)
#1    1269.7     81.875                          
#2    1268.1     81.893 1.5578 -0.018115 

#### Totalno mi je zbunjujuce sve ovo, ali mislim da mi je drugi model bolji.

------

gam3 <- gam(RT ~ NV +
            trial.z  +
            s(len.z, k = 8) +
            s(flemw.z) +
            psuf.z +
            s(Stimulus, bs="re") +
            s(trial.z, Ispitanik, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam3) 

compareML(gam2,gam3)
anova(gam2,gam3,test="Chisq")

#Chi-square test of REML scores
#-----
#  Model    Score Edf Difference    Df p.value Sig.
#1  gam2 200.0753  10                              
#2  gam3 199.3445  11      0.731 1.000   0.227     
#
#AIC difference: 2.66, model gam3 has lower AIC.

#  Resid. Df Resid. Dev       Df Deviance Pr(>Chi)
#1    1268.1     81.893                           
#2    1268.8     81.860 -0.66804 0.032941  

#### Opet mi je malo zbunjujuce, ali izgleda da mi je opet bolji drugi (tj. gam3)

-------

gam4 <- gam(RT ~ NV +
            trial.z  +
            s(len.z, k = 8) +
            s(flemw.z) +
            s(psuf.z) +
            s(Stimulus, bs="re") +
            s(trial.z, Ispitanik, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam4)

compareML(gam3,gam4)
anova(gam3,gam4,test="Chisq")

#Chi-square test of REML scores
#-----
#  Model    Score Edf Difference    Df p.value Sig.
#1  gam3 199.3445  11                              
#2  gam4 199.2082  12      0.136 1.000   0.602  

#  Resid. Df Resid. Dev     Df  Deviance Pr(>Chi)
#1    1268.8     81.860                          
#2    1267.7     81.878 1.1042 -0.018215 

##### Opet drugi bolji izgleda

-----

###### Malo samo da nesto probam jos i odabiram najbolji

gam5 <- gam(RT ~ NV +
            trial.z  +
            s(len.z, k = 8) +
            s(flemw.z) +
            s(psuf.z, by=NV) +
            s(Stimulus, bs="re") +
            s(trial.z, Ispitanik, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam5)

compareML(gam4,gam5)
anova(gam4,gam5,test="Chisq")

#  Model    Score Edf Difference     Df
#1  gam5 201.1488  14                  
#2  gam4 199.2082  12      1.941 -2.000

#  Resid. Df Resid. Dev     Df Deviance Pr(>Chi)
#1    1267.7     81.878                         
#2    1266.1     81.848 1.5913 0.029829   0.6884

######### E ovde je prvi bolji, bez prilagodjavanja produktivnosti za NV!

-----

######### REFITOVANJE +/- 2.5! gam4 je najbolji!

####################### NAJBOLJI MODEL, ISPOD JE REFITOVANNNNNNN!

gam4 <- gam(RT ~ NV +
            trial.z  +
            s(len.z, k = 8) +
            s(flemw.z) +
            s(psuf.z) +
            s(Stimulus, bs="re") +
            s(trial.z, Ispitanik, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam4)

gam4.a <- gam(RT ~ NV +
            trial.z  +
            s(len.z, k = 8) +
            s(flemw.z) +
            s(psuf.z) +
            s(Stimulus, bs="re") +
            s(trial.z, Ispitanik, bs="fs", m=1),
            method="REML",
            data=dat, subset=abs(scale(resid(gam4)))<2.5)
summary(gam4.a)

#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.442631   0.035029 -41.184   <2e-16 ***
#NVvise       0.015829   0.027380   0.578    0.563    
#trial.z     -0.005771   0.009953  -0.580    0.562    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                        edf  Ref.df      F  p-value    
#s(len.z)              1.000   1.000 10.007  0.00159 ** 
#s(flemw.z)            2.191   2.318 18.486 2.14e-09 ***
#s(psuf.z)             1.000   1.000  0.303  0.58189    
#s(Stimulus)          45.924  64.000  2.647  < 2e-16 ***
#s(trial.z,Ispitanik) 92.370 386.000  2.644  < 2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

------------

ZA KRAJ, VREMENSKE SERIJE (kako pise u tutorijalu)!

start_event(data=dat, column="RT", event=c("trial.z", "Ispitanik"))
head(dat)

(valRho <- acf(resid(gam4), plot=FALSE)$acf[2])

# -0.03078674
 
gam4.rho <- gam(RT ~ NV +
            trial.z  +
            s(len.z, k = 8) +
            s(flemw.z) +
            s(psuf.z) +
            s(Stimulus, bs="re") +
            s(trial.z, Ispitanik, bs="fs", m=1),
            method="REML",
            data=dat,
        AR.start=dat$start_eventt, rho=valRho)
summary(gam4.rho)

###### Mada meni rho ovde nije visok kao njoj u prezentaciji, mozda meni ovo nije ni bilo potrebno.

#Parametric coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.4364242  0.0353741 -40.607   <2e-16 ***
#NVvise       0.0202405  0.0293484   0.690    0.491    
#trial.z     -0.0001346  0.0101214  -0.013    0.989    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                        edf  Ref.df      F  p-value    
#s(len.z)              1.002   1.002  7.797  0.00527 ** 
#s(flemw.z)            2.188   2.331 19.449 7.32e-10 ***
#s(psuf.z)             1.646   1.760  0.545  0.43546    
#s(Stimulus)          43.375  64.000  2.206  < 2e-16 ***
#s(trial.z,Ispitanik) 88.708 386.000  2.338  < 2e-16 ***

######## Isti rezultati i kada nema rho

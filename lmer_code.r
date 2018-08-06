# Loading the packages.
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

# Loading the data set.
dat=read.table("vld_final.txt",T)

# Dimensions of the data set (number of rows [data], number of columns [variables]).
dim(dat)
#[1] 2023   16

# Column names.
colnames(dat)

# [1] "Subject"            "TrialOrder"         "Group"             
# [4] "CorrectResponse"    "TrialNumber"        "Suffix"            
# [7] "SuffixLength"       "Response"           "Accuracy"          
# [10] "RT"                 "SuffixAmbiguity"    "Noun"              
# [13] "LemmaFrequency"     "NounLength"         "SuffixFrequency"   
# [16] "SuffixProductivity"

# ------------------
# -------------------------------- PREPARING DATA FOR STATISTICAL ANALYSIS!
# ------------------

# Sorting the participants according to the accuracy of the answers they gave. 
sort(tapply(dat$Accuracy, dat$Subject, sum)/44)

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

# Sorting the stimuli according to the accuracy of the answers participants gave on it. 
sort(tapply(dat$Accuracy, dat$TrialNumber, sum)/23)

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

# Note: Now I need to throw out the participants and stimuli with above 25% error-rate! All participants have less than 25% error-rate!

# Deletion of stimuli (25% of the error)!
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

# Dimensions of the data set (number of rows [data], number of columns [variables]).
dim(dat)
#[1] 1707   16

# Calculating the percentage of deleted data.
(2023-1707)/2023
#[1] 0.1562037

# -----------------------------------------

# I have to examine in which covariates the ambiguous and unambiguous suffixes differe (e.g. Are ambiguous suffixes longer, or more frequent [stat.sign.]?!). I will create subset with 
# data of two participants from different experimental groups (because it would be too long to look at the whole sample, and also it is useless).

datx=dat[dat$Subject=="s9" | dat$Subject=="s38",]

# Dimensions of subest.
dim(datx)
#74 16

# ----------------------
##################### Noun length (in letters)
# ----------------------

aov.datx= aov(NounLength~SuffixAmbiguity, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(NounLength~SuffixAmbiguity, data=datx)

#                Df Sum Sq Mean Sq F value Pr(>F)  
#SuffixAmbiguity  1   8.56   8.564   4.088 0.0469 *
#Residuals       72 150.84   2.095                 
        
#    ambiguous unambiguous
#         7.07        7.76
#        41.00       33.00

# Note: This effect is marginally statistically significant (p = 0.04). This results suggest that unambiguous suffixes are in general longer than ambiguous ones.

# -------------------------
##################### Lemma Frequency (taken from the corpus srWac)
# -------------------------

aov.datx= aov(LemmaFrequency~SuffixAmbiguity, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(LemmaFrequency~SuffixAmbiguity, data=datx)

#                Df    Sum Sq   Mean Sq F value Pr(>F)  
#SuffixAmbiguity  1 7.865e+08 786503093   3.235 0.0763 
#Residuals       72 1.750e+10 243114598 

#     ambiguous unambiguous
#         10934        4375
# rep        41          33

# Note: This effect is not statistically significant, which means that nouns with ambiguous and unambiguous suffixes do not differe in the frequency of lemmas.

# -------------------------
##################### Suffix frequency (taken from the database created by me and my colleagues)
# -------------------------

aov.datx= aov(SuffixFrequency~SuffixAmbiguity, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(SuffixFrequency~SuffixAmbiguity, data=datx)

#                Df    Sum Sq  Mean Sq F value Pr(>F)
#SuffixAmbiguity  1 7.809e+06  7808535   0.288  0.593
#Residuals       72 1.950e+09 27089474

#    ambiguous unambiguous
#         4578        3925
#rep        41          33

# Note: This effect is not statistically significant, which means that nouns with ambiguous and unambiguous suffixes do not differe in the suffix frequency.

# -------------------------
##################### Suffix length (in letters)
# -------------------------

aov.datx= aov(SuffixLength~SuffixAmbiguity, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(SuffixLength~SuffixAmbiguity, data=datx)

#                Df Sum Sq Mean Sq F value Pr(>F)
#SuffixAmbiguity  1   1.04  1.0356   1.461  0.231
#Residuals       72  51.02  0.7086   

#    ambiguous unambiguous
#         2.73        2.97
#rep     41.00       33.00

# Note: This effect is not statistically significant, which means that nouns with ambiguous and unambiguous suffixes do not differe in the suffix length.

# -------------------------
##################### Suffix Productivity (taken from the database created by me and my colleagues)
# -------------------------

aov.datx= aov(SuffixProductivity~SuffixAmbiguity, data=datx)
summary (aov.datx)
print(model.tables(aov.datx, "means"),digits=3)
boxplot(SuffixProductivity~SuffixAmbiguity, data=datx)

#            Df   Sum Sq Mean Sq F value Pr(>F)  
#NV           1   593524  593524   2.905 0.0926
#Residuals   72 14708156  204280

#    jedno vise
#      377  197
#      33   41

# Note: This effect is not statistically significant, which means that nouns with ambiguous and unambiguous suffixes do not differe in the suffix productivity.
# ______
# _______________________________________
# __________________________________________________________

# I need to throw out the RTs that are errors.
dat=dat[dat$Accuracy>0,]

# Dimension of the data set (number of rows [data], number of columns [variables]).
dim(dat)
#[1] 1633   16

# Calculating the percentage of deleted data.
(2023-1633)/2023
#[1] 0.192783

# Visual inspection of data (RTs).
par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))

# I have to examine RTs with powerTransform() to see which transformation I have to use (distribution of RTs is not normal). So, if the value is close to zero
# the best transformation is log tranformation, if the value is close to one, than we need inverse transformation of raw RTs. 
powerTransform(dat$RT)
#Estimated transformation parameters 
#   dat$RT 
#-1.314563 

# Note: the results suggest inverse transformation of RTs.

# Following previous studies, I will use log transformation to transform the raw values of LemmaFrequency, SuffixFrequency, SuffixLength, NounLength and SuffixProductivity.
dat$flem=log(dat$LemmaFrequency)
dat$nlen=log(dat$NounLength)
dat$fsuf=log(dat$SuffixFrequency)
dat$slen=log(dat$SuffixLength)
dat$sprod=log(dat$SuffixProductivity)

# Inverse transformation of RTs. 
dat$RT=-1000/dat$RT

# Visual inspection of data (RTs), after transformation.
par(mfrow=c(2,2))
plot(sort(dat$RT))
plot(density(dat$RT))
qqnorm(dat$RT)
par(mfrow=c(1,1))

# Note: this distribution is closer to normal.

# ------- Shapiro-Wilk & Kolmogorov-Smirnov after transformation of RTs (this step has been skipped earlier, because on the basis of VIP1 graphics I saw that distribution deviates exceptionally from normal [before transformation]).

# I will use K-S test to test normality of transformed RTs distribution.
ks.test(jitter(dat$RT),"pnorm",mean(dat$RT),sd(dat$RT))
#        One-sample Kolmogorov-Smirnov test
#
#data:  jitter(dat$RT)
#D = 0.041749, p-value = 0.006742
#alternative hypothesis: two-sided

# Note: This test is statistically significant, and this suggest that our sample distribution is different than reference probability distribution [distribution is not normal].

# I continue to deal with the distribution, and it's normality. I will use S-W test, and this test is more powerful than K-S test. If S-W test is significant (p-value), than distribution is not normal. 
shapiro.test(dat$RT)
#        Shapiro-Wilk normality test
#
#data:  dat$RT
#W = 0.99274, p-value = 3.377e-07

# Note: It is significant, distribution is not normal, even after the transformation. In psycholinguistics studies, this is very often the case.

# Normalization of continuous precitors, to be comparable on the same scale.
dat$trial.z = scale(dat$TrialOrder)
dat$flem.z = scale(dat$flem)
dat$nlen.z = scale(dat$nlen)
dat$fsuf.z = scale(dat$fsuf)
dat$slen.z = scale(dat$slen)
dat$sprod.z = scale(dat$sprod)

# Factor as factor (SuffixAmbiguity).
as.factor(as.character(dat$SuffixAmbiguity))
levels(dat$SuffixAmbiguity)
table(dat$SuffixAmbiguity)

#  ambiguous unambiguous 
#        911         722

# Factor as factor (Subject).
as.factor(as.character(dat$Subject))
levels(dat$Subject)
table(dat$Subject)

# Factor as factor (TrialNumber).
as.factor(as.character(dat$TrialNumber))
levels(dat$TrialNumber)
table(dat$TrialNumber)

# __________________________________________________________
# ________________________________________________________________

# Visualization of continuous predictors.

# -------------------------
##################### Trial order
# -------------------------

par(mfrow=c(2,2))
plot(sort(dat$TrialOrder))
plot(density(dat$TrialOrder))
qqnorm(dat$TrialOrder)
par(mfrow=c(1,1))

# -------------------------
##################### Noun length
# -------------------------

par(mfrow=c(2,2))
plot(sort(dat$nlen))
plot(density(dat$nlen))
qqnorm(dat$nlen)
par(mfrow=c(1,1))

# -------------------------
##################### Lemma frequency
# -------------------------

par(mfrow=c(2,2))
plot(sort(dat$flem))
plot(density(dat$flem))
qqnorm(dat$flem)
par(mfrow=c(1,1))

# -------------------------
##################### Suffix frequency
# -------------------------

par(mfrow=c(2,2))
plot(sort(dat$fsuf))
plot(density(dat$fsuf))
qqnorm(dat$fsuf)
par(mfrow=c(1,1))

# -------------------------
##################### Suffix length
# -------------------------

par(mfrow=c(2,2))
plot(sort(dat$slen))
plot(density(dat$slen))
qqnorm(dat$slen)
par(mfrow=c(1,1))

# -------------------------
##################### Suffix productivity
# -------------------------

par(mfrow=c(2,2))
plot(sort(dat$sprod))
plot(density(dat$sprod))
qqnorm(dat$sprod)
par(mfrow=c(1,1))

# __________________________________________________________
# ____________________________________________________________

# Visual inspection of random effects.

qqmath(~RT|Subject,data=dat)
qqmath(~RT|TrialNumber,data=dat)
xylowess.fnc (RT~TrialOrder | Subject, data=dat, ylab= "RT")

# ____________________________________________________________
# ____________________________________________________________

# Examine the data for Suffix frequency, we have one outlier (-nje) with frequency = 23567.
table(dat$SuffixFrequency)

#   46    73    81    88    91   170   424   464   537   868   913  2140  2280 
#   21    41    21    23    24    38    43    24    46   217    24    92    68 
# 3001  3200  3764  3765  5345  6155  6585 23567 
#  181    40    91    24   173   129   224    89

# _________________________________________________________
# ________________________________________________________________

# Colinearity between predictors.
C=cov(dat[,c("flem","nlen","fsuf","slen","sprod")], y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
Cor=cov2cor(C)
Cor

#             flem        nlen       fsuf        slen       sprod
#flem   1.00000000 -0.02192262  0.4623827  0.01930949  0.34034109
#nlen  -0.02192262  1.00000000 -0.1175926  0.62876318 -0.02878838
#fsuf   0.46238265 -0.11759259  1.0000000 -0.27074274  0.91055550
#slen   0.01930949  0.62876318 -0.2707427  1.00000000 -0.20600849
#sprod  0.34034109 -0.02878838  0.9105555 -0.20600849  1.00000000

collin.fnc(dat[,c("flem","nlen","fsuf","slen","sprod")])$cnumber

# This value is too big!
#48.82148

# Reduced, but still too big! This suggest that GAMMs might be more appropriate analysis for this data.
collin.fnc(dat[,c("flem","nlen","sprod")])$cnumber
#29.71396

# Note: I exscluded SuffixFrequency and SuffixLength.

# Visualization of multicolinearity (3 predictors).
postscript("isidora.pairscor1.ps", width=16, height=16,paper="special",horizontal=FALSE,onefile=FALSE)
png("isidora.pairscor1.png", width=800, height=800)
pairscor.fnc(dat[,c("flem","nlen","sprod")], hist=TRUE, smooth=TRUE, cex.point=1, col.points="darkgrey")
dev.off()

# Visualization of multicolinearity (5 predictors).
postscript("isidora1.pairscor1.ps", width=16, height=16,paper="special",horizontal=FALSE,onefile=FALSE)
png("isidora1.pairscor1.png", width=800, height=800)
pairscor.fnc(dat[,c("flem","nlen","fsuf","slen","sprod")], hist=TRUE, smooth=TRUE, cex.point=1, col.points="darkgrey")
dev.off()

# ______________________________
# _____________________________________________
# ____________________________________________________________

# ------------------
# -------------------------------- LMER ANALYSIS!
# ------------------

# ________________________
# _____________________________________________
# ____________________________________________________________

################################################################# LMER: Linear Mixed-Effect Regression!

########## Random effects!
lmer0 <- lmer(RT ~ 1 + (1|Subject) + (1|TrialNumber),	data=dat)
ranefItem <- lmer(RT ~ 1 + (1|Subject),	data=dat)
ranefSubj <- lmer(RT ~ 1 + (1|TrialNumber), data=dat)

anova(ranefItem, lmer0)

#object: RT ~ 1 + (1 | Subject)
#..1: RT ~ 1 + (1 | Subject) + (1 | TrialNumber)
#       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  3 730.20 746.39 -362.10   724.20                             
#..1     4 502.38 523.98 -247.19   494.38 229.81      1  < 2.2e-16 ***

# Note: The second model is better (AIC and BIC is smaller, LogLik is bigger).

anova(ranefSubj, lmer0)

#object: RT ~ 1 + (1 | TrialNumber)
#..1: RT ~ 1 + (1 | Subject) + (1 | TrialNumber)
#       Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#object  3 1043.87 1060.06 -518.93  1037.87                             
#..1     4  502.38  523.98 -247.19   494.38 543.48      1  < 2.2e-16 ***

# Note: The second model is better (AIC and BIC is smaller, LogLik is bigger).

# __________________________________________________________
# ________________________________________________________________

############# TRIAL ORDER 

lmer.dat <- lmer(RT ~ trial.z + (1|Subject) + (1|TrialNumber),	data=dat)
summary (lmer.dat)

#Fixed effects:
#              Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept) -1.411e+00  3.303e-02  6.970e+01 -42.712   <2e-16 ***
#trial.z     -2.744e-03  6.509e-03  1.528e+03  -0.422    0.673 

# Note: not significant.

lmer.dat1 <- lmer(RT ~ poly(TrialOrder, 2) + (1|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat1)

#Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)            -1.41090    0.03312   69.50000 -42.602  < 2e-16 ***
#poly(TrialOrder, 2)1   -0.11370    0.26238 1526.80000  -0.433  0.66482    
#poly(TrialOrder, 2)2    0.72977    0.26288 1528.40000   2.776  0.00557 ** 

# I set REML = FALSE, because the models differ in fixed effects.
lmer.dat.a=update(lmer.dat, REML=FALSE)
lmer.dat1.a=update(lmer.dat1, REML=FALSE)

anova(lmer.dat.a,lmer.dat1.a)

#lmer.dat.a: RT ~ trial.z + (1 | Subject) + (1 | TrialNumber)
#lmer.dat1.a: RT ~ poly(TrialOrder, 2) + (1 | Subject) + (1 | TrialNumber)
#            Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
#lmer.dat.a   5 504.21 531.20 -247.10   494.21                            
#lmer.dat1.a  6 498.52 530.91 -243.26   486.52 7.6902      1   0.005552 **

# Note: the second model is better, the one with POLY function of TrialOrder.

# ------------------------
################ Embbeding of other predictors in LMER model.

lmer.dat2 <- lmer(RT ~ poly(TrialOrder,2) + SuffixAmbiguity + (1|Subject) +(1|TrialNumber), data=dat)
summary (lmer.dat2)

lmer.dat3 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z + SuffixAmbiguity + (1|Subject) +(1|TrialNumber), data=dat)
summary (lmer.dat3)

lmer.dat4 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z + flem.z + SuffixAmbiguity + (1|Subject) +(1|TrialNumber), data=dat)
summary (lmer.dat4)

lmer.dat5 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z + fsuf.z + flem.z + SuffixAmbiguity + (1|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat5)

# I will check the interactions between predictors.
lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z + flem.z + fsuf.z*SuffixAmbiguity + (1|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                                    Estimate Std. Error         df t value
#(Intercept)                         -1.42338    0.03580   83.80000 -39.763
#poly(TrialOrder, 2)1                -0.14721    0.26190 1536.40000  -0.562
#poly(TrialOrder, 2)2                 0.72810    0.26223 1541.20000   2.777
#nlen.z                               0.04421    0.01224   66.40000   3.611
#flem.z                              -0.09973    0.01392   66.90000  -7.165
#fsuf.z                               0.03153    0.03330   64.00000   0.947
#SuffixAmbiguityunambiguous           0.01309    0.02986   64.60000   0.438
#fsuf.z:SuffixAmbiguityunambiguous   -0.01521    0.03754   64.30000  -0.405
#                                  Pr(>|t|)    
#(Intercept)                        < 2e-16 ***
#poly(TrialOrder, 2)1              0.574134    
#poly(TrialOrder, 2)2              0.005560 ** 
#nlen.z                            0.000587 ***
#flem.z                            7.71e-10 ***
#fsuf.z                            0.347403    
#SuffixAmbiguityunambiguous        0.662579    
#fsuf.z:SuffixAmbiguityunambiguous 0.686747 

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

#lmer.dat5.a: RT ~ poly(TrialOrder, 2) + nlen.z + fsuf.z + flem.z + SuffixAmbiguity + 
#lmer.dat5.a:     (1 | Subject) + (1 | TrialNumber)
#lmer.dat6.a: RT ~ poly(TrialOrder, 2) + nlen.z + flem.z + fsuf.z * SuffixAmbiguity + 
#lmer.dat6.a:     (1 | Subject) + (1 | TrialNumber)
#            Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#lmer.dat5.a 10 449.88 503.86 -214.94   429.88                         
#lmer.dat6.a 11 451.71 511.09 -214.85   429.71 0.1748      1     0.6758

# Note: the first model is better.

# ------
# I will check the interactions between some other predictors.
lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z + fsuf.z + flem.z*SuffixAmbiguity + (1|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                                    Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                       -1.414e+00  3.383e-02  7.330e+01 -41.784  < 2e-16 ***
#poly(TrialOrder, 2)1              -1.534e-01  2.619e-01  1.536e+03  -0.586  0.55826    
#poly(TrialOrder, 2)2               7.275e-01  2.622e-01  1.542e+03   2.775  0.00559 ** 
#nlen.z                             4.193e-02  1.227e-02  6.620e+01   3.417  0.00109 ** 
#fsuf.z                             1.506e-02  1.565e-02  6.530e+01   0.963  0.33934    
#flem.z                            -1.108e-01  1.949e-02  6.810e+01  -5.686 2.97e-07 ***
#SuffixAmbiguityunambiguous         6.646e-03  2.815e-02  6.540e+01   0.236  0.81410    
#flem.z:SuffixAmbiguityunambiguous  1.926e-02  2.804e-02  6.680e+01   0.687  0.49453 

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

#lmer.dat5.a: RT ~ poly(TrialOrder, 2) + nlen.z + fsuf.z + flem.z + SuffixAmbiguity + 
#lmer.dat5.a:     (1 | Subject) + (1 | TrialNumber)
#lmer.dat6.a: RT ~ poly(TrialOrder, 2) + nlen.z + fsuf.z + flem.z * SuffixAmbiguity + 
#lmer.dat6.a:     (1 | Subject) + (1 | TrialNumber)
#            Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#lmer.dat5.a 10 449.88 503.86 -214.94   429.88                         
#lmer.dat6.a 11 451.38 510.76 -214.69   429.38 0.5065      1     0.4766

# Note: the first model is better, again.

# ------
# I will check the interactions between some other predictors, again.
lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + fsuf.z + flem.z + nlen.z*SuffixAmbiguity + (1|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                                    Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                       -1.420e+00  3.326e-02  6.990e+01 -42.685  < 2e-16 ***
#poly(TrialOrder, 2)1              -1.446e-01  2.619e-01  1.536e+03  -0.552  0.58098    
#poly(TrialOrder, 2)2               7.255e-01  2.622e-01  1.542e+03   2.767  0.00573 ** 
#fsuf.z                             1.863e-02  1.436e-02  6.590e+01   1.297  0.19914    
#flem.z                            -1.023e-01  1.352e-02  6.710e+01  -7.565 1.45e-10 ***
#nlen.z                             3.112e-02  1.969e-02  6.500e+01   1.581  0.11878    
#SuffixAmbiguityunambiguous         7.926e-03  2.796e-02  6.520e+01   0.283  0.77773    
#nlen.z:SuffixAmbiguityunambiguous  1.988e-02  2.509e-02  6.600e+01   0.793  0.43091 

#lmer.dat5.a: RT ~ poly(TrialOrder, 2) + nlen.z + fsuf.z + flem.z + SuffixAmbiguity + 
#lmer.dat5.a:     (1 | Subject) + (1 | TrialNumber)
#lmer.dat6.a: RT ~ poly(TrialOrder, 2) + fsuf.z + flem.z + nlen.z * SuffixAmbiguity + 
#lmer.dat6.a:     (1 | Subject) + (1 | TrialNumber)
#            Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#lmer.dat5.a 10 449.88 503.86 -214.94   429.88                         
#lmer.dat6.a 11 451.21 510.59 -214.61   429.21 0.6707      1     0.4128

# Note: the first model is better, again.

# ------
# And again ...
lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*fsuf.z + flem.z + SuffixAmbiguity + (1|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat6)

#Fixed effects:
#                             Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                  -1.43378    0.03365   72.30000 -42.609  < 2e-16 ***
#poly(TrialOrder, 2)1         -0.13777    0.26183 1537.70000  -0.526  0.59883    
#poly(TrialOrder, 2)2          0.72402    0.26211 1543.20000   2.762  0.00581 ** 
#nlen.z                        0.03354    0.01251   65.90000   2.681  0.00928 ** 
#fsuf.z                        0.04310    0.01748   65.60000   2.466  0.01629 *  
#flem.z                       -0.10766    0.01337   67.80000  -8.054 1.78e-11 ***
#SuffixAmbiguityunambiguous    0.03500    0.02947   64.90000   1.187  0.23938    
#nlen.z:fsuf.z                -0.03206    0.01429   66.10000  -2.243  0.02824 *  

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat6.a=update(lmer.dat6, REML=FALSE)

anova(lmer.dat5.a,lmer.dat6.a)

#lmer.dat5.a: RT ~ poly(TrialOrder, 2) + nlen.z + fsuf.z + flem.z + SuffixAmbiguity + 
#lmer.dat5.a:     (1 | Subject) + (1 | TrialNumber)
#lmer.dat6.a: RT ~ poly(TrialOrder, 2) + nlen.z * fsuf.z + flem.z + SuffixAmbiguity + 
#lmer.dat6.a:     (1 | Subject) + (1 | TrialNumber)
#            Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#lmer.dat5.a 10 449.88 503.86 -214.94   429.88                           
#lmer.dat6.a 11 446.65 506.03 -212.33   424.65 5.2287      1    0.02222 *

# Note: the second model is better. 

# ------
# I will check the interactions between some other predictors, the last one.
lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*flem.z + fsuf.z + SuffixAmbiguity + (1|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat7)

#Fixed effects:
#                             Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                -1.419e+00  3.333e-02  7.030e+01 -42.586  < 2e-16 ***
#poly(TrialOrder, 2)1       -1.468e-01  2.619e-01  1.536e+03  -0.561  0.57512    
#poly(TrialOrder, 2)2        7.266e-01  2.622e-01  1.542e+03   2.771  0.00565 ** 
#nlen.z                      4.104e-02  1.289e-02  6.500e+01   3.183  0.00223 ** 
#flem.z                     -1.019e-01  1.354e-02  6.700e+01  -7.523 1.74e-10 ***
#fsuf.z                      2.012e-02  1.443e-02  6.560e+01   1.394  0.16796    
#SuffixAmbiguityunambiguous  1.190e-02  2.853e-02  6.490e+01   0.417  0.67790    
#nlen.z:flem.z              -7.194e-03  1.331e-02  6.750e+01  -0.540  0.59068   

lmer.dat5.a=update(lmer.dat5, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat5.a,lmer.dat7.a)

#lmer.dat5.a: RT ~ poly(TrialOrder, 2) + nlen.z + fsuf.z + flem.z + SuffixAmbiguity + 
#lmer.dat5.a:     (1 | Subject) + (1 | TrialNumber)
#lmer.dat7.a: RT ~ poly(TrialOrder, 2) + nlen.z * flem.z + fsuf.z + SuffixAmbiguity + 
#lmer.dat7.a:     (1 | Subject) + (1 | TrialNumber)
#            Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#lmer.dat5.a 10 449.88 503.86 -214.94   429.88                         
#lmer.dat7.a 11 451.57 510.95 -214.78   429.57 0.3148      1     0.5747

# Note: the second model is better. 

# Final note: lmer6 is the best model so far.

# ----

lmer.dat6 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*fsuf.z + flem.z + SuffixAmbiguity + (1|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat6)

#Random effects:
# Groups      Name        Variance Std.Dev.
# TrialNumber (Intercept) 0.006708 0.0819  
# Subject     (Intercept) 0.037194 0.1929  
# Residual                0.066543 0.2580  
#Number of obs: 1633, groups:  TrialNumber, 74; Subject, 46
#
#Fixed effects:
#                             Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                  -1.43378    0.03365   72.30000 -42.609  < 2e-16 ***
#poly(TrialOrder, 2)1         -0.13777    0.26183 1537.70000  -0.526  0.59883    
#poly(TrialOrder, 2)2          0.72402    0.26211 1543.20000   2.762  0.00581 ** 
#nlen.z                        0.03354    0.01251   65.90000   2.681  0.00928 ** 
#fsuf.z                        0.04310    0.01748   65.60000   2.466  0.01629 *  
#flem.z                       -0.10766    0.01337   67.80000  -8.054 1.78e-11 ***
#SuffixAmbiguityunambiguous    0.03500    0.02947   64.90000   1.187  0.23938    
#nlen.z:fsuf.z                -0.03206    0.01429   66.10000  -2.243  0.02824 * 

# -------------------------------------------------
############################# Is it better with POLY function?

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + poly(nlen, 2)*fsuf.z + flem.z + SuffixAmbiguity + (1|Subject) +(1|TrialNumber), data=dat)
summary (lmer.dat7)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat6.a,lmer.dat7.a)

# Better without poly.

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) +(1|TrialNumber), data=dat)
summary (lmer.dat7)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat6.a,lmer.dat7.a)

# Better with poly.

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*fsuf.z + poly(flem, 2) + SuffixAmbiguity + (1|Subject) +(1|TrialNumber), data=dat)
summary (lmer.dat7)

lmer.dat6.a=update(lmer.dat6, REML=FALSE)
lmer.dat7.a=update(lmer.dat7, REML=FALSE)

anova(lmer.dat6.a,lmer.dat7.a)

# Better without poly.

# -----------------------------------------------
# ---------------------------
################## Best LMER model so far (with -nje suffix)!

lmer.dat7 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) +(1|TrialNumber), data=dat)
summary (lmer.dat7)

#Random effects:
# Groups      Name        Variance Std.Dev.
# TrialNumber (Intercept) 0.005935 0.07704 
# Subject     (Intercept) 0.036985 0.19231 
# Residual                0.066545 0.25796 
#Number of obs: 1633, groups:  TrialNumber, 74; Subject, 46
#
#Fixed effects:
#                             Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                  -1.42747    0.03338   71.10000 -42.761  < 2e-16 ***
#poly(TrialOrder, 2)1         -0.14428    0.26174 1539.30000  -0.551 0.581542    
#poly(TrialOrder, 2)2          0.74782    0.26228 1540.00000   2.851 0.004413 ** 
#nlen.z                        0.05031    0.01386   63.20000   3.629 0.000571 ***
#poly(fsuf, 2)1                1.57338    0.69704   63.80000   2.257 0.027424 *  
#poly(fsuf, 2)2               -0.62566    0.57366   63.40000  -1.091 0.279558    
#flem.z                       -0.11324    0.01322   66.40000  -8.568 2.45e-12 ***
#SuffixAmbiguityunambiguous   -0.00607    0.03201   63.00000  -0.190 0.850220    
#nlen.z:poly(fsuf, 2)1        -0.83987    0.58316   64.10000  -1.440 0.154675    
#nlen.z:poly(fsuf, 2)2         1.72207    0.61701   63.80000   2.791 0.006924 ** 
  
#--------------------------

#################### By-participant random slope adjustments.

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+poly(TrialOrder, 2)|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat7.a=update(lmer.dat7, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat7.a,lmer.dat8.a)

# Better without.

# --------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+nlen.z|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat7.a=update(lmer.dat7, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat7.a,lmer.dat8.a)

# Better with.

# ------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+poly(fsuf, 2)|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat7.a=update(lmer.dat7, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat7.a,lmer.dat8.a)

# Better without.

# ------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+flem.z|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat7.a=update(lmer.dat7, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat7.a,lmer.dat8.a)

# Better without.

# ----------------------------
# ---------------------------------------------
# ---------------------------------------------------------

#################### By-stimulus random slope adjustments.

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+poly(TrialOrder, 2)|TrialNumber) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat7.a=update(lmer.dat7, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat7.a,lmer.dat8.a)

# Better without.

# --------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+nlen.z|TrialNumber) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat7.a=update(lmer.dat7, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat7.a,lmer.dat8.a)

# Better without.

# ------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+poly(fsuf, 2)|TrialNumber) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat7.a=update(lmer.dat7, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat7.a,lmer.dat8.a)

# Better without.

# ------------------------

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+flem.z|TrialNumber) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat7.a=update(lmer.dat7, REML=FALSE)
lmer.dat8.a=update(lmer.dat8, REML=FALSE)

anova(lmer.dat7.a,lmer.dat8.a)

# Better without.

########################## LMER MODEL #######
################### WITH -NJE #################

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+nlen.z|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

# ----------------------------

#Random effects:
# Groups      Name        Variance Std.Dev.
# TrialNumber (Intercept) 0.005929 0.07700 
# Subject     nlen.z      0.001474 0.03839 
# Subject.1   (Intercept) 0.037261 0.19303 
# Residual                0.065028 0.25501 
#Number of obs: 1633, groups:  TrialNumber, 74; Subject, 46
#
#Fixed effects:
#                             Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                -1.428e+00  3.344e-02  7.090e+01 -42.703  < 2e-16 ***
#poly(TrialOrder, 2)1       -1.686e-01  2.601e-01  1.525e+03  -0.648  0.51694    
#poly(TrialOrder, 2)2        6.946e-01  2.608e-01  1.529e+03   2.663  0.00782 ** 
#nlen.z                      5.059e-02  1.492e-02  7.200e+01   3.390  0.00114 ** 
#poly(fsuf, 2)1              1.594e+00  6.948e-01  6.370e+01   2.295  0.02506 *  
#poly(fsuf, 2)2             -6.515e-01  5.724e-01  6.350e+01  -1.138  0.25933    
#flem.z                     -1.133e-01  1.316e-02  6.610e+01  -8.609 2.13e-12 ***
#SuffixAmbiguityunambiguous -6.119e-03  3.188e-02  6.280e+01  -0.192  0.84842    
#nlen.z:poly(fsuf, 2)1      -8.571e-01  5.822e-01  6.440e+01  -1.472  0.14581    
#nlen.z:poly(fsuf, 2)2       1.746e+00  6.162e-01  6.410e+01   2.833  0.00616 **      

# ---------------------
# ---------------------------

# How much does any individual (in this model) differ from the population?
ranef(lmer.dat8)$Subject

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

# -------------
# -----------------------

# I will call this "fake bootstrapping", because I have a small amount of data, and it is not posible to do a real bootstrapping.
confint(lmer.dat8)

#                                 2.5 %      97.5 %
#.sig01                      0.05372813  0.09173528
#.sig02                      0.01954329  0.05712520
#.sig03                      0.15575420  0.24003656
#.sigma                      0.24592758  0.26437098
#(Intercept)                -1.49300718 -1.36295043
#poly(TrialOrder, 2)1       -0.68130718  0.33816352
#poly(TrialOrder, 2)2        0.18588863  1.20953336
#nlen.z                      0.02231979  0.07889158
#poly(fsuf, 2)1              0.28080754  2.90387778
#poly(fsuf, 2)2             -1.73092947  0.43011346
#flem.z                     -0.13814976 -0.08843001
#SuffixAmbiguityunambiguous -0.06629716  0.05403256
#nlen.z:poly(fsuf, 2)1      -1.95500907  0.24295053
#nlen.z:poly(fsuf, 2)2       0.58134293  2.90784395

par(mfrow=c(2,3))
plot(fitted(lmer.dat8),residuals(lmer.dat8))
plot(fitted(lmer.dat8),scale(residuals(lmer.dat8)))
qqnorm(residuals(lmer.dat8), main=" ")
qqline(residuals(lmer.dat8))
par(mfrow=c(1,1))

b1=bootMer(lmer.dat8, FUN = function(x) as.numeric(logLik(x)), nsim = 100)

head(as.data.frame(b1))

#         V1
#1 -182.3588
#2 -201.8891
#3 -217.7920
#4 -214.7473
#5 -189.7041
#6 -222.9504

plot(b1$t)

# -------------
# ------------------------

# Model criticism (Baayen & Milin, 2010; Baayen, 2008).

dat$NV <- relevel(dat$NV, ref = "jedno")

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+nlen.z|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat8.a <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+nlen.z|Subject) + (1|TrialNumber), data=dat, subset=abs(scale(resid(lmer.dat8)))<2.5)
summary (lmer.dat8.a)

#Random effects:
# Groups      Name        Variance Std.Dev.
# TrialNumber (Intercept) 0.006190 0.07868 
# Subject     nlen.z      0.002064 0.04543 
# Subject.1   (Intercept) 0.037196 0.19286 
# Residual                0.057027 0.23880 
#Number of obs: 1609, groups:  TrialNumber, 74; Subject, 46
#
#Fixed effects:
#                             Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                -1.435e+00  3.339e-02  7.140e+01 -42.967  < 2e-16 ***
#poly(TrialOrder, 2)1       -3.854e-01  2.472e-01  1.495e+03  -1.559 0.119285    
#poly(TrialOrder, 2)2        4.571e-01  2.479e-01  1.498e+03   1.844 0.065363 .  
#nlen.z                      5.276e-02  1.531e-02  7.790e+01   3.447 0.000916 ***
#poly(fsuf, 2)1              1.678e+00  6.931e-01  6.380e+01   2.421 0.018322 *  
#poly(fsuf, 2)2             -1.006e+00  5.721e-01  6.410e+01  -1.758 0.083522 .  
#flem.z                     -1.197e-01  1.313e-02  6.620e+01  -9.114 2.66e-13 ***
#SuffixAmbiguityunambiguous -5.134e-03  3.178e-02  6.270e+01  -0.162 0.872181    
#nlen.z:poly(fsuf, 2)1      -1.103e+00  5.807e-01  6.440e+01  -1.899 0.061995 .  
#nlen.z:poly(fsuf, 2)2       1.854e+00  6.155e-01  6.450e+01   3.012 0.003704 **    

# ------------------
# ------------------------------------
################# Same model, but without -nje suffix (outlier) #######################

dat1=dat[dat$SuffixFrequency<10000,]

dat1$SuffixAmbiguity <- relevel(dat1$SuffixAmbiguity, ref = "unambiguous")

lmer.dat8 <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+nlen.z|Subject) + (1|TrialNumber), data=dat)
summary (lmer.dat8)

lmer.dat8.a <- lmer(RT ~ poly(TrialOrder,2) + nlen.z*poly(fsuf, 2) + flem.z + SuffixAmbiguity + (1|Subject) + (0+nlen.z|Subject) + (1|TrialNumber), data=dat, subset=abs(scale(resid(lmer.dat8)))<2.5)
summary (lmer.dat8.a)

#Random effects:
# Groups      Name        Variance Std.Dev.
# TrialNumber (Intercept) 0.006190 0.07868 
# Subject     nlen.z      0.002064 0.04543 
# Subject.1   (Intercept) 0.037196 0.19286 
# Residual                0.057027 0.23880 
#Number of obs: 1609, groups:  TrialNumber, 74; Subject, 46
#
#Fixed effects:
#                             Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                -1.435e+00  3.339e-02  7.140e+01 -42.967  < 2e-16 ***
#poly(TrialOrder, 2)1       -3.854e-01  2.472e-01  1.495e+03  -1.559 0.119285    
#poly(TrialOrder, 2)2        4.571e-01  2.479e-01  1.498e+03   1.844 0.065363 .  
#nlen.z                      5.276e-02  1.531e-02  7.790e+01   3.447 0.000916 ***
#poly(fsuf, 2)1              1.678e+00  6.931e-01  6.380e+01   2.421 0.018322 *  
#poly(fsuf, 2)2             -1.006e+00  5.721e-01  6.410e+01  -1.758 0.083522 .  
#flem.z                     -1.197e-01  1.313e-02  6.620e+01  -9.114 2.66e-13 ***
#SuffixAmbiguityunambiguous -5.134e-03  3.178e-02  6.270e+01  -0.162 0.872181    
#nlen.z:poly(fsuf, 2)1      -1.103e+00  5.807e-01  6.440e+01  -1.899 0.061995 .  
#nlen.z:poly(fsuf, 2)2       1.854e+00  6.155e-01  6.450e+01   3.012 0.003704 ** 

# ------------------
# -------------------------------- VIZUALIZATION OF FINAL RESULTS!
# ------------------

g1<-ggplot(data=dat, aes(x=SuffixAmbiguity, y=RT)) + 
    geom_bar(stat="identity", fill="darkred", colour="darkred")
g1
           
# ------------
           
g2<-ggplot(data=dat, aes(x=nlen.z, y=RT)) + 
  geom_point(shape=18, color="gray")+
  geom_smooth(method=lm,  linetype="solid",
             color="darkred", fill="gray")
g2
           
# ------------
           
g3<-ggplot(data=dat, aes(x=flem.z, y=RT)) + 
  geom_point(shape=18, color="gray")+
  geom_smooth(method=lm,  linetype="solid",
             color="darkred", fill="gray")
g3
           
# ------------
           
g4<-ggplot(data=dat, aes(x=fsuf.z, y=RT)) + 
  geom_point(shape=18, color="gray")+
  geom_smooth(method=lm,  linetype="solid",
             color="darkred", fill="gray")
g4

# ------------

# Interaction of two continuous predictors.
g5<-ggplot(data=dat, aes(x = nlen.z, y = RT, size = fsuf.z))+
	geom_point()+
	geom_smooth(method=lm,  linetype="solid", color="darkred", fill="gray")+
		labs(x = 'len', y = 'RT')+
		scale_size_continuous(guide = FALSE)+
	geom_abline(aes(intercept=33.965, slope=-4.3985, linetype='-1SD FreqSuff'))+
	geom_abline(aes(intercept=38.1208, slope=-5.854, linetype='Mean FreqSuff'))+
	geom_abline(aes(intercept=42.2767, slope=-7.3095, linetype='+1SD FreqSuff'))+
	scale_linetype_manual(values=c('dotted','dashed','solid'),
		breaks=c('-1SD FreqSuff','Mean FreqSuff','+1SD FreqSuff'),name='Simple\nSlope')
g5

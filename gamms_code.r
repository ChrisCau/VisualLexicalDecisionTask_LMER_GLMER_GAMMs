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

# Loading dataset.
dat=read.table("vld_final.txt", header=TRUE, quote="\"")

# Dimensions of dataset (number of rows [data], number of columns [variables]).
dim(dat)
#[1] 2023   16

# Column names.
colnames(dat)

# [1] "Subject"            "TrialOrder"         "Group"             
# [4] "CorrectResponse"    "TrialNumber"        "Suffix"            
# [7] "SuffixLength"       "Response"           "Accuracy"          
#[10] "RT"                 "SuffixAmbiguity"    "Noun"              
#[13] "LemmaFrequency"     "NounLength"         "SuffixFrequency"   
#[16] "SuffixProductivity"

# ------------------
# -------------------------------- PREPARING DATA FOR STATISTICAL ANALYSIS!
# ------------------

# This part of data analysis is almost the same as in previous analysis, LMER & GLMER.

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

# Dimensions of dataset (number of rows [data], number of columns [variables]).
dim(dat)
#[1] 1707   16

# Calculating the percentage of deleted data.
(2023-1707)/2023
#[1] 0.1562037

# -----------------------------------------

# I will skip this step with subset, and the examination of differences between ambiguous and unambiguous suffixes in some of covariates' measures. 
# These steps are identical as in LMER/GLMER analysis.
# ____________
# __________________________________________________________

# I continue with preparation of data for statistical analysis.

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

# Searching for an appropriate transformation for RTs.
powerTransform(dat$RT)
#Estimated transformation parameters 
#   dat$RT 
#-1.314563 

# Note: The results suggest inverse transformation of RTs.

# Following previous studies, I will use log transformation to transform the raw values of LemmaFrequency, SuffixFrequency, SuffixLength, NounLength, and SuffixProductivity.
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

# ------- Shapiro-Wilk & Kolmogorov-Smirnov after transformation of RTs (this step has been skipped earlier, because on the basis of VIP1 graphics I saw that distribution deviates exceptionally from normal [before transformation]).

# I will use K-S test to test normality of transformed RTs distribution.
ks.test(jitter(dat$RT),"pnorm",mean(dat$RT),sd(dat$RT))
#        One-sample Kolmogorov-Smirnov test
#
#data:  jitter(dat$RT)
#D = 0.041731, p-value = 0.006774
#alternative hypothesis: two-sided

# Note: This test is statistically significant, and this suggest that our sample distribution is different than reference probability distribution [distribution is not normal].

# I continue to deal with the distribution, and it's normality. I will use S-W test, and this test is more powerful than K-S test. If S-W test is significant (p-value), than distribution is not normal. 
shapiro.test(dat$RT)
#        Shapiro-Wilk normality test
#
#data:  dat$RT
#W = 0.99274, p-value = 3.377e-07

# Note: It is significant, distribution is not normal, even after the transformation.

# Normalization of continuous precitors, to be comparable on the same scale (by centring to zero and dividing by the standard deviation – z-score).
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
#
# I will skip the visualization of continuous predictors, as well as the random effects, it's the same as in LMER/GLMER analyses.
# _________________________________________________________
# ________________________________________________________________

# Colinearity between continuous predictors.
C=cov(dat[,c("flem.z","nlen.z","fsuf.z","slen.z","sprod.z")], y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
Cor=cov2cor(C)
Cor

#             flem.z      nlen.z     fsuf.z      slen.z     sprod.z
#flem.z   1.00000000 -0.02192262  0.4623827  0.01930949  0.34034109
#nlen.z  -0.02192262  1.00000000 -0.1175926  0.62876318 -0.02878838
#fsuf.z   0.46238265 -0.11759259  1.0000000 -0.27074274  0.91055550
#slen.z   0.01930949  0.62876318 -0.2707427  1.00000000 -0.20600849
#sprod.z  0.34034109 -0.02878838  0.9105555 -0.20600849  1.00000000

collin.fnc(dat[,c("flem.z","nlen.z","fsuf.z","slen.z","sprod.z")])$cnumber
# This value is too big!
# 5.577054

# Reduced, but still too big! This suggest that GAMMs might be more appropriate analysis for this data.
collin.fnc(dat[,c("flem.z","nlen.z","sprod.z")])$cnumber
# 1.427498

# Note: I excluded SuffixFrequency and SuffixLength.

# Visualization of multicolinearity (3 predictors).
postscript("isidora.pairscor1.ps", width=16, height=16,paper="special",horizontal=FALSE,onefile=FALSE)
png("isidora.pairscor1.png", width=800, height=800)
pairscor.fnc(dat[,c("flem.z","nlen.z","sprod.z")], hist=TRUE, smooth=TRUE, cex.point=1, col.points="darkgrey")
dev.off()

# Visualization of multicolinearity (5 predictors).
postscript("isidora2.pairscor1.ps", width=16, height=16,paper="special",horizontal=FALSE,onefile=FALSE)
png("isidora2.pairscor1.png", width=800, height=800)
pairscor.fnc(dat[,c("flem.z","nlen.z","fsuf.z","slen.z","sprod.z")], hist=TRUE, smooth=TRUE, cex.point=1, col.points="darkgrey")
dev.off()

# ______________________________
# _____________________________________________
# ____________________________________________________________

# ------------------
# -------------------------------- GAMMs ANALYSIS!
# ------------------

# ________________________
# _____________________________________________
# ____________________________________________________________

# Quickly visualize the average data of SA factor.
aggregate(RT ~ SuffixAmbiguity, data=dat, mean, na.rm=TRUE)

#  SuffixAmbiguity        RT
#1       ambiguous -1.454187
#2     unambiguous -1.374906

plot(RT ~ SuffixAmbiguity, data = dat)

# ----

# I am starting with the simplest GAMMs model.
gam1 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            nlen.z +
            flem.z +
            sprod.z +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam1)

# Same model as gam1, but I include s(), because I would like to model a potentially nonlinear smooth of my continuous predictors.
gam2 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=6) +
            s(flem.z) +
            s(sprod.z) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam2)

#Parametric coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                -1.391640   0.029515 -47.150   <2e-16 ***
#SuffixAmbiguityunambiguous -0.032369   0.017944  -1.804   0.0714 .  
#trial.z                    -0.006423   0.010838  -0.593   0.5535    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                         edf  Ref.df      F  p-value    
#s(nlen.z)          3.415e+00   4.022  7.177 1.06e-05 ***
#s(flem.z)          4.465e+00   5.359 32.723  < 2e-16 ***
#s(sprod.z)         2.594e+00   3.147  2.867   0.0307 *  
#s(TrialNumber)     2.436e-04   1.000  0.000   0.5276    
#s(trial.z,Subject) 1.073e+02 413.000  2.352  < 2e-16 ***

# Comparasion of two models.
compareML(gam1,gam2)
anova(gam1,gam2,test="Chisq")

#Chi-square test of REML scores
#-----
#  Model    Score Edf Difference    Df   p.value Sig.
#1  gam1 263.4124   9                                
#2  gam2 253.1520  12     10.260 3.000 1.324e-04  ***
#
#AIC difference: 25.58, model gam2 has lower AIC.
#
#
#Model 1: RT ~ SuffixAmbiguity + trial.z + nlen.z + flem.z + sprod.z + 
#    s(TrialNumber, bs = "re") + s(trial.z, Subject, bs = "fs", 
#    m = 1)
#Model 2: RT ~ SuffixAmbiguity + trial.z + s(nlen.z, k = 6) + s(flem.z) + 
#    s(sprod.z) + s(TrialNumber, bs = "re") + s(trial.z, Subject, 
#    bs = "fs", m = 1)
#  Resid. Df Resid. Dev     Df Deviance  Pr(>Chi)    
#1    1479.4     105.07                              
#2    1462.3     101.66 17.104   3.4099 3.468e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Note: gam2 is better model, because AIC is lower.

# -----

# Same model as gam2, but I change k of nlen.z.
gam3 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8) +
            s(flem.z) +
            s(sprod.z) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam3)

#Parametric coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                -1.391562   0.029520 -47.140   <2e-16 ***
#SuffixAmbiguityunambiguous -0.032550   0.017957  -1.813   0.0701 .  
#trial.z                    -0.006403   0.010839  -0.591   0.5548    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                         edf  Ref.df      F  p-value    
#s(nlen.z)          3.652e+00   4.401  6.424 1.78e-05 ***
#s(flem.z)          4.446e+00   5.336 32.782  < 2e-16 ***
#s(sprod.z)         2.580e+00   3.130  2.855   0.0315 *  
#s(TrialNumber)     3.809e-04   1.000  0.000   0.5220    
#s(trial.z,Subject) 1.074e+02 413.000  2.353  < 2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Comparasion of two models.
compareML(gam2,gam3)
anova(gam2,gam3,test="Chisq")

# Note: gam3 is better model, a bit (extremely small difference), but is better (AIC is lower).

# ----

# I still looking for the best model.
gam4 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8) +
            s(flem.z) +
            sprod.z +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam4)

# Comparasion of two models.
compareML(gam3,gam4)
anova(gam3,gam4,test="Chisq")

# Note: gam3 is better model.

# ----

gam5 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8) +
            flem.z +
            s(sprod.z) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam5)

# Comparasion of two models.
compareML(gam3,gam5)
anova(gam3,gam5,test="Chisq")

# Note: gam3 is better again (lower AIC),  definitely I continue on this model.

# -----

############# Checking the existence of potential interactions (I part: interactions with SuffixAmbiguity).

# Build the first model.
gam6 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8) +
            s(flem.z, by=SuffixAmbiguity) +
            s(sprod.z) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam6)

#Parametric coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                -1.38994    0.02976 -46.707   <2e-16 ***
#SuffixAmbiguityunambiguous -0.03459    0.01871  -1.849   0.0647 .  
#trial.z                    -0.00640    0.01087  -0.589   0.5560    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                                           edf  Ref.df      F  p-value    
#s(nlen.z)                            4.101e+00   4.845  5.809 2.43e-05 ***
#s(flem.z):SuffixAmbiguityambiguous   1.000e+00   1.001 98.949  < 2e-16 ***
#s(flem.z):SuffixAmbiguityunambiguous 3.977e+00   4.755 17.617 6.60e-16 ***
#s(sprod.z)                           2.988e+00   3.570  2.980   0.0286 *  
#s(TrialNumber)                       2.246e-05   1.000  0.000   0.9758    
#s(trial.z,Subject)                   1.080e+02 413.000  2.374  < 2e-16 ***

# Comparasion of two models.
compareML(gam3,gam6)
anova(gam3,gam6,test="Chisq")

# Note: The difference is extremely small, but gam6 is better, I will try with other interactions.

# ----

gam7 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8) +
            s(flem.z) +
            s(sprod.z, by=SuffixAmbiguity) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam7)

# Comparasion of two models (gam3 and gam7).
compareML(gam3,gam7)
anova(gam3,gam7,test="Chisq")

# Note: Model gam3 is better.

# Comparasion of two models (gam6 and gam7).
compareML(gam6,gam7)
anova(gam6,gam7,test="Chisq")

# Note: Model gam6 is better.

# ----
gam8 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8, by=SuffixAmbiguity) +
            s(flem.z) +
            s(sprod.z) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam8)

#Parametric coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                -1.40970    0.02995 -47.063   <2e-16 ***
#SuffixAmbiguityunambiguous -0.02101    0.01911  -1.099    0.272    
#trial.z                    -0.00584    0.01091  -0.535    0.593    
# ----
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                                           edf  Ref.df      F  p-value    
#s(nlen.z):SuffixAmbiguityambiguous   3.978e+00   4.517  4.197  0.00488 ** 
#s(nlen.z):SuffixAmbiguityunambiguous 4.037e+00   4.748  8.165 3.26e-07 ***
#s(flem.z)                            2.782e+00   3.414 45.526  < 2e-16 ***
#s(sprod.z)                           3.129e+00   3.725  2.970  0.03955 *  
#s(TrialNumber)                       2.896e-04   1.000  0.000  0.76974    
#s(trial.z,Subject)                   1.089e+02 413.000  2.398  < 2e-16 ***

# Comparasion of two models (gam3 and gam8).
compareML(gam3,gam8)
anova(gam3,gam8,test="Chisq")

# Note: gam8 is better, I will check what will be with gam6 and gam8.

# Comparasion of two models (gam6 and gam8).
compareML(gam6,gam8)
anova(gam6,gam8,test="Chisq")

#  Model    Score Edf Difference    Df
#1  gam6 251.8339  14                 
#2  gam8 249.5802  14     -2.254 0.000
#
#AIC difference: 6.56, model gam8 has lower AIC.
#
#> anova(gam6,gam8,test="Chisq")
#Analysis of Deviance Table
#
#Model 1: RT ~ SuffixAmbiguity + trial.z + s(nlen.z, k = 8) + s(flem.z, 
#    by = SuffixAmbiguity) + s(sprod.z) + s(TrialNumber, bs = "re") + 
#    s(trial.z, Subject, bs = "fs", m = 1)
#Model 2: RT ~ SuffixAmbiguity + trial.z + s(nlen.z, k = 8, by = SuffixAmbiguity) + 
#    s(flem.z) + s(sprod.z) + s(TrialNumber, bs = "re") + s(trial.z, 
#    Subject, bs = "fs", m = 1)
#  Resid. Df Resid. Dev     Df Deviance Pr(>Chi)  
#1    1461.6    100.794                           
#2    1457.1     99.918 4.5474  0.87587  0.01565 *

# Note: gam8 is the best model, so far.
# ----

############# Checking the existence of potential interactions (II part: interactions between continuous predictors).

gam9 <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8, by=SuffixAmbiguity) +
            te(flem.z, sprod.z) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat)
summary(gam9)

#Parametric coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                -1.406148   0.029917 -47.002   <2e-16 ***
#SuffixAmbiguityunambiguous -0.025270   0.018765  -1.347    0.178    
#trial.z                    -0.006108   0.010889  -0.561    0.575    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                                           edf  Ref.df      F  p-value    
#s(nlen.z):SuffixAmbiguityambiguous   3.900e+00   4.447  3.628   0.0116 *  
#s(nlen.z):SuffixAmbiguityunambiguous 3.516e+00   4.158  6.768 1.66e-05 ***
#te(flem.z,sprod.z)                   9.954e+00  12.104 15.590  < 2e-16 ***
#s(TrialNumber)                       1.832e-04   1.000  0.000   0.8480    
#s(trial.z,Subject)                   1.088e+02 413.000  2.398  < 2e-16 ***

# Comparasion of two models (gam8 and gam9).
compareML(gam8,gam9)
anova(gam8,gam9,test="Chisq")

#Chi-square test of REML scores
#-----
#  Model    Score Edf Difference    Df   p.value Sig.
#1  gam8 249.5802  14                                
#2  gam9 244.1120  15      5.468 1.000 9.430e-04  ***
#
#AIC difference: 2.49, model gam9 has lower AIC.
#
#Analysis of Deviance Table
#
#Model 1: RT ~ SuffixAmbiguity + trial.z + s(nlen.z, k = 8, by = SuffixAmbiguity) + 
#    s(flem.z) + s(sprod.z) + s(TrialNumber, bs = "re") + s(trial.z, 
#    Subject, bs = "fs", m = 1)
#Model 2: RT ~ SuffixAmbiguity + trial.z + s(nlen.z, k = 8, by = SuffixAmbiguity) + 
#    te(flem.z, sprod.z) + s(TrialNumber, bs = "re") + s(trial.z, 
#    Subject, bs = "fs", m = 1)
#  Resid. Df Resid. Dev     Df Deviance Pr(>Chi)  
#1    1457.1     99.918                           
#2    1451.7     99.210 5.3512  0.70779  0.06959 .

# ----

# Note: this is final model.

# Final model criticism.
gam9.a <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8, by=SuffixAmbiguity) +
            te(flem.z, sprod.z) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat, subset=abs(scale(resid(gam9)))<2.5)
summary(gam9.a)

#Parametric coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                -1.41595    0.02970 -47.678   <2e-16 ***
#SuffixAmbiguityunambiguous -0.01805    0.01793  -1.006    0.314    
#trial.z                    -0.01028    0.01146  -0.897    0.370    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                                           edf  Ref.df      F  p-value    
#s(nlen.z):SuffixAmbiguityambiguous   3.928e+00   4.464  4.219  0.00331 ** 
#s(nlen.z):SuffixAmbiguityunambiguous 3.988e+00   4.645  7.551 1.41e-06 ***
#te(flem.z,sprod.z)                   9.851e+00  11.975 17.053  < 2e-16 ***
#s(TrialNumber)                       7.821e-05   1.000  0.000  0.86001    
#s(trial.z,Subject)                   1.218e+02 413.000  2.761  < 2e-16 ***

# ---------
# -----------------
# ______________________________
# _____________________________________________

#####################
################################## Autocorrelation of residuals!
#####################

# ---------
# -----------------
# ______________________________
# _____________________________________________

# Model evaluation (and distribution normality).
par(mfrow=c(1,2))
plot(density(scale(resid(gam9.a))))
qqnorm(scale(resid(gam9.a)))
qqline(scale(resid(gam9.a)))

plot(scale(resid(gam9.a)), fitted(gam9.a))

# ----

# I will check autocorrelation structure of the residuals.
acf_resid(gam9.a, split_pred="Subject", main="ACF resid(gam9.a)")
check_resid(gam9.a, split_pred=c("trial.z", "Subject"))

# Note: Having in mind all things considered above, I think this model is not too complex, so it is not necessary to include AR1 in model.
# But, I will try to include this in the best GAMM model -- gam9.a!

# I will specify the data for time series, and I will do that for a few very last models (I need to compare them).
start_event(data=dat, column="RT", event=c("trial.z", "Subject"))
head(dat)

#  Subject TrialOrder Group CorrectResponse TrialNumber Suffix SuffixLength
#1      s1         31     1               m          24     ak            2
#2      s2         27     1               m          24     ak            2
#3      s3         35     1               m          24     ak            2
#4      s4         39     1               m          24     ak            2
#6      s6          6     1               m          24     ak            2
#7      s7          3     1               m          24     ak            2
#  Response Accuracy        RT SuffixAmbiguity
#1        m        1 -1.736111       ambiguous
#2        m        1 -1.557632       ambiguous
#3        m        1 -1.358696       ambiguous
#4        m        1 -1.976285       ambiguous
#6        m        1 -1.785714       ambiguous
#7        m        1 -1.303781       ambiguous

# Calculate RHO coefficient of gam9.a model.
(valRho <- acf(resid(gam9.a), plot=FALSE)$acf[2])

# 0.04770717

gam9.rho <- gam(RT ~ SuffixAmbiguity +
            trial.z +
            s(nlen.z, k=8, by=SuffixAmbiguity) +
            te(flem.z, sprod.z) +
            s(TrialNumber, bs="re") +
            s(trial.z, Subject, bs="fs", m=1),
            method="REML",
            data=dat, subset=abs(scale(resid(gam9)))<2.5,
			AR.start=dat$start_event, rho=valRho)
summary(gam9.rho)

#Parametric coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                -1.41595    0.02970 -47.678   <2e-16 ***
#SuffixAmbiguityunambiguous -0.01805    0.01793  -1.006    0.314    
#trial.z                    -0.01028    0.01146  -0.897    0.370    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Approximate significance of smooth terms:
#                                           edf  Ref.df      F  p-value    
#s(nlen.z):SuffixAmbiguityambiguous   3.928e+00   4.464  4.219  0.00331 ** 
#s(nlen.z):SuffixAmbiguityunambiguous 3.988e+00   4.645  7.551 1.41e-06 ***
#te(flem.z,sprod.z)                   9.851e+00  11.975 17.053  < 2e-16 ***
#s(TrialNumber)                       7.821e-05   1.000  0.000  0.86001    
#s(trial.z,Subject)                   1.218e+02 413.000  2.761  < 2e-16 ***

# Note: There is no any differences in the structure of results.

# ---------
# -----------------
# ______________________________
# _____________________________________________

# The end, only visualization is left.


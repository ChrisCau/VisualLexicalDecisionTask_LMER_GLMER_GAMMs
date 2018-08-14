# Loading packages.
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

# Loading dataset.
dat=read.table("vld_final.txt", sep="\t",header=TRUE)

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

# This part is almost the same as in the process of conducting LMER analysis, I will skip some parts.

# I will skip sorting part, because I work now with different measure (Accuracy), so I need all answers.

# -----------------------------------------

# Furtermore, I will not create subset, this parts of analysis are the same as in previous LMER analysis.

# ______
# _______________________________________
# __________________________________________________________

# I will not delete errors, I am working with them.

# Visual inspection of data (RTs).
par(mfrow=c(2,2))
plot(sort(dat$Accuracy))
plot(density(dat$Accuracy))
qqnorm(dat$Accuracy)
par(mfrow=c(1,1))

# I will not transform "Accuracy", because of binomial distribution.

# Following previous studies, I will use log transformation to transform the raw values of LemmaFrequency, SuffixFrequency, SuffixLength, NounLength and SuffixProductivity.
dat$flem=log(dat$LemmaFrequency)
dat$nlen=log(dat$NounLength)
dat$fsuf=log(dat$SuffixFrequency)
dat$slen=log(dat$SuffixLength)
dat$sprod=log(dat$SuffixProductivity)

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
#       1012        1011 

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

# I will skip this step (Visualization of continuous predictors), because it is the same as in LMER analysis.

# __________________________________________________________
# ____________________________________________________________

# Visual inspection of random effects.

qqmath(~Accuracy|Subject,data=dat)
qqmath(~Accuracy|TrialNumber,data=dat)
xylowess.fnc (Accuracy~TrialOrder | Subject, data=dat, ylab= "Accuracy")

# _________________________________________________________
# ________________________________________________________________

# Colinearity between predictors.
C=cov(dat[,c("flem","nlen","fsuf","slen","sprod")], y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
Cor=cov2cor(C)
Cor

#             flem        nlen       fsuf         slen      sprod
#flem  1.000000000  0.02816801  0.5037532  0.008320418  0.3947721
#nlen  0.028168006  1.00000000 -0.1695659  0.645946526 -0.1183992
#fsuf  0.503753210 -0.16956594  1.0000000 -0.325074463  0.9190694
#slen  0.008320418  0.64594653 -0.3250745  1.000000000 -0.3125367
#sprod 0.394772138 -0.11839922  0.9190694 -0.312536700  1.0000000

collin.fnc(dat[,c("flem","nlen","fsuf","slen","sprod")])$cnumber

# This value is too big!
#45.08018

# Reduced, but still too big! This suggest that GAMMs might be more appropriate analysis for this data (now, I am sure, it's even worse when looking at the situation with Accuracy measure).
collin.fnc(dat[,c("flem","nlen","sprod")])$cnumber
#30.29088

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

################################################################# GLMER: Generalized Linear Mixed-Effects Regression!

# Note: I will take the final LMER model, and I will change all parameters that need to be changed to become GLMER model.
# Note2: Without -nje, and without Model Criticism.

dat1=dat[dat$SuffixFrequency<10000,]

dat1$SuffixAmbiguity <- relevel(dat1$SuffixAmbiguity, ref = "unambiguous")

glmer.dat1 <- glmer(Accuracy ~ poly(TrialOrder,2) + nlen.z + sprod.z + flem.z + SuffixAmbiguity + (1|Subject) + (0+nlen.z+poly(TrialOrder, 2)|Subject) + (1|TrialNumber), data=dat, family="binomial")
summary (glmer.dat1)

#Random effects:
# Groups      Name                 Variance  Std.Dev. Corr       
# TrialNumber (Intercept)            1.68124  1.2966             
# Subject     nlen.z                 0.02778  0.1667             
#             poly(TrialOrder, 2)1 453.17771 21.2880  -0.30      
#             poly(TrialOrder, 2)2  83.75679  9.1519  -0.94  0.62
# Subject.1   (Intercept)            0.21836  0.4673             
#Number of obs: 2023, groups:  TrialNumber, 88; Subject, 46
#
#Fixed effects:
#                           Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                 3.50785    0.35026  10.015  < 2e-16 ***
#poly(TrialOrder, 2)1       11.98442    5.53443   2.165   0.0304 *  
#poly(TrialOrder, 2)2        3.79703    4.71865   0.805   0.4210    
#nlen.z                      0.21512    0.19552   1.100   0.2712    
#sprod.z                     0.31238    0.22679   1.377   0.1684    
#flem.z                      1.36199    0.21073   6.463 1.03e-10 ***
#SuffixAmbiguityunambiguous  0.08815    0.46009   0.192   0.8481

################## Results fit with those from LMER analysis, except the LemmaFrequency effect, which is inhibitory in this analysis.
# This analysis definitely is not sufficiently strong for our study, so we will use another (much powerfull analysis in other repository) -- GAMMs.

---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Here we are setting the WD to google drive.  See earlier versions for getting the original data source.
```{r}
#setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
#data = read.csv("ELCS-K-2011.csv", header = TRUE)

# X12CHGSCH only have one for this variable so put in zeros for the rest rep(length(X12CHGSCH), 0), X12CHGTCH, X34CHGTCH
# Need to grab the second year versions of these.  If no second version, create a second version with the "2 and 4" title so we can aggregate the data later.

# Ok just need first one and then take the difference between the dependent variables to create 

data1 = cbind(X1PRNCON = data$X4PRNCON-data$X1PRNCON, X1PRNSOC = data$X4PRNSOC-data$X1PRNSOC, X1BMI = data$X1BMI, X1PAR1AGE = data$X1PAR1AGE, X1PAR1EMP = data$X1PAR1EMP, X1HTOTAL = data$X1HTOTAL, X1POVTY = data$X2POVTY, X1PAR1ED_I = data$X12PAR1ED_I, X1LANGST = data$X12LANGST,  X1_CHSEX_R = data$X_CHSEX_R, X1HPARNT = data$X1HPARNT, X1KAGE_R = data$X1KAGE_R, X1CHGSCH = data$X12CHGSCH, S1REGSKL  = data$S2REGSKL , X1PAR1RAC = data$X1PAR1RAC, X1_RACETHP_R = data$X_RACETHP_R)
head(data1)

# Change the -9 to NAs
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
data1 = as.data.frame(data1)
head(data1)
dim(data1)
```
First get them in long form before we make transformation.  Don't need this now, because mathit cannot do longitudinal analysis.
```{r}

#data1 = as.data.frame(data1)
#dim(data1)
#library(reshape)
#data1 = reshape(data1, varying = list(c("X1PRNCON", "X2PRNCON", "X4PRNCON"), c("X1PRNSOC","X2PRNSOC","X4PRNSOC"), c("X1BMI", "X2BMI", "X4BMI"), c("X1PAR1AGE" , "X2PAR1AGE" , "X4PAR1AGE"), c("X1PAR1EMP" , "X2PAR1EMP" , "X4PAR1EMP"), c("X1HTOTAL" , "X2HTOTAL" , "X4HTOTAL"), c("X1POVTY", "X2POVTY", "X4POVTY_I" ), c("X1PAR1ED_I", "X2PAR1ED_I", "X4PAR1ED_I"), c("X1LANGST", "X2LANGST", "X4LANGST"), c("X1_CHSEX_R", "X2_CHSEX_R", "X4_CHSEX_R"), c("X1HPARNT", "X2HPARNT",  "X4HPARNT"), c("X1KAGE_R", "X2KAGE_R", "X4AGE"), c("X1CHGSCH", "X2CHGSCH", "X4CHGTCH"), c("S1REGSKL", "S2REGSKL", "S4REGSKL"), c("X1PAR1RAC", "X2PAR1RAC", "X4PAR1RAC"), c("X1_RACETHP_R", "X2_RACETHP_R", "X4_RACETHP_R")), times = c(1,2,4), direction = "long")
#data1 = as.data.frame(data1)

```



Now we need to alter the variables to be binary in necessary.  First we create get all the variables where 1 is the interest and get those as 1 and rest as zero.  Then for parent ethnicty we change the ones to zero and everything else to one to have a non-white be one.  Then we need to grab a seperate subset of the all the remaining variables, so we don't double up on those variables when we cbind them togehter at the end.  I then needed to get the variables in the same order and rename with meaningful names.  Need to grab the correct data from data sets two and three, because those have the binary transformations.
```{r}
# These all need to be transformed into 1 as one 1 and everything else zero.  They need to be from data1, because that is the data set where all of the missing values are recoded as NA's.
data2 = cbind(X1LANGST = data1$X1LANGST,  X1_CHSEX_R = data1$X1_CHSEX_R, X1HPARNT = data1$X1HPARNT, X1CHGSCH = data1$X1CHGSCH)

data2 = ifelse(is.na(data2), NA, ifelse(data2 == 1, 1,0))
data2 = as.data.frame(data2)
head(data2)

# Here is the ethnicity variable that needs to be transformed into the original variables that you used.  Remember that original variable were incorrect and just keeping the names the same here for consistency.
data3 = data1$X1_RACETHP_R
data3 = as.data.frame(data3)
X_HISP_R = ifelse(is.na(data3), NA, ifelse(data3 == 3 | data3 == 4, 1, 0))
X_HISP_R = as.data.frame(X_HISP_R)
names(X_HISP_R) = c("X_HISP_R")
#Instead of changing all of the names, just replaced the other ethnicity with white
X_WHITE_R = ifelse(is.na(data3), NA, ifelse(data3 == 1, 1, 0))
X_WHITE_R = as.data.frame(X_WHITE_R)
names(X_WHITE_R) = c("X_WHITE_R")
sum(X_WHITE_R, na.rm =TRUE)

X_BLACK_R = ifelse(is.na(data3), NA, ifelse(data3 == 2, 1, 0))
X_BLACK_R = as.data.frame(X_BLACK_R)
names(X_BLACK_R) = c("X_BLACK_R")

X_ASIAN_R = ifelse(is.na(data3), NA, ifelse(data3 == 5, 1, 0))
X_ASIAN_R = as.data.frame(X_ASIAN_R)
names(X_ASIAN_R) = c("X_ASIAN_R")

X_AMINAN_R = ifelse(is.na(data3), NA, ifelse(data3 == 7, 1, 0))
X_AMINAN_R = as.data.frame(X_AMINAN_R)
names(X_AMINAN_R) = c("X_AMINAN_R")

X_HAWPI_R = ifelse(is.na(data3), NA, ifelse(data3 == 6, 1, 0))
X_HAWPI_R = as.data.frame(X_HAWPI_R)
names(X_HAWPI_R) = c("X_HAWPI_R")

X_MULTR_R = ifelse(is.na(data3), NA, ifelse(data3 == 8, 1, 0))
X_MULTR_R = as.data.frame(X_MULTR_R)
names(X_MULTR_R) = c("X_MULTR_R")


data4 = cbind(X_HISP_R, X_WHITE_R, X_BLACK_R, X_ASIAN_R, X_AMINAN_R, X_HAWPI_R)
data4 = as.data.frame(data4)

# Need to grab the public (only regular public classrooms) versus private and magnet and charter: S2REGSKL.  Need to reverse code, because it has 1 as public and one 1 as public.
data5 = data1$S1REGSKL
S1REGSKL = ifelse(is.na(data5), NA, ifelse(data5 == 1, 0, 1))

# Need to change 2 through 8 to be 1 and 1 to be zero and therefore 1 is all non-white parents
data6 = cbind(X1PAR1RAC = data1$X1PAR1RAC)
data6 = ifelse(is.na(data6), NA, ifelse(data6 == 1, 0,1))
data6 = as.data.frame(data6)
```
Reording the variables to be in the correct order.  Grab each variable from the correct data set from above.
```{r}
# Rearrange and then rename variables to get them in the correct order.  This includes getting data2 and data3 into the correct order as well, because you need to rename all of these variables. 

data7 = cbind(X1PRNCON = data1$X1PRNCON, X1PRNSOC = data1$X1PRNSOC, X1BMI = data1$X1BMI, X1PAR1AGE = data1$X1PAR1AGE, X1PAR1EMP = data$X1PAR1EMP, X1HTOTAL = data$X1HTOTAL,  X1POVTY = data1$X1POVTY, X1PAR1ED_I = data1$X1PAR1ED_I, X1KAGE_R = data1$X1KAGE_R)
data7 = as.data.frame(data7)
head(data7)
data1 = cbind(data2, data4, S1REGSKL, data6,data7)
data1 = as.data.frame(data1)
head(data1)
dim(data1)

```

Here we will use Amelia.  Need to set m as five for five imputed data sets.  Then we place each of the variables into their appropriate categories.

```{r}
library(Amelia)
library(mitools)
library(survey)
m = 5
a.out = amelia(x = data1, m=m, ords = c("X1PAR1EMP", "X1POVTY", "X1PAR1ED_I"), logs = c("X1HTOTAL"), noms = c("X_HISP_R", "X_BLACK_R", "X_WHITE_R", "X_ASIAN_R","X_AMINAN_R", "X_HAWPI_R", "X1_CHSEX_R", "X1LANGST", "X1HPARNT", "X1CHGSCH", "X1PAR1RAC", "S1REGSKL"))
# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
write.amelia(obj = a.out, file.stem = "ECLSK")
head(a.out.imp[c(1:6)])
test = a.out.imp$imputations$imp1
```
Now we are analyzing one data set, using matchIT and seeing if we can get a regular regression and then a multilevel model with time
```{r}
library(MatchIt)
setwd("~/Desktop")
ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
ECLSK1 = ECLSK1[c(-1)]
ECLSK1 = na.omit(ECLSK1)
ECLSK1 = as.data.frame(ECLSK1)
sum(is.na(ECLSK1))

m.out = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, data = ECLSK1, method = "nearest", ratio = 1)
summary(m.out)
plot(m.out)
m.data <- match.data(m.out)
dim(m.data)
library(Zelig)
z.outSC = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = m.data)
summary(z.outSC)

z.outPS = zelig( X1PRNSOC  ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = m.data)
summary(z.outPS)

```
Next step do it for all five data sets and then combine them


Here we are analyzing data from the first imputted data set ECLSK1 
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data/ImputedFirst")
ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
ECLSK1 = ECLSK1[c(-1)]
ECLSK1 = as.data.frame(ECLSK1)


# Grab descriptives
svyMean1 = svymean(ECLSK1, scdrep1)

modelSI1 = svyglm(log(X1PRNSOC) ~X_HISP_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep1)


modelSI1Coef= summary(modelSI1)$coefficients[,1:2]
modelSI1Coef = as.data.frame(t(modelSI1Coef))

```




Here we are analyzing data from the second imputted data set ECLSK2
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data/ImputedFirst")
ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
ECLSK2 = ECLSK2[c(-1)]
ECLSK2 = as.data.frame(ECLSK2)

scdrep2 = svrepdesign(data = ECLSK2, type="JKn", repweights = ECLSK2[,26:106], weights = ECLSK2[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean2= svymean(ECLSK2, scdrep2)


modelSI2 = svyglm(log(X1PRNSOC) ~X_HISP_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI  + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep2)


modelSI2Coef= summary(modelSI2)$coefficients[,1:2]
modelSI2Coef = as.data.frame(t(modelSI2Coef))
```
Now with imputed data set three
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data/ImputedFirst")

ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
ECLSK3 = ECLSK3[c(-1)]
ECLSK3 = as.data.frame(ECLSK3)

scdrep3 = svrepdesign(data = ECLSK3, type="JKn", repweights = ECLSK3[,26:106], weights = ECLSK3[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean3= svymean(ECLSK3, scdrep3)


modelSI3 = svyglm(log(X1PRNSOC) ~X_HISP_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI  + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep3)


modelSI3Coef= summary(modelSI3)$coefficients[,1:2]
modelSI3Coef = as.data.frame(t(modelSI3Coef))
```
How with imputed data four
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data/ImputedFirst")

ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
ECLSK4 = ECLSK4[c(-1)]
ECLSK4 = as.data.frame(ECLSK4)

scdrep4 = svrepdesign(data = ECLSK4, type="JKn", repweights = ECLSK4[,26:106], weights = ECLSK4[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean4= svymean(ECLSK4, scdrep4)

modelSI4 = svyglm(log(X1PRNSOC) ~X_HISP_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI  + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep4)


modelSI4Coef= summary(modelSI4)$coefficients[,1:2]
modelSI4Coef = as.data.frame(t(modelSI4Coef))

```
Now with the fifth imputed data set
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data/ImputedFirst")
 
ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
ECLSK5 = ECLSK5[c(-1)]
ECLSK5 = as.data.frame(ECLSK5)

scdrep5 = svrepdesign(data = ECLSK5, type="JKn", repweights = ECLSK5[,26:106], weights = ECLSK5[,26], combined.weights = TRUE, rscales = 1, scale = 1, nest = TRUE)

# Grab descriptives
svyMean5= svymean(ECLSK5, scdrep5)


modelSI5 = svyglm(log(X1PRNSOC) ~X_HISP_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI  + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP  + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep5)


modelSI5Coef= summary(modelSI5)$coefficients[,1:2]
modelSI5Coef = as.data.frame(t(modelSI5Coef))

# SEtting the degrees of freedom here.
df =  modelSI5$df.null 
```
Now we need to rbind all of the parameter and sd estimates into two columns.  However, we need to grab the first row of these data sets, because the second row is the standard error.  We need seperate data set for the se's, because we need to stack the different sets on top of each other.


Here we have the SI model
```{r}
ParsSI = rbind(modelSI1Coef[1,], modelSI2Coef[1,], modelSI3Coef[1,], modelSI4Coef[1,], modelSI5Coef[1,])
ParsSI = as.matrix(ParsSI)
ParsSI

SeSI = rbind(modelSI1Coef[2,], modelSI2Coef[2,], modelSI3Coef[2,], modelSI4Coef[2,], modelSI5Coef[2,])
SeSI = as.matrix(SeSI)
SeSI

SIModelSIombine = mi.meld(q = ParsSI, se = SeSI)

SIModelSIombine = as.data.frame(cbind(Estimate = t(SIModelSIombine$q.mi),SE =  t(SIModelSIombine$se.mi)))

names(SIModelSIombine) = c("Estimate", "SE")
SIModelSIombine
SIModelSIombine$T_Stat = SIModelSIombine$Estimate / SIModelSIombine$SE
SIModelSIombine$P_Value = 2*pt(-abs(SIModelSIombine$T_Stat), df = df)
SIModelSIombine = round(SIModelSIombine,3)

SIModelSIombine$Sig = ifelse(SIModelSIombine$P_Value <= .000, "***", ifelse( SIModelSIombine$P_Value <= .01, "**", ifelse(SIModelSIombine$P_Value <= .05, "*","NS")))

SIModelSIombine

```
Now we need to grab the means and standard deviations and combine them.  Need to transpose them, because Amelia takes the five different version and combines them.
```{r}
allSvyMeans = t(as.matrix(cbind(svyMean1, svyMean2, svyMean3, svyMean4, svyMean5)))
# For Se's we need to create dataframes so that we can grab the se's them combine them
SvySes1 = as.data.frame(svyMean1)
SvySes2 = as.data.frame(svyMean2)
SvySes3 = as.data.frame(svyMean3)
SvySes4 = as.data.frame(svyMean4)
SvySes5 = as.data.frame(svyMean5)

SvySes1 = SvySes1$SE
SvySes2 = SvySes2$SE
SvySes3 = SvySes3$SE
SvySes4 = SvySes4$SE
SvySes5 = SvySes5$SE

allSvySes = t(as.matrix(cbind(SvySes1, SvySes2, SvySes3, SvySes4, SvySes5)))

allSvyMeansSes = mi.meld(q = allSvyMeans, se = allSvySes)

allSvyMeansComb = t(as.data.frame(allSvyMeansSes$q.mi))
allSvySesComb = t(as.data.frame(allSvyMeansSes$se.mi))

allSvyMeansSesComb = data.frame(Mean = allSvyMeansComb, SE = allSvySesComb)
```



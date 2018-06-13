---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Need to library packages
```{r}
library(psych)
library(prettyR)
library(Amelia)
library(mitools)
library(MatchIt)
```



Here we are setting the WD to google drive.  See earlier versions for getting the original data source.

The focus is on self-control, so just grab that.  Need teacher self-report self control  
Get rid of school change, just looking ITT effect so what is the effect of starting in a private school
We need all four self control, child demographics (all binary), parental demographics (all binary).
You have time points 1,2,4 it is ok that there are different intervals between them, because multilevel modeling time points can be spaced out differently.  

Poverty on page 7-49, poverty
Coding parent one employment 35 hours or more or less than 35 hours
Can create SES look on page 7-50 if you need to create it later.
Poverty is 200 percent below
Employment 35 hours or more is one and else two

Education?  
PAR race is white and non-white

Only one research question whether public school (as you define it) versus private school as you define it.  Future researchers will need to gain access to whether a student was in a charter school and start to analyze differences between those options and public and versuss private.
S2REGSKL ECLS1998-1999

If there are only two measurements, just including the baseline, because when we trasform to long version you need to transform all time points the same otherwise it repeats, which is fine, we are just treating those as time invariant variables.
```{r}
#setwd("~/Box Sync/PropScore")
#data = read.csv("ELCS-K-2011.csv", header = TRUE)

attach(data)
data1 = cbind(X1PRNCON, X2PRNCON, X4PRNCON,X1RTHET= data$X1RTHET, X2RTHET = data$X2RTHET, X4RTHET = data$X4RTHET, X1MTHET = data$X1MTHET, X2MTHET = data$X2MTHET, X4MTHET = data$X4MTHET, X1BMI, X2BMI, X4BMI,X1HTOTAL, X2HTOTAL, X4HTOTAL, X1PAR1AGE, X2PAR1AGE, X4PAR1AGE = data$X4PAR1AGE, X1PAR1EMP, X2POVTY, X12PAR1ED_I, X12LANGST,X_CHSEX_R, S2REGSKL, X1PAR1RAC, X1_RACETHP_R = data$X_RACETHP_R)

summary(data1)
# Change the -9 to NAs
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
#summary(data1)
data1 = as.data.frame(data1)
head(data1)
dim(data1)
```
Make the demographics binary first then replace them.  Easier for imputation and not interested in the effects of demographics, just using them as controls.  

Put the demographics back into the data1 data set with them transformed.

Remember that R does not transform NA into zeros, so the code below works.
```{r}
datCat = data1[c(19:26)]
head(datCat)
data1[c(19:26)] = NULL
datCat
apply(datCat, 2, function(x){describe.factor(x)})
datCat = data.frame(apply(datCat, 2, function(x){(ifelse(x > 1, 0, 1))}))
head(datCat)
data1 = data.frame(data1, datCat)
head(data1)
```
Descriptives:
Descirptives with no missing values.
Getting descriptives here.  Not a great solution, but we get with and without missing data.  Then change to long later and then impute again and use that data set to do the data analysis. Maybe run more imputations to make sure you get similar results.    
```{r}
dataDesc = data1
dim(dataDesc)
dataDesc = na.omit(dataDesc)
dim(dataDesc)
datCon = dataDesc[c(1:18)]
head(datCon)
mean_sd_fun = function(x){
  mean_mean = mean(x)
  sd_sd = sd(x)
  mean_sd = cbind(mean_mean, sd_sd)
}
apply(datCon, 2, mean_sd_fun)
datCat = dataDesc[c(19:26)]
head(datCat)
apply(datCat, 2, function(x){describe.factor(x)})
```
Now getting descriptives using data imputation.  

```{r}
head(data1)
m = 5
a.out = amelia(x = data1, m=m, logs = c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), noms = c("X1PAR1EMP", "X2POVTY", "X12PAR1ED_I", "X12LANGST","X_CHSEX_R", "S2REGSKL", "X1PAR1RAC", "X1_RACETHP_R"))
# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
summary(a.out)
compare.density(a.out, var = "X1PRNCON", main = "Observed and Imputed values of Self Control")
compare.density(a.out, var = "X2PRNCON", main = "Observed and Imputed values of Self Control")
compare.density(a.out, var = "X4PRNCON", main = "Observed and Imputed values of Self Control")
disperse(a.out)
```
Descriptives:
Descirptives when missing values are imputed for the tables
So need to write a loop for the functions that produce the means for each.  Because everything is dichomoized can just get counts from means.
Then you need to combine the correctly 
```{r}
descFun = function(x){
  x = data.frame(t(x))
}

mean.out = NULL
for(i in 1:m){
  mean.out[[i]] = apply(a.out$imputations[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}

mean.out = descFun(mean.out)
mean.out

sd.out = NULL
for(i in 1:m){
  sd.out[[i]] = apply(a.out$imputations[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out

mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out
```
Now we are analyzing one data set, using matchIT and seeing if we can get a regular regression and then a multilevel model with time.  We matched everyone.

Here are the estimates for the first model.  Need to grab the parameter estimates and sd's 
```{r}

m.out = NULL

# Need to change all of these to include the new covariates
m.out1 = matchit(S2REGSKL ~ X1PRNCON + X2PRNCON + X4PRNCON +X1RTHET +X2RTHET + X4RTHET + X1MTHET + X2MTHET + X4MTHET + X1BMI +X2BMI + X4BMI + X1HTOTAL + X2HTOTAL + X4HTOTAL + X1PAR1AGE + X2PAR1AGE + X4PAR1AGE + X1PAR1EMP + X2POVTY + X12PAR1ED_I + X12LANGST + X_CHSEX_R + S2REGSKL + X1PAR1RAC + X1_RACETHP_R, data = a.out$imputations$imp1, method = "nearest", ratio = 1)

summary(m.out1)
plot(m.out1, type = "jitter")
plot(m.out1, type = "hist")

```
Now we are getting residual analyses for first the data set
```{r}
resZ.outSC1 = as.data.frame(z.outSC1$get_residuals())
resZ.outSC1 = resZ.outSC1[,1]
resZ.outSC1 = as.data.frame(resZ.outSC1)
resZ.outSC1 = as.data.frame(resZ.outSC1$resZ.outSC1)


fitZ.outSC1 = as.data.frame(z.outSC1$get_predict())
fitZ.outSC1 = fitZ.outSC1[,1]
fitZ.outSC1 = as.data.frame(fitZ.outSC1)
fitZ.outSC1 = as.data.frame(fitZ.outSC1$fitZ.outSC1)
fitResidSC1 = cbind(fitZ.outSC1, resZ.outSC1)
plot(fitResidSC1)
plot(resZ.outSC1)

resZ.outSC1 = scale(resZ.outSC1, center = TRUE, scale = TRUE)
hist(resZ.outSC1, xlim = c(-5,5))
above = sum(ifelse(resZ.outSC1 > 3, 1, 0))
below = sum(ifelse(resZ.outSC1 < -3, 1, 0))
totalS = sum(above, below)
totalS / (7694 + 19)*100
```
Now we get the estimates for the second data set
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
ECLSK2 = ECLSK2[c(-1)]
ECLSK2 = na.omit(ECLSK2)
ECLSK2 = as.data.frame(ECLSK2)

m.out2 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD + X1PRNIMP + X1RTHET + X1MTHET, data = ECLSK1, method = "nearest", ratio = 1)

plot(m.out2, type = "jitter")
plot(m.out2, type = "hist")

# Now getting descriptives
m.data2 <- match.data(m.out2)
m.dataMeans2 = apply(m.data2, 2, mean)
m.dataSD2 = apply(m.data2,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC2 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out2))
SCCof2 =  z.outSC2$get_coef()
SCSes2 = z.outSC2$get_se()


# Here is for the social interaction variable.
z.outSI2 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out2))

SICof2 =  z.outSI2$get_coef()
SISes2 = z.outSI2$get_se()
```
Now get the estimates for the third data set
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
ECLSK3 = ECLSK3[c(-1)]
ECLSK3 = na.omit(ECLSK3)
ECLSK3 = as.data.frame(ECLSK3)

m.out3 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD + X1PRNIMP + X1RTHET + X1MTHET, data = ECLSK1, method = "nearest", ratio = 1)

plot(m.out3, type = "jitter")
plot(m.out3, type = "hist")

# Now getting descriptives
m.data3 <- match.data(m.out3)
m.dataMeans3 = apply(m.data3, 2, mean)
m.dataSD3 = apply(m.data3,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC3 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out3))
SCCof3 =  z.outSC3$get_coef()
SCSes3 = z.outSC3$get_se()


# Here is for the social interaction variable.
z.outSI3 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out3))

SICof3 =  z.outSI3$get_coef()
SISes3 = z.outSI3$get_se()
```
Now get the estimates for the fourth data set
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
ECLSK4 = ECLSK4[c(-1)]
ECLSK4 = na.omit(ECLSK4)
ECLSK4 = as.data.frame(ECLSK4)

m.out4 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD + X1PRNIMP + X1RTHET + X1MTHET, data = ECLSK1, method = "nearest", ratio = 1)
summary(m.out4)
plot(m.out4, type = "jitter")
plot(m.out4, type = "hist")

# Now getting descriptives
m.data4 <- match.data(m.out4)
m.dataMeans4 = apply(m.data4, 2, mean)
m.dataSD4 = apply(m.data4,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC4 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out4))
SCCof4 =  z.outSC4$get_coef()
SCSes4 = z.outSC4$get_se()


# Here is for the social interaction variable.
z.outSI4 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out4))

SICof4 =  z.outSI4$get_coef()
SISes4 = z.outSI4$get_se()
```
Now we get the fifth data set estimates
```{r}
setwd("~/Google Drive/PARCS/Projects/PropScore/Data")
ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
ECLSK5 = ECLSK5[c(-1)]
ECLSK5 = na.omit(ECLSK5)
ECLSK5 = as.data.frame(ECLSK5)

m.out5 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD + X1PRNIMP + X1RTHET + X1MTHET, data = ECLSK1, method = "nearest", ratio = 1)
summary(m.out5)
plot(m.out5, type = "jitter")
plot(m.out5, type = "hist")

# Now getting descriptives
m.data5 <- match.data(m.out5)
m.dataMeans5 = apply(m.data5, 2, mean)
m.dataSD5 = apply(m.data5,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC5 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out5))
SCCof5 =  z.outSC5$get_coef()
SCSes5 = z.outSC5$get_se()


# Here is for the social interaction variable.
z.outSI5 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT  + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R + X1PRNSAD +X1PRNIMP + X1RTHET + X1MTHET, model = "ls", data = match.data(m.out5))

SICof5 =  z.outSI5$get_coef()
SISes5 = z.outSI5$get_se()
```
Now we need to combine the results for the descriptive statistics
```{r}
library(Amelia)
allMeans = t(as.matrix(cbind(m.dataMeans1, m.dataMeans2, m.dataMeans3, m.dataMeans4, m.dataMeans5)))

allSDs = t(as.matrix(cbind(m.dataSD1, m.dataSD2, m.dataSD3, m.dataSD4, m.dataSD5)))


allMeansSDsCom = mi.meld(q = allMeans, se = allSDs)

```
Now we are getting the combined estimates for the SI variable
```{r}
# Here we need to rerrange the variables, because it wasn't working.  Here we are getting the

SICof1 = as.data.frame(SICof1)
names(SICof1) = c("ParEst")

SICof2 = as.data.frame(SICof2)
names(SICof2) = c("ParEst")

SICof3 = as.data.frame(SICof3)
names(SICof3) = c("ParEst")

SICof3 = as.data.frame(SICof3)
names(SICof3) = c("ParEst")

SICof4 = as.data.frame(SICof4)
names(SICof4) = c("ParEst")

SICof5 = as.data.frame(SICof5)
names(SICof5) = c("ParEst")

allParsSI = t(as.matrix(cbind(SICof1, SICof2, SICof3, SICof4, SICof5)))

SISes1 = as.data.frame(SISes1)
names(SISes1) = c("SE")

SISes2 = as.data.frame(SISes2)
names(SISes2) = c("SE")

SISes2 = as.data.frame(SISes2)
names(SISes2) = c("SE")

SISes3 = as.data.frame(SISes3)
names(SISes3) = c("SE")

SISes4 = as.data.frame(SISes4)
names(SISes4) = c("SE")

SISes5 = as.data.frame(SISes5)
names(SISes5) = c("SE")

allSEsSI = t(as.matrix(cbind(SISes1, SISes2, SISes3, SISes4, SISes5)))

allParsSesSICom = mi.meld(q = allParsSI, se = allSEsSI)
allParsPaperSI = t(as.data.frame(allParsSesSICom$q.mi))

allSesPaperSI = t(as.data.frame(allParsSesSICom$se.mi))

allParSesPaperSI = cbind(allParsPaperSI, allSesPaperSI)
write.csv(allParSesPaperSI, "allParSesPaperSI.csv")
```
Now we are getting the combined estimates for the SC variable
```{r}
# Here we need to rerrange the variables, because it wasn't working.  Here we are getting the

SCCof1 = as.data.frame(SCCof1)
names(SCCof1) = c("ParEst")

SCCof2 = as.data.frame(SCCof2)
names(SCCof2) = c("ParEst")

SCCof3 = as.data.frame(SCCof3)
names(SCCof3) = c("ParEst")

SCCof3 = as.data.frame(SCCof3)
names(SCCof3) = c("ParEst")

SCCof4 = as.data.frame(SCCof4)
names(SCCof4) = c("ParEst")

SCCof5 = as.data.frame(SCCof5)
names(SCCof5) = c("ParEst")

allParsSC = t(as.matrix(cbind(SCCof1, SCCof2, SCCof3, SCCof4, SCCof5)))

SCSes1 = as.data.frame(SCSes1)
names(SCSes1) = c("SE")

SCSes2 = as.data.frame(SCSes2)
names(SCSes2) = c("SE")

SCSes2 = as.data.frame(SCSes2)
names(SCSes2) = c("SE")

SCSes3 = as.data.frame(SCSes3)
names(SCSes3) = c("SE")

SCSes4 = as.data.frame(SCSes4)
names(SCSes4) = c("SE")

SCSes5 = as.data.frame(SCSes5)
names(SCSes5) = c("SE")

allSEsSC = t(as.matrix(cbind(SCSes1, SCSes2, SCSes3, SCSes4, SCSes5)))

allParsSesSCCom = mi.meld(q = allParsSC, se = allSEsSC)
allParsPaperSC = t(as.data.frame(allParsSesSCCom$q.mi))

allSesPaperSC = t(as.data.frame(allParsSesSCCom$se.mi))

allParSesPaperSC = cbind(allParsPaperSC, allSesPaperSC)
write.csv(allParSesPaperSC, "allParSesPaperSC.csv")
```


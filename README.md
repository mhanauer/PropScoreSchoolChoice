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
library(reshape2)
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
Remember to reverse code, because public schools is 1 and your hypothesis is that there private schools should have a make difference.
```{r}
datCat = data1[c(19:26)]
head(datCat)
data1[c(19:26)] = NULL
datCat
apply(datCat, 2, function(x){describe.factor(x)})
datCat = data.frame(apply(datCat, 2, function(x){(ifelse(x > 1, 0, 1))}))
head(datCat)

## Need to center the continous vars
data1 = data.frame(data1, datCat)
head(data1)
data1$S2REGSKL = ifelse(data1$S2REGSKL == 1,0,1) 
data1$S2REGSKL
```
Put the data into long format first.  Then for descriptives just subset the data for baseline characterisitcs.
```{r}
head(data1)
data1 = reshape(data1, varying = list(c("X1PRNCON", "X2PRNCON", "X4PRNCON"), c("X1RTHET", "X2RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X4BMI"), c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), c("X1PAR1AGE", "X2PAR1AGE", "X4PAR1AGE")), times = c(0,1,2), direction = "long")
data1
```


Descriptives:
Descirptives with no missing values.
Getting descriptives here.  Not a great solution, but we get with and without missing data.  Then change to long later and then impute again and use that data set to do the data analysis. Maybe run more imputations to make sure you get similar results.    

Getting the baseline data only 
```{r}
dataDesc = subset(data1, data1$time == 0)
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
a.out = amelia(x = data1, m=m, logs = c("X1HTOTAL"), noms = c("X1PAR1EMP", "X2POVTY", "X12PAR1ED_I", "X12LANGST","X_CHSEX_R", "S2REGSKL", "X1PAR1RAC", "X1_RACETHP_R"))
# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
summary(a.out)
compare.density(a.out, var = "X1PRNCON", main = "Observed and Imputed values of Self Control")
disperse(a.out)
```
Descriptives:
Descirptives when missing values are imputed for the tables
So need to write a loop for the functions that produce the means for each.  Because everything is dichomoized can just get counts from means.
Then you need to combine the correctly 

Here we need to subset the data so that it is only the first time point for each
```{r}
a.out.imputationsDesc = a.out$imputations
a.out.imputationsDesc = lapply(1:m, function(x) subset(a.out.imputationsDesc[[x]], time == 1))
dim(test[[1]])
descFun = function(x){
  x = data.frame(t(x))
}

mean.out = NULL
for(i in 1:m){
  mean.out[[i]] = apply(a.out.imputationsDesc[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}

mean.out = descFun(mean.out)
mean.out

sd.out = NULL
for(i in 1:m){
  sd.out[[i]] = apply(a.out.imputationsDesc[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out

mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out
```
Now we are analyzing one data set, using matchIT and seeing if we can get a regular regression and then a multilevel model with time.  We matched everyone.

Here are the estimates for the first model.  Need to grab the parameter estimates and sd's 
Cannot run a loop over the five data sets.  Try lm and work from there
```{r}
m.out = lapply(1:m, function(x) matchit(S2REGSKL ~ X1PRNCON + X2PRNCON + X4PRNCON +X1RTHET +X2RTHET + X4RTHET + X1MTHET + X2MTHET + X4MTHET + X1BMI +X2BMI + X4BMI + X1HTOTAL + X2HTOTAL + X4HTOTAL + X1PAR1AGE + X2PAR1AGE + X4PAR1AGE + X1PAR1EMP + X2POVTY + X12PAR1ED_I + X12LANGST + X_CHSEX_R  + X1PAR1RAC + X1_RACETHP_R, data = a.out$imputations[[x]], method = "nearest", ratio = 1))
m.out
summary(m.out[[1]])
plot(m.out[[1]], type = "jitter")
plot(m.out[[1]], type = "hist")

```
Now we need to develop the multilevel models, and do sequential tests

Combine par estimates




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
library(nlme)
library(dplyr)
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


data1 = cbind(X1TCHCON = data$X1TCHCON, X2TCHCON = data$X2TCHCON, X4TCHCON = data$X4TCHCON,X1RTHET= data$X1RTHET, X2RTHET = data$X2RTHET, X4RTHET = data$X4RTHET, X1MTHET = data$X1MTHET, X2MTHET = data$X2MTHET, X4MTHET = data$X4MTHET, X1BMI = data$X1BMI, X2BMI = data$X2BMI, X4BMI = data$X4BMI,X1HTOTAL = data$X1HTOTAL, X2HTOTAL = data$X2HTOTAL, X4HTOTAL = data$X4HTOTAL, X1PAR1AGE = data$X1PAR1AGE, X2PAR1AGE = data$X2PAR1AGE, X4PAR1AGE = data$X4PAR1AGE, X1PAR1EMP = data$X1PAR1EMP, X2POVTY = data$X2POVTY, X12LANGST = data$X12LANGST,X_CHSEX_R = data$X_CHSEX_R, X1PUBPRI = data$X1PUBPRI, X1PAR1RAC = data$X1PAR1RAC, X1_RACETHP_R = data$X_RACETHP_R)
# Change the -9 to NAs
data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})
#summary(data1)
data1 = as.data.frame(data1)
```
Make the demographics binary first then replace them.  Easier for imputation and not interested in the effects of demographics, just using them as controls.  

Put the demographics back into the data1 data set with them transformed.

Remember that R does not transform NA into zeros, so the code below works.
Remember to reverse code, because public schools is 1 and your hypothesis is that there private schools should have a make difference.

Primary lanuage if english is two.
```{r}
datCat = data1[c(19:25)]
head(datCat)
data1[c(19:25)] = NULL
datCatLang = datCat$X12LANGST
datCat$X12LANGST = NULL
X12LANGST = ifelse(datCatLang == 2, 1, 0)
apply(datCat, 2, function(x){describe.factor(x)})
datCat = data.frame(apply(datCat, 2, function(x){(ifelse(x > 1, 0, 1))}))
head(datCat)
datCat = cbind(datCat, X12LANGST)
## Need to recode the public private indicator
data1 = data.frame(data1, datCat)
data1$X1PUBPRI = ifelse(data1$X1PUBPRI == 1,0,1) 
summary(data1)
```
Descriptives:
Descirptives with no missing values.
Getting descriptives here.  Not a great solution, but we get with and without missing data.  Then change to long later and then impute again and use that data set to do the data analysis. Maybe run more imputations to make sure you get similar results.    

Getting the baseline data only 
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
round(apply(datCon, 2, mean_sd_fun),3)
datCat = dataDesc[c(19:25)]
head(datCat)
apply(datCat, 2, function(x){describe.factor(x)})
```
Now getting descriptives using data imputation.  

```{r}
#head(data1)
m = 5
a.out = amelia(x = data1, m=m, logs = c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), noms = c("X1PAR1EMP", "X2POVTY", "X12LANGST","X_CHSEX_R", "X1PUBPRI", "X1PAR1RAC", "X1_RACETHP_R"))
# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
summary(a.out)
#compare.density(a.out, var = "X1TCHCON", main = "Observed and Imputed values of Self Control Time 1")
#compare.density(a.out, var = "X2TCHCON", main = "Observed and Imputed values of Self Control Time 2")
#compare.density(a.out, var = "X4TCHCON", main = "Observed and Imputed values of Self Control Time 4")
#compare.density(a.out, var = "X12LANGST")
#disperse(a.out)
```
Descriptives:
Descirptives when missing values are imputed for the tables
So need to write a loop for the functions that produce the means for each.  Because everything is dichomoized can just get counts from means.
Then you need to combine the correctly 

Here we need to subset the data so that it is only the first time point for each
```{r}
a.out.imputationsDesc = a.out$imputations
dim(a.out.imputationsDesc[[1]])
a.out.imputationsDesc = lapply(1:m, function(x) subset(a.out.imputationsDesc[[x]]))
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

This model works wide format, because we only want pretreatment covariates or baseline covariates
Need to grab the weights (these are indicating whether the person is in the treatment). Then grab the treatment indicator, because it is erased in the regression.  

You need to grab the time varying covariates, because they are not included after matching
```{r}
describe.factor(a.out$imputations$imp1$X1PUBPRI)
describe.factor(X1PUBPRI)
m.out = lapply(1:m, function(x) matchit(X1PUBPRI ~ X1TCHCON  +X1RTHET  + X1MTHET + X1BMI  + X1HTOTAL  + X1PAR1AGE + X1PAR1EMP + X2POVTY + X12LANGST + X_CHSEX_R  + X1PAR1RAC + X1_RACETHP_R, data = a.out$imputations[[x]], method = "nearest", ratio = 1))
#plot(m.out[[1]], type = "jitter")
#plot(m.out[[1]], type = "hist")
m.out = lapply(1:m, function(x) match.data(m.out[[x]]))
```
Put the data into long format first.  Must do this for each data seperatly, cannot loop or lapply it.
```{r}
dat1 = data.frame(m.out[[1]])
dat2 = data.frame(m.out[[2]])
dat3 = data.frame(m.out[[3]])
dat4 = data.frame(m.out[[4]])
dat5 = data.frame(m.out[[5]])
head(dat4)
dat1 = reshape(dat1, varying = list(c("X1TCHCON", "X2TCHCON", "X4TCHCON"), c("X1RTHET", "X2RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X4BMI"), c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), c("X1PAR1AGE", "X2PAR1AGE", "X4PAR1AGE")), times = c(0,1,2), direction = "long")

dat2 = reshape(dat2, varying = list(c("X1TCHCON", "X2TCHCON", "X4TCHCON"), c("X1RTHET", "X2RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X4BMI"), c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), c("X1PAR1AGE", "X2PAR1AGE", "X4PAR1AGE")), times = c(0,1,2), direction = "long")


dat3 = reshape(dat3, varying = list(c("X1TCHCON", "X2TCHCON", "X4TCHCON"), c("X1RTHET", "X2RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X4BMI"), c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), c("X1PAR1AGE", "X2PAR1AGE", "X4PAR1AGE")), times = c(0,1,2), direction = "long")


dat4 = reshape(dat4, varying = list(c("X1TCHCON", "X2TCHCON", "X4TCHCON"), c("X1RTHET", "X2RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X4BMI"), c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), c("X1PAR1AGE", "X2PAR1AGE", "X4PAR1AGE")), times = c(0,1,2), direction = "long")

dat5 = reshape(dat5, varying = list(c("X1TCHCON", "X2TCHCON", "X4TCHCON"), c("X1RTHET", "X2RTHET", "X4RTHET"), c("X1MTHET", "X2MTHET", "X4MTHET"), c("X1BMI", "X2BMI", "X4BMI"), c("X1HTOTAL", "X2HTOTAL", "X4HTOTAL"), c("X1PAR1AGE", "X2PAR1AGE", "X4PAR1AGE")), times = c(0,1,2), direction = "long")

datAll = list(dat1, dat2, dat3, dat4, dat5)

#n matched for descriptives here look at time zero, because comparing baseline
describe.factor(dat1$time)

#n matched for entire 

```
Need descriptives for matched data.  Do this for public and then private.  Need to subset for time equals one, because comparing just at baseline.
```{r}
mean_public = NULL
for(i in 1:m){
  mean_public[[i]] = subset(datAll[[i]], X1PUBPRI == 0 & time == 0)
  mean_public[[i]]= apply(mean_public[[i]], 2, mean)
  mean_public[[i]] = data.frame(t(mean_public[[i]]))
}
mean_public = bind_rows(mean_public); mean_public

sd_public = NULL
for(i in 1:m){
  sd_public[[i]] = subset(datAll[[i]], X1PUBPRI == 0 & time == 0)
  sd_public[[i]]= apply(sd_public[[i]], 2, sd)
  sd_public[[i]] = data.frame(t(sd_public[[i]]))
}
sd_public = bind_rows(sd_public); sd_public

mean_sd_public = mi.meld(q = mean_public, se = sd_public); mean_sd_public

mean_private = NULL
for(i in 1:m){
  mean_private[[i]] = subset(datAll[[i]], X1PUBPRI == 1 & time == 0)
  mean_private[[i]]= apply(mean_private[[i]], 2, mean)
  mean_private[[i]] = data.frame(t(mean_private[[i]]))
}
mean_private = bind_rows(mean_private); mean_private

sd_private = NULL
for(i in 1:m){
  sd_private[[i]] = subset(datAll[[i]], X1PUBPRI == 1 & time == 0)
  sd_private[[i]]= apply(sd_private[[i]], 2, sd)
  sd_private[[i]] = data.frame(t(sd_private[[i]]))
}
sd_private = bind_rows(sd_private); sd_private

mean_sd_private = mi.meld(q = mean_private, se = sd_private); mean_sd_private
```


Now we need to develop the multilevel models, and do sequential tests
Need four models, one with intercept only, random intercepts, random slopes + random intercepts, plus autoregressive strcuture. Cite that you do not need matching covariates in the model.  Only need X1RTHET  + X1MTHET + X1BMI  + X1HTOTAL  + X1PAR1EMP these, because they are time varying covariates.

Could wrap everything is a function and only need to write it once.  Just start with null model
model3 with corAR1 model won't converage and neither will the random intercepts.  Just say that we evaluated the random slopes versus random intercepts and it was a better fit.
```{r}
numMatched = subset(datAll[[1]], time == 0)
numMatched
dim()
model1 = lapply(1:m, function(x) lme(fixed  = X1TCHCON ~ time, random =  ~ 1 | id, data = datAll[[x]], method = "ML"))

summary(model1[[1]])

model2 = lapply(1:m, function(x) lme(X1TCHCON ~ X1PUBPRI*time + X1RTHET + X1MTHET + X1BMI + X1HTOTAL + X1PAR1EMP, random =  ~ time | id, data = datAll[[x]], method = "ML"))

anova1v2 = lapply(1:m, function(x) anova(model1[[x]], model2[[x]]))
anova1v2

model3 = lapply(1:m, function(x) lme(X1TCHCON ~ X1PUBPRI*time, random =  ~ time | id, data = datAll[[x]], correlation = corAR1(), method = "ML"))

```
Now combine par and se estimates from model 2.  Create this code to finish.  DF  = 10078
```{r}
coefs = NULL
for(i in 1:m){
  coefs[[i]] = summary(model2[[i]])
  coefs[[i]] = coefs[[i]]$tTable[[9,1]]
}
coefs = t(data.frame(coefs))

se = NULL
for(i in 1:m){
  se[[i]] = summary(model2[[i]])
  se[[i]] = se[[i]]$tTable[[9,2]]
}
se = t(data.frame(se))

parSe = mi.meld(q = coefs, se = se)
tStat = parSe$q.mi/parSe$se.mi
2*pt(-abs(tStat), df = 10078)
upper = parSe$q.mi +(2*parSe$se.mi)
upper
lower = parSe$q.mi-(2*parSe$se.mi)
lower
```

---
title: "PropScoreEquations"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}Time_{ij} + \beta_{xj}X_{ij} + e_{ij}}~~~ (1.1)$$

Level 2 Intercept: Here the intercept is broken down into the constant plus the effect of the intervention, which is at level 2 in the intercept because it does not vary over time only by person and the error term which varies by person. 

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + \gamma_{01}Private_{j} + u_{0j}} ~~~ (1.2)$$


Then there is level the two slope which has the constant effect, plus the slope for the intervention for each person, plus a random error term that unique to each person.  

$$ Level~2~Slope~Time:~~~{\beta_{1j} = \gamma_{10} + \gamma_{11}Private_{j} + u_{1j}} ~~~ (1.3)$$

Then we have the mixed model, which has all the components combined
$$Mixed~model:~~~{y_{ij} =  (\gamma_{00}+ \gamma_{01}Private_{j} + u_{0j}) + (\gamma_{1x}*X_{ij}) + (\gamma_{10}}+\gamma_{11}*Private_{j} +u_{1j})*Time_{ij} + e_{ij}~~~ (1.4)$$ 





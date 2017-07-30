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

compare.density(a.out, var = "X1PRNCON", main = "Observed and Imputed values of Self Control")
compare.density(a.out, var = "X1PRNSOC", main = "Observed and Imputed values of Social Interaction")
disperse(a.out)
summary(a.out)

# Now we can creat seperate data set and then analyze them seperately and combine them later with the mi.meld function in Ameila
write.amelia(obj = a.out, file.stem = "ECLSK")

```
Now we are analyzing one data set, using matchIT and seeing if we can get a regular regression and then a multilevel model with time.  We matched everyone.

Here are the estimates for the first model.  Need to grab the parameter estimates and sd's 
```{r}
library(MatchIt)
setwd("~/Desktop")
ECLSK1  = read.csv("ECLSK1.csv", header = TRUE)
ECLSK1 = ECLSK1[c(-1)]
ECLSK1 = na.omit(ECLSK1)
ECLSK1 = as.data.frame(ECLSK1)

m.out1 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, data = ECLSK1, method = "nearest", ratio = 1)
summary(m.out1)
plot(m.out1, type = "jitter")
plot(m.out1, type = "hist")

# Now getting descriptives
m.data1 <- match.data(m.out1)
m.dataMeans1 = apply(m.data1, 2, mean)
m.dataSD1 = apply(m.data1,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC1 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out1))
SCCof1 =  z.outSC1$get_coef()
SCSes1 = z.outSC1$get_se()


# Here is for the social interaction variable.
z.outSI1 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out1))

SICof1 =  z.outSI1$get_coef()
SISes1 = z.outSI1$get_se()

```
Now we get the estimates for the second data set
```{r}
setwd("~/Desktop")
ECLSK2  = read.csv("ECLSK2.csv", header = TRUE)
ECLSK2 = ECLSK2[c(-1)]
ECLSK2 = na.omit(ECLSK2)
ECLSK2 = as.data.frame(ECLSK2)

m.out2 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, data = ECLSK2, method = "nearest", ratio = 1)

plot(m.out2, type = "jitter")
plot(m.out2, type = "hist")

# Now getting descriptives
m.data2 <- match.data(m.out2)
m.dataMeans2 = apply(m.data2, 2, mean)
m.dataSD2 = apply(m.data2,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC2 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out2))
SCCof2 =  z.outSC2$get_coef()
SCSes2 = z.outSC2$get_se()


# Here is for the social interaction variable.
z.outSI2 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out2))

SICof2 =  z.outSI2$get_coef()
SISes2 = z.outSI2$get_se()
```
Now get the estimates for the third data set
```{r}
setwd("~/Desktop")
ECLSK3  = read.csv("ECLSK3.csv", header = TRUE)
ECLSK3 = ECLSK3[c(-1)]
ECLSK3 = na.omit(ECLSK3)
ECLSK3 = as.data.frame(ECLSK3)

m.out3 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, data = ECLSK3, method = "nearest", ratio = 1)

plot(m.out3, type = "jitter")
plot(m.out3, type = "hist")

# Now getting descriptives
m.data3 <- match.data(m.out3)
m.dataMeans3 = apply(m.data3, 2, mean)
m.dataSD3 = apply(m.data3,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC3 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out3))
SCCof3 =  z.outSC3$get_coef()
SCSes3 = z.outSC3$get_se()


# Here is for the social interaction variable.
z.outSI3 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out3))

SICof3 =  z.outSI3$get_coef()
SISes3 = z.outSI3$get_se()
```
Now get the estimates for the fourth data set
```{r}
setwd("~/Desktop")
ECLSK4  = read.csv("ECLSK4.csv", header = TRUE)
ECLSK4 = ECLSK4[c(-1)]
ECLSK4 = na.omit(ECLSK4)
ECLSK2 = as.data.frame(ECLSK4)

m.out4 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, data = ECLSK4, method = "nearest", ratio = 1)
summary(m.out2)
plot(m.out4, type = "jitter")
plot(m.out4, type = "hist")

# Now getting descriptives
m.data4 <- match.data(m.out4)
m.dataMeans4 = apply(m.data4, 2, mean)
m.dataSD4 = apply(m.data4,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC4 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out4))
SCCof4 =  z.outSC4$get_coef()
SCSes4 = z.outSC4$get_se()


# Here is for the social interaction variable.
z.outSI4 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out4))

SICof4 =  z.outSI4$get_coef()
SISes4 = z.outSI4$get_se()
```
Now we get the fifth data set estimates
```{r}
setwd("~/Desktop")
ECLSK5  = read.csv("ECLSK5.csv", header = TRUE)
ECLSK5 = ECLSK5[c(-1)]
ECLSK5 = na.omit(ECLSK5)
ECLSK2 = as.data.frame(ECLSK5)

m.out5 = matchit(S1REGSKL ~ X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, data = ECLSK5, method = "nearest", ratio = 1)
summary(m.out2)
plot(m.out5, type = "jitter")
plot(m.out5, type = "hist")

# Now getting descriptives
m.data5 <- match.data(m.out5)
m.dataMeans5 = apply(m.data5, 2, mean)
m.dataSD5 = apply(m.data5,2, sd)


library(Zelig)
# Grabbing the parameter estimates and se's
z.outSC5 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out5))
SCCof5 =  z.outSC5$get_coef()
SCSes5 = z.outSC5$get_se()


# Here is for the social interaction variable.
z.outSI5 = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out5))

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


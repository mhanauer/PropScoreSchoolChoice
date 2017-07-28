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
plot(m.out, type = "jitter")
plot(m.out, type = "hist")

# Now getting descriptives
m.data1 <- match.data(m.out1)
m.dataMeans1 = apply(m.data1, 2, mean)
m.dataMeans1
m.dataSD1 = apply(m.data1,2, sd)


library(Zelig)
# Here we have the first example from the matchit pdf, but not sure what it is doing
z.outSC1 = zelig(X1PRNCON ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNSOC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out1))
z.outSC1$get_coef()
z.outSC1$get_se()

z.outSC1

#x.out = setx(z.outSC, S1REGSKL = 0)
#x1.out = setx(z.outSC, S1REGSKL = 1)
#s.out = sim(z.outSC, x = x.out, x1 = x1.out)
#summary(s.out)

# Here is for the social interaction variable.
z.outSI = zelig(X1PRNSOC ~ + S1REGSKL +  X1_CHSEX_R + X1HPARNT + X1CHGSCH + X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R + X_AMINAN_R + X_HAWPI_R + X1PAR1RAC + X1PRNCON + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1POVTY + X1PAR1ED_I + X1KAGE_R, model = "ls", data = match.data(m.out))

summary(z.outSI)
```


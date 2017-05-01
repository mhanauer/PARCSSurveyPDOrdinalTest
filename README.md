---
title: "PARCS Survey PD Ordinal Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Grab the PD variable and the school level variable and combine them across data sets.  Then omit missing variables
```{r}
setwd("~/Desktop/QualData")
mccsc = read.csv("MCCSCStaffSurvey.csv", header = TRUE)
rbbcsc = read.csv("RBBCSCStaffSurvey.csv", header = TRUE)

mccsc1 = mccsc[c("Q1_6", "Q44")]
mccsc2 = mccsc1[-c(1:2), ]

rbbcsc1 = rbbcsc[c("Q1_6", "Q15")]

rbbcsc2 = rbbcsc1[-c(1:2), ]

names(rbbcsc2) = c("Q1_6", "Q44")

both = rbind(mccsc2, rbbcsc2)

both = na.omit(both)

```
Now we change the PD variable to be numeric.
```{r}
both = apply(both, 2, function(x){ifelse(x == "Strongly agree", 7, x)})
both = as.data.frame(both)
both = apply(both, 2, function(x){ifelse(x == "Agree", 6, x)})
both = as.data.frame(both)
both = apply(both, 2, function(x){ifelse(x == "Somewhat agree", 5, x)})
both = as.data.frame(both)
both = apply(both, 2, function(x){ifelse(x == "Neither agree nor disagree", 4, x)})
both = as.data.frame(both)
both = apply(both, 2, function(x){ifelse(x == "Somewhat disagree", 3, x)})
both = as.data.frame(both)
both = apply(both, 2, function(x){ifelse(x == "Disagree", 2, x)})
both = as.data.frame(both)
both = apply(both, 2, function(x){ifelse(x == "Strongly disagree", 1, x)})
both = as.data.frame(both)

both = apply(both, 1, function(x){ifelse(x == "Primary school teacher", 1, x)})
both = as.data.frame(both)
both = apply(both, 1, function(x){ifelse(x == "Secondary school teacher", 2, x)})
both = as.data.frame(both)
head(both)

```
Now we need to grab only those school level data points with primary and secondary teachers
```{r}
both = both[both$Q44 %in% c(1,2), ]
names(both) = c("SELPDScore", "TeacherType")
head(both)
dim(both)
write.csv(both, "both.csv")
```
Now we can run Krusckhe's two group ordinal analysis
```{r}
# Example for Jags-Yord-Xnom2grp-MrobustHet.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
# Load The data file 
#....................................................................
setwd("~/Desktop/QualData")
myDataFrame = read.csv( file="both.csv" )
yName="SELPDScore"
xName="TeacherType"
fileNameRoot="PARCSSurveyOrdinalTest-"
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Yord-Xnom2grp-MnormalHet.R")
#------------------------------------------------------------------------------- 
# Optional: Specify filename root and graphical format for saving output.
# Otherwise specify as NULL or leave saveName and saveType arguments 
# out of function calls.
graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( datFrm=myDataFrame , yName=yName , xName=xName ,
                    numSavedSteps=12000 , thinSteps=3 , 
                    saveName=fileNameRoot ) 
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
# Get all parameter names:
parameterNames = varnames(mcmcCoda) 
# for ( parName in parameterNames ) {
#   diagMCMC( codaObject=mcmcCoda , parName=parName , 
#                 saveName=fileNameRoot , saveType=graphFileType )
# }
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda, 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=myDataFrame , yName=yName , xName=xName , 
          pairsPlot=TRUE , saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

```


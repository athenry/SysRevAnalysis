## Test to compare increase of of systematic reviews in engineering literature to increase in number of publications in the same time span.

## Load needed libraries
library(tidyverse)
library(bibliometrix)
library(tseries)

## read in our data. We will have three time series: all journal and conference articles published from 2000-2018, the systematic review articles published in the same time period, and all the non-SR articles published in that time period.

statdata <- read.csv(file = "Data/statdata.csv", header = TRUE)
SRdata <- read.csv(file = "Data/SRquery.csv", header = TRUE)
Adata <- read.csv(file = "Data/Adata.csv", header = TRUE)

## generate a basic plot to examine the data
plot1 <- ggplot(data = SRdata, aes(Year, Count)) + geom_line(col = "red") + geom_line(data = statdata, aes(Year, Count/1000), col = "blue") + theme_minimal()

## Perform regressions on each series
regP <- lm(formula = Count~Year, data = statdata)
regA <- lm(formula = Count~Year, data = Adata)
regB <- lm(formula = Count~Year, data = SRdata)

## Calculate the residual sum of squares for each regression
rssP <- sum(residuals(regP)^2)
rssA <- sum(residuals(regA)^2)
rssB <- sum(residuals(regB)^2)

## Chow test

## k is the number of parameters for our model
k=2

## Determine the F critical value 
Fcrit = qf(.95, df1=regA$df.residual, df2 = regB$df.residual)
Fcrit

## Calculate the Chow statistic
Chow_statistic = (((rssP - (rssA + rssB))/k)/((rssA + rssB)/(regA$df.residual + regB$df.residual - 2*k)))
Chow_statistic

## If the Chow statistic is larger than our F critical value, we reject the null hypothesis
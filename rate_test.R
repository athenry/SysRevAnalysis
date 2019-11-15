## Test to compare increase of of systematic reviews in engineering literature to increase in number of publications in the same time span.

## Load needed libraries
library(tidyverse)
library(tseries)

## read in our data. We will have three time series: all journal and conference articles published from 2000-2018, the systematic review articles published in the same time period, and all the non-SR articles published in that time period.

statdata <- read.csv(file = "Data/statdata.csv", header = TRUE)
SRdata <- read.csv(file = "Data/SRdata.csv", header = TRUE)
Adata <- read.csv(file = "Data/Adata.csv", header = TRUE)

alldata <- merge(statdata, SRdata, by = "Year")
colnames(alldata) <- c("Year", "allpubs", "SR")
alldata$share <- (alldata$SR / alldata$allpubs) * 100

## generate a basic plot to examine the data
plot1 <- ggplot(data = alldata) + geom_line(aes(Year, SR), col = "red") + geom_line(aes(Year, allpubs/1000), col = "blue") + theme_minimal()
plot1

## Examine the change in proportion of articles which use SR methodology
plot2 <- ggplot(data = alldata) + geom_line(aes(Year, share), col = "green")
plot2

## Perform a trend test to confirm that the share of articles using this methodology shows a pronoun

## Perform regressions on each series, omitting intercept
regP <- lm(formula = Count~Year - 1, data = subset(statdata, Year > 2014))
regA <- lm(formula = Count~Year - 1, data = subset(Adata, Year > 2014))
regB <- lm(formula = Count~Year - 1, data = subset(SRdata, Year >2014))

## Calculate the residual sum of squares for each regression
rssP <- sum(residuals(regP)^2)
rssA <- sum(residuals(regA)^2)
rssB <- sum(residuals(regB)^2)

## Chow test. Null hypothesis  = no significant difference

## k is the number of parameters for our model
k=2

## Determine the F critical value 
Fcrit = qf(.95, df1=regA$df.residual, df2 = regB$df.residual)
Fcrit

## Calculate the Chow statistic
Chow_statistic = ((rssP - (rssA + rssB))/k)/((rssA + rssB)/(regA$df.residual + regB$df.residual - 2*k))
Chow_statistic

## If the Chow statistic is larger than our F critical value, we reject the null hypothesis
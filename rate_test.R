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
##alldata$time <- (alldata$Year-2000)

## generate a basic plot to examine the data
plot1 <- ggplot(data = alldata) + geom_point(aes(Year, SR), col = "#007C41") + geom_point(aes(Year, allpubs/1000), col = "#FFDB05") + theme_minimal()
plot1

## Examine the change in proportion of articles which use SR methodology
plot2 <- ggplot(data = alldata) + geom_point(aes(x=Year, y=share), color = "#007C41") + theme_minimal() 
plot2

## perform a linear regression on the share of articles 

model <- lm(share ~ poly(Year, 2), data = alldata)

## examine the summary of the model
summary(model)

## create predicted values for our years based on the model
df <- predict(model, data.frame(Year = alldata$Year))
alldata$predValue <- df

## add the predicted values to our plot
plot3 <- ggplot(data = alldata, aes(x=Year, y=share)) + geom_point() + geom_line(aes(x=Year, y=predValue)) + theme_minimal()
plot3

## bar graph of count of articles per year
plot4 <- ggplot(data=alldata, aes(x=Year, y=SR)) + geom_col() + theme_minimal()
plot4
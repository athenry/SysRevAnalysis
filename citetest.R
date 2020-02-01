## testing whether SR articles outperform their journals

citation <- read.csv(file = "Data/citation.csv", header = TRUE, na)

citedata <- na.omit(select(citation, Title, Published.Year, Journal, TC..pub.year...2., JIF..pub.year...2.))

colnames(citedata) <- c("Title", "Year", "Journal", "TC", "JIF")

citedata$test <- (citedata$TC>citedata$JIF)

ggplot(data = citedata) + geom_bar(aes(test)) + geom_text(stat = 'count', aes(x= test, label = ..count..)) + theme_minimal()

ggplot(data = citedata) + geom_point(aes(x=JIF, y=TC)) +geom_abline(slope = 1, intercept = 0,0) + theme_minimal()


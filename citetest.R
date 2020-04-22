## testing whether SR articles outperform their journals

citation <- read.csv(file = "Data/citation.csv", header = TRUE)

citedata <- na.omit(select(citation, Covidence.., Title, Published.Year, Journal, TC..pub.year...2., JIF..pub.year...2.))

colnames(citedata) <- c("Covidence", "Title", "Year", "Journal", "TC", "JIF")

citedata$test <- (citedata$TC>citedata$JIF)

plot5 <- ggplot(data = citedata) + geom_bar(aes(test)) + geom_text(stat = 'count', aes(x= test, label = ..count..)) + theme_minimal()
plot5

plot6 <- ggplot(data = citedata) + geom_point(aes(x=JIF, y=TC), color = "#007C41", show.legend = FALSE) + geom_abline(slope = 1, intercept = 0,0, color = "#FFDB05") + theme_minimal()
plot6

## in case we want to include a table of the citation counts
write.csv(citedata, file = "citedata.csv", quote = TRUE, row.names = FALSE)

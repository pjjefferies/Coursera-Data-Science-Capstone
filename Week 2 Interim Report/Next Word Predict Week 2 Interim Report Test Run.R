#```{r exploratory analysis line count, cache=TRUE, echo=FALSE, eval=TRUE}
#Read Text Files into variables
# inputDataFilenames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
# con <- file(inputDataFilenames[1], "r")
# blogFile <- readLines(con=con)
# close(con)
# blogLength <- length(blogFile)  #Count Number of Lines in file
# rm(blogFile)
# 
# con <- file(inputDataFilenames[2], "r")
# newsFile <- readLines(con=con)
# close(con)
# newsLength <- length(newsFile)  #Count Number of Lines in file
# rm(news)
# 
# con <- file(inputDataFilenames[3], "r")
# twitFile <- readLines(con=con)
# close(con)
# twitLength <- length(twitFile)  #Count Number of Lines in file
# rm(twitFile)
# 
# allFilesLength <- blogLength + newsLength + twitLength
#```



#```{r exploratory analysis word count, cache=TRUE, echo=FALSE, eval=TRUE}
#Analyze previously created words list from files

# blogFileWordList <-
#     read.csv("blogWordListFullSortedCleaned.csv", row.names=1)
# blogTotalWords <- sum(blogFileWordList$count)
# blogUniqueWords <- nrow(blogFileWordList)
# rm(blogFileWordList)
# 
# newsFileWordList <-
#     read.csv("newsWordListFullSortedCleaned.csv", row.names=1)
# newsTotalWords <- sum(newsFileWordList$count)
# newsUniqueWords <- nrow(newsFileWordList)
# rm(newsFileWordList)
# 
# twitFileWordList <-
#     read.csv("twitWordListFullSortedCleaned.csv", row.names=1)
# twitTotalWords <- sum(twitFileWordList$count)
# twitUniqueWords <- nrow(twitFileWordList)
# rm(twitFileWordList)

#allFilesWordList <- read.csv("allWordListSortByCount.csv", row.names=1)
allTotalWords <- sum(allFilesWordList$count)
allUniqueWords <- nrow(allFilesWordList)

#Sort words by count to be sure it's sorted
allFilesWordList <-
    allFilesWordList[order(allFilesWordList[,"count"],
                           decreasing = TRUE),
                     , drop=FALSE]
topNWordsToPareto <- 100
allFileTopWordList <- allFilesWordList[1:topNWordsToPareto,,drop=FALSE]
#rm(allFilesWordList)
allFileTopWordList$percent <- allFileTopWordList$count / allTotalWords

allFileTopWordList[1, "cumpercent"] <- allFileTopWordList[1,"percent"]
allFileTopWordList[1,"cumSum"] <- allFileTopWordList[1, "count"]
for(aLineNo in 2:nrow(allFileTopWordList)) {
    allFileTopWordList[aLineNo, "cumpercent"] <-
        allFileTopWordList[aLineNo-1, "cumpercent"] +
        allFileTopWordList[aLineNo, "percent"]
    allFileTopWordList[aLineNo, "cumSum"] <-
        allFileTopWordList[aLineNo-1, "cumSum"] +
        allFileTopWordList[aLineNo, "count"]
}
topNWordsToParetoCumPercent <- round(100 *
    allFileTopWordList[nrow(allFileTopWordList), "cumpercent"],1)
maxCum <- max(allFileTopWordList$cumSum, na.rm=TRUE)
#```


par(mar=c(5,5,4,5))
pc <- barplot(allFileTopWordList$count,
        width=1, space=0.2, border=NA, axes=FALSE,
        ylim=c(0,1.05*maxCum),
        ylab="Cummulative Counts", cex.names=0.7,
        names.arg = allFileTopWordList$word,
        las=3,
        main="All Files Word Count Pareto")
lines(pc, allFileTopWordList$cumSum, type="b", cex=0.5, pch=19, col="cyan4")
box(col="black")

axis(side=2, at=NULL, las=1, col.axis="black",
     col="black", cex.axis=0.8)
axis(side=4, at=seq(0,topNWordsToParetoCumPercent*y2axisScale,
                    length.out=10),
     labels=paste0(round(seq(0, topNWordsToParetoCumPercent,
                             length.out=10), 2),"%"),
     las=1, col.axis="cyan4", col="cyan4", cex.axis=0.8)

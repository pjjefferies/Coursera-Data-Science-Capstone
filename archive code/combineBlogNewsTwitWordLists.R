#
#Combine twit, news, blog word file lists
#

fileToComb <- c("blogWordListFullSortedCleaned.csv",
                "newsWordListFullSortedCleaned.csv",
                "twitWordListFullSortedCleaned.csv")
combFileNameWordSort <- "allWordListSortByWords.csv"
combFileNameCountSort <- "allWordListSortByCount.csv"

writeLines(paste("Reading file", fileToComb[1]))
combWordListDF <- read.csv(fileToComb[1], row.names = 1)

for(aFileName in fileToComb[2:length(fileToComb)]) {
    writeLines(paste("Reading file", aFileName))
    tempWordListDF <- read.csv(aFileName, row.names=1)

    writeLines(paste("   Adding 'word' column to", aFileName))
    tempWordListDF$word <- row.names(tempWordListDF)
    
    writeLines(paste("   Merging full and new dataframes"))
    wordListDF <- merge(combWordListDF, tempWordListDF, by="word", all=TRUE)
    
    writeLines(paste("   Adding row names back to wordListDF"))
    row.names(wordListDF) <- wordListDF$word
    
    writeLines(paste("   Replacing NA's with zeros"))
    wordListDF[is.na(wordListDF$count.x),"count.x"] <- 0
    wordListDF[is.na(wordListDF$count.y),"count.y"] <- 0
    
    writeLines(paste("   Adding counts from merged dataframes"))
    wordListDF$count <- wordListDF$count.x + wordListDF$count.y
    
    writeLines(paste("   Remove separte DF counts"))
    wordListDF <- wordListDF[,c("word", "count")]
    
    writeLines(c("   Finished adding words.", ""))
}

writeLines(paste("Sorting Final Word List Dataframe by Word"))
wordListDF <- wordListDF[order(wordListDF[,"word"],
                               decreasing = FALSE),
                         , drop=FALSE]

writeLines(paste("Saving Final Word List Sorted by words"))
write.csv(wordListDF, combFileNameWordSort)

writeLines(paste("Sorting Final Word List Dataframe by Count"))
wordListDF <- wordListDF[order(wordListDF[,"count"],
                               decreasing = TRUE),
                         , drop=FALSE]

writeLines(paste("Saving Final Word List Sorted by count"))
write.csv(wordListDF, combFileNameCountSort)

rm(fileToComb, combFileNameWordSort, combFileNameCountSort, combWordListDF, wordListDF, tempWordListDF,
   aFileName)
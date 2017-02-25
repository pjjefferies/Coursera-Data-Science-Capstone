#
#Clean Full Word Files of ' marks and combine raw words with others
#


dirtyFileName <- "blogWordListFullSorted.csv"
cleanedFileName <- "blogWordListFullSortedCleaned.csv"

dirtyFileDF <- read.csv(dirtyFileName, row.names = 1)

rowsWithBegQuote <- grepl("^'+", dirtyFileDF$word)
#rowsWithOutBegQuote <- grep("^'+", dirtyFileDF$word, invert=TRUE)

cleanFileDF <- dirtyFileDF[!rowsWithBegQuote,,drop=FALSE]

veryDirtyFileDF <- dirtyFileDF[rowsWithBegQuote,,drop=FALSE]
rm(dirtyFileDF, rowsWithBegQuote)
#veryDirtyFileDFSorted <- veryDirtyFileDF[order(veryDirtyFileDF[,"word"]),
#                                         ,
#                                         drop=FALSE]

lessVeryDirtyWordList <- gsub("^'+", "", veryDirtyFileDF$word)
#cleanedDirtyFileDF <- veryDirtyFileDF[,"count", drop=FALSE]
cleanedDirtyFileDF <- veryDirtyFileDF
rm(veryDirtyFileDF)
cleanedDirtyFileDF$word <- lessVeryDirtyWordList
rm(lessVeryDirtyWordList)

#Have to combine rows with duplicate 'word's at this point
tempCleanedWordList <- data.frame(word=as.character(),
                                  count=as.integer())
for(aWordNo in 1:nrow(cleanedDirtyFileDF)) {
    aWord <- cleanedDirtyFileDF[aWordNo, "word"]
    ifelse(aWord %in% tempCleanedWordList$word,
           tempCleanedWordList[aWord, "count"] <- tempCleanedWordList[aWord, "count"] +
               cleanedDirtyFileDF[aWordNo, "count"],
           tempCleanedWordList[aWord, "count"] <- cleanedDirtyFileDF[aWordNo, "count"])
    }
rm(cleanedDirtyFileDF, aWord, aWordNo)
tempCleanedWordList$word <- row.names(tempCleanedWordList)

#merge cleaned to dirty
writeLines(paste("   Merging clean and cleaned dirty dataframes"))
cleanFileDF <- merge(cleanFileDF, tempCleanedWordList, by="word", all=TRUE)
rm(tempCleanedWordList)
writeLines(paste("   Adding row names back to cleanFileDF"))

row.names(cleanFileDF) <- cleanFileDF$word
writeLines(paste("   Replacing NA's with zeros"))
cleanFileDF[is.na(cleanFileDF$count.x),"count.x"] <- 0
cleanFileDF[is.na(cleanFileDF$count.y),"count.y"] <- 0
writeLines(paste("   Adding counts from merged dataframes"))
cleanFileDF$count <- cleanFileDF$count.x + cleanFileDF$count.y
writeLines(paste("   Remove separte DF counts"))
cleanFileDF <- cleanFileDF[,c("word", "count")]
cleanFileDF <- cleanFileDF[order(cleanFileDF[,"count"],
                                 decreasing = TRUE),
                           , drop=FALSE]

writeLines(c("   Finished merging DBs.", "   Saving overall file"))
write.csv(cleanFileDF, cleanedFileName)
writeLines("   Finished saving file")
rm(cleanedFileName, dirtyFileName, cleanFileDF)



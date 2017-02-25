#
#

blogWordFilesList <- list.files("C:/Users/Lynn/Data/tmp/Capstone",
                                pattern="^blogWordList[0-9]+.csv")
noFilesToAdd <- length(blogWordFilesList)
filesRemainingToAdd <- noFilesToAdd
writeLines(paste("There are", noFilesToAdd, "files to add to complete list"))
blogWordFull <- "blogWordListFull.csv"
blogWordFullSorted <- "blogWordListFullSorted.csv"

if(file.exists(blogWordFull)) {
    wordListDF <- read.csv(blogWordFull)
    writeLines("Read in existing blogWordListFull to start")
} else {
    wordListDF <- data.frame(word=as.character(),
                             count=as.integer())
    writeLines("Starting new blogWordListFull file")
}

for(aFileName in blogWordFilesList) {
    thisStartTime <- proc.time()
    writeLines(paste("Reading file", aFileName))
    tempWordListDF <- read.csv(aFileName, row.names=1)
    writeLines(paste("   Adding 'word' column to", aFileName))
    tempWordListDF$word <- row.names(tempWordListDF)
    writeLines(paste("   Merging full and new dataframes"))
    wordListDF <- merge(wordListDF, tempWordListDF, by="word", all=TRUE)
    writeLines(paste("   Adding row names back to wordListDF"))
    row.names(wordListDF) <- wordListDF$word
    writeLines(paste("   Replacing NA's with zeros"))
    wordListDF[is.na(wordListDF$count.x),"count.x"] <- 0
    wordListDF[is.na(wordListDF$count.y),"count.y"] <- 0
    writeLines(paste("   Adding counts from merged dataframes"))
    wordListDF$count <- wordListDF$count.x + wordListDF$count.y
    writeLines(paste("   Remove separte DF counts"))
    wordListDF <- wordListDF[,c("word", "count")]
    
    writeLines(c("   Finished adding words.", "   Saving overall file"))
    write.csv(wordListDF, blogWordFull)
    writeLines("   Finished saving file")
    filesRemainingToAdd <- filesRemainingToAdd - 1
    thisEndTime <- proc.time()
    writeLines(paste("   This file took", round((thisEndTime-thisStartTime)[3]/60,2), "minutes"))
    timeToFinish <- round(((thisEndTime-thisStartTime)[3]*filesRemainingToAdd)/3600,2)
    writeLines(paste("   ", filesRemainingToAdd, "files remaining to add"))
    writeLines(paste("   Estimated time to finish:", timeToFinish, "hours"))
}

wordListDFSorted <- wordListDF[order(wordListDF[,"count"]),,drop=FALSE]
write.csv(wordListDFSorted, blogWordFullSorted)
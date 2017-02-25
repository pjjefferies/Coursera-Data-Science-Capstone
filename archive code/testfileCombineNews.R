#
#

newsWordFilesList <- list.files("C:/Users/Lynn/Data/tmp/Capstone",
                                pattern="^newsWordList[0-9]+.csv")
noWordFiles <- length(newsWordFilesList)
remainWordFiles <- noWordFiles
newsWordFull <- "newsWordListFull.csv"

if(file.exists(newsWordFull)) {
    wordListDF <- read.csv(newsWordFull)
    writeLines("Reading in existing newsWordListFull.csv file")
} else {
    wordListDF <- data.frame(count=as.integer())
    writeLines("Creating new newWordFull dataframe")
}


for(aFileName in newsWordFilesList) {
    thisStartTime <- proc.time()
    writeLines(paste("Reading file", aFileName))
    tempWordListDF <- read.csv(aFileName, row.names=1)
    writeLines(c("   File read", "   Adding words from file to list"))
    tempWordList <- row.names(tempWordListDF)
    for(aWordNo in 1:nrow(tempWordListDF)) {
        aWord <- tempWordList[aWordNo]
        ifelse(aWord %in% row.names(wordListDF),
               wordListDF[aWord, "count"] <- wordListDF[aWord, "count"] +
                   tempWordListDF[aWord, "count"],
               wordListDF[aWord, "count"] <- tempWordListDF[aWord, "count"])
    }
    writeLines(c("   Finished adding words.", "   Saving overall file"))
    write.csv(wordListDF, newsWordFull)
    writeLines("   Finished saving file")
    thisEndTime <- proc.time()
    thisIterTime <- thisEndTime - thisStartTime
    remainWordFiles <- remainWordFiles - 1
    remainingTime <- round(thisIterTime[3]/3600 * remainWordFiles,2)
    writeLines(c(paste("There are", remainWordFiles, "remaining word files"),
                 paste("This are", remainingTime, "hours remaining")))
}

wordListDFOrdered <- wordListDF[order(wordListDF[,"count"]),,drop=FALSE]
newsWordFullOrdered <- "newsWordListFullOrdered.csv"
write.csv(wordListDF, newsWordFullOrdered)
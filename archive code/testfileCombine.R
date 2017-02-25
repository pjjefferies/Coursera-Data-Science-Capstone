#
#

twitWordFilesList <- list.files("C:/Users/PaulJ/Data/Education/Online Courses/Coursera Data Science Capstone",
                                pattern="^twitWordList[0-9]+.csv")
noFilesToAdd <- length(twitWordFilesList)
filesRemainingToAdd <- noFilesToAdd
writeLines(paste("There are", noFilesToAdd, "files to add to complete list"))
twitWordFull <- "twitWordListFull.csv"

if(file.exists(twitWordFull)) {
    wordListDF <- read.csv(twitWordFull)
    writeLines("Read in existing twitWordListFull to start")
} else {
    wordListDF <- data.frame(count=as.integer())
    writeLines("Starting new twitWordListFull file")
}

for(aFileName in twitWordFilesList) {
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
    write.csv(wordListDF, twitWordFull)
    writeLines("   Finished saving file")
    filesRemainingToAdd <- filesRemainingToAdd - 1
    thisEndTime <- proc.time()
    writeLines(paste("This file took", round((thisEndTime-thisStartTime)[3]/60,2), "minutes"))
    timeToFinish <- round(((thisEndTime-thisStartTime)[3]*filesRemainingToAdd)/3600,2)
    writeLines(paste(filesRemainingToAdd, "files remaining to add"))
    writeLines(paste("Estimated time to finish:", timeToFinish, "hours"))
}

wordListDF <- wordListDF[order(wordListDF[,"count"]),,drop=FALSE]
write.csv(wordListDF, twitWordFull)
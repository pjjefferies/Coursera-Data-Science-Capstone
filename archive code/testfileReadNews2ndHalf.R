#
#
#
con <- file("en_US.news.txt", "r")
#newsFile <- readLines(con=con)
close(con)
writeLines("File read")

wordListDF <- data.frame(count=as.integer())

totalWords <- 0
savePrintLineCount <- 0
savePrintInc <- length(newsFile)/100

for(aLineNo in as.integer(length(newsFile)/2):length(newsFile)) {
    savePrintLineCount <- savePrintLineCount + 1
    if(savePrintLineCount >= savePrintInc) {
        writeLines(paste("Processing line", aLineNo))
        newsSaveFilename <- paste0("newsWordList", aLineNo, ".csv")
        write.csv(wordListDF, newsSaveFilename)
        wordListDF <- data.frame(count=as.integer())  #create new list for every X% of file and later combine them
        savePrintLineCount <- 0
    }

    thisLine <- newsFile[aLineNo]
    if(length(thisLine) == 0) next
    
    aLine <- strsplit(tolower(gsub("[^a-zA-Z \']", "", thisLine )), " ")[[1]]
    
    totalWords <- totalWords + length(aLine)
    
    #Capture line list of words for use in building Markov Matrix
    
    for(aWord in aLine) {
        if(is.na(aWord)) next
        if(aWord == "") next
        ifelse(aWord %in% row.names(wordListDF),
               wordListDF[aWord, "count"] <- wordListDF[aWord, "count"] + 1L,
               wordListDF[aWord, "count"] <- 1L)
    }
}

writeLines(paste("Total Words:", totalWords))
writeLines(paste("Total Unique Words:", nrow(wordListDF)))

endTime <- proc.time()
writeLines(paste("Elapsed time:", (endTime-startTime)[3]))

write.csv(wordListDF, "newsWordListFinal.csv")
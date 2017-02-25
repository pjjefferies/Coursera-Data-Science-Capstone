#
#
#
con <- file("en_US.blogs.txt", "r")
#blogFile <- readLines(con=con)
close(con)
writeLines("File read")

wordListDF <- data.frame(count=as.integer())

totalWords <- 0
savePrintLineCount <- 0
savePrintInc <- length(blogFile)/100

for(aLineNo in 1:length(blogFile)) {
    savePrintLineCount <- savePrintLineCount + 1
    if(savePrintLineCount >= savePrintInc) {
        writeLines(paste("Processing line", aLineNo))
        blogSaveFilename <- paste0("blogWordList", aLineNo, ".csv")
        write.csv(wordListDF, blogSaveFilename)
        wordListDF <- data.frame(count=as.integer())  #create new list for every X% of file and later combine them
        savePrintLineCount <- 0
    }

    thisLine <- blogFile[aLineNo]
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

write.csv(wordListDF, "blogWordListFinal.csv")
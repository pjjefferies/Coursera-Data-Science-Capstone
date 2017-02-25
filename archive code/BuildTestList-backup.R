#
#
#
#testlist format to return
#(Orig. String, string of words to a point, n-3 word, n-2 word, n-1 word, n/test word)

buildTestList <- function(anInputFilename,
                          noLinesToReadFromEach,
                          locationToReadLines,
                          trainLineNos,
                          testPercent) {
    testList <- data.frame(origLine = as.character(),
                           wordNoToTest = as.integer(),
                           #nMin4Word = as.character(),
                           nMin3Word = as.character(),
                           nMin2Word = as.character(),
                           nMin1Word = as.character(),
                           testWord  = as.character())

    set.seed(123456)
    
    testListLines <- c()
    totalLines <- as.integer(strsplit(system2("wc",
                                              args=c("-l", anInputFilename),
                                              stdout=TRUE),
                                      " ")[[1]][1])
    if(noLinesToReadFromEach <= 1) {  #if <= 1, interprete as a fraction of whole file
        noLinesToRead <- as.integer(noLinesToReadFromEach * totalLines)
    } else {
        noLinesToRead <- noLinesToReadFromEach
    }
    if(locationToReadLines == "top") {
        locText <- "from top"
        testLineNos <- trainLineNos
    } else {
        if(locationToReadLines == "random") {
            locText <- "randomly throughout file"
            trainLineNos <- sort(sample(totalLines,
                                        as.integer(noLinesToRead*trainPercent)))
        }
    }
    writeLines(paste("    Reading", noLinesToRead, "lines from file",
                     anInputFilename, locText))
    
    con <- file(anInputFilename, "r")
    if(locationToReadLines == "top") {
        linesFromInputFile <- readLines(con=con,
                                        n=noLinesToRead,
                                        skipNul=TRUE, warn=FALSE)
    } else {
        if(locationToReadLines == "random") {
            allLinesFromInputFile <- readLines(con=con,
                                               skipNul=TRUE, warn=FALSE)
            linesFromInputFile <- allLinesFromInputFile[trainLineNos]
            rm(allLinesFromInputFile)
        }
    }
    close(con)
    dataIntoCorpus <- append(dataIntoCorpus, linesFromInputFile)
}

    
    
    
    
    lineCountPrint <- max(as.integer(noLinesToread /5), 1)
    #lineCountPrint <- 1 #temp for debug
    lineNo <- 0
    writeLines(paste("  Reading", noLinesToread, "lines from file", anInputFilename))
    con <- file(anInputFilename, "r")
    linesFromInputFile <- readLines(con=con, n=noLinesToread)
    close(con)
    
    for(aLineNo in 1:length(linesFromInputFile)) {
        thisLine <- linesFromInputFile[aLineNo]
        if(length(thisLine) == 0) next
        if(aLineNo %in% trainLineNos) next
        
        aLineOfWords <- strsplit(tolower(gsub("[^a-zA-Z \']", "", thisLine )), " ")[[1]]
        
        if(length(aLineOfWords) < 2) {
            next
        }
        if(length(aLineOfWords) == 2) {
            wordPosToPredict <- 2
        } else {
            wordPosToPredict <- sample(seq(from=2, to=length(aLineOfWords), by=1),1)
        }
        if(wordPosToPredict >= 5) {
            nMin4Word <- aLineOfWords[wordPosToPredict-4]
        } else {
            nMin4Word <- NA
        }
        if(wordPosToPredict >= 4) {
            nMin3Word <- aLineOfWords[wordPosToPredict-3]
        } else {
            nMin3Word <- NA
        }
        if(wordPosToPredict >= 3) {
            nMin2Word <- aLineOfWords[wordPosToPredict-2]
        } else {
            nMin2Word <- NA
        }
        nMin1Word <- aLineOfWords[wordPosToPredict-1]
        testWord  <- aLineOfWords[wordPosToPredict]

        tempTestList <- data.frame(origLine = thisLine,
                                   wordNoToTest = wordPosToPredict,
                                   nMin4Word = nMin4Word,
                                   nMin3Word = nMin3Word,
                                   nMin2Word = nMin2Word,
                                   nMin1Word = nMin1Word,
                                   testWord  = testWord)
        testList <- rbind(testList, tempTestList)
        
    }
    writeLines(paste("Finished building testlist from file:", anInputFilename))
    #Return testList        
    return(testList)
}
#
#
#
#testlist format to return
#(Orig. String, string of words to a point, n-3 word, n-2 word, n-1 word, n/test word)

buildTestList <- function(anInputFilename, readLines=linesToReadFromEach,
                          trainLineNos = trainLineNos$trainSampNos) {
    testList <- data.frame(origLine = as.character(),
                           #partialLine = as.character(),
                           nMin4Word = as.character(),
                           nMin3Word = as.character(),
                           nMin2Word = as.character(),
                           nMin1Word = as.character(),
                           testWord  = as.character())

    con <- file(anInputFilename, "r")
    lineCountPrint <- max(as.integer(readLines /5), 1)
    #lineCountPrint <- 1 #temp for debug
    lineNo <- 0
    while ( TRUE ) {
        lineNo <- lineNo + 1
        if(((lineNo %% lineCountPrint) == 0) & readLines > 20) {
            writeLines(paste("   Processing line", lineNo, "of", readLines))
        }
        aLine <- readLines(con, n = 1)
        #print(paste("1:", aLine[1], lineNo, readLines))
        if (length(aLine) == 0 | lineNo > readLines) { #stop if at end of file or have read enough lines
            break
        }
        if(lineNo %in% trainLineNos) next   #skip line if for training
        aLineOfWords <- strsplit(tolower(gsub("[^a-zA-Z \']", "", aLine )), " ")[[1]]
        #print(paste("2:", aLine, lineNo, readLines))
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
        #partialLine <- aLineOfWords[1:wordPosToPredict-1]
        #print(partialLine)
        #print(aLineOfWords)
        #print(length(aLineOfWords))
        #print(wordPosToPredict)
        #print(c(nMin4Word, nMin3Word, nMin2Word, nMin1Word, testWord))
        #print(aLineOfWords[wordPosToPredict])
        tempTestList <- data.frame(origLine = aLine,
                                   #partialLine = partialLine,
                                   nMin4Word = nMin4Word,
                                   nMin3Word = nMin3Word,
                                   nMin2Word = nMin2Word,
                                   nMin1Word = nMin1Word,
                                   testWord  = testWord)
        testList <- rbind(testList, tempTestList)
        #print(testList[nrow(testList),"partialLine"])
    }
    close(con)
    writeLine(paste("Finished building testlist from file:", anInputFilename))
    #Return testList        
    testList
}
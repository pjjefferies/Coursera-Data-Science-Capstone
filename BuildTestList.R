#
#
#
#testlist format to return
#(Orig. String, string of words to a point, n-3 word, n-2 word, n-1 word, n/test word)

buildTestList <- function(anInputFilename,
                          noLinesToReadFromEach,
                          minTotalLines,
                          locationToReadLines,
                          trainLineNos,
                          testLineNos,
                          testPercent) {
    
    
    set.seed(123456)
    library(tm)
    library(SnowballC)
    source("cleanCorpus.R")
    
    ### GENERATE LIST OF LINE NUMBERS TO TEST THAT ARE DISTICT FROM TRAINING LINES
    
    if(noLinesToReadFromEach <= 1) {  #if <= 1, interprete as a fraction of whole file
        noLinesToRead <- as.integer(noLinesToReadFromEach * minTotalLines)
    } else {
        noLinesToRead <- noLinesToReadFromEach
    }
    
    if(locationToReadLines == "top") {  #iously created for another file
        locText <- "from top"
    } else {
        if(locationToReadLines == "random") {
            locText <- "randomly throughout file"
        } else {
            writeLines(paste("    Location To Read Lines not valid:", locationToReadLines))
            return(list(list(), list()))
        }
    }
    
    if(length(testLineNos) == 0) {           #don't create testLineNos if prev-
        if(locationToReadLines == "top") {  #iously created for another file
            locText <- "from top"
            testLineNos <- c()
            for(aLineNo in 1:noLinesToReadFromEach) {
                if(!(aLineNo %in% trainLineNos)) {
                    testLineNos <- append(testLineNos, aLineNo)
                }
            }
            testLineNos <- sort(testLineNos)
        } else {
            if(locationToReadLines == "random") {
                locText <- "randomly throughout file"
                testLineNos <- c()
                testLineCount <- 0
                testLinesRequired <- noLinesToReadFromEach*testPercent
                while(testLineCount < testLinesRequired) {
                    aNum <- sample(minTotalLines, 1)
                    if(!(aNum %in% trainLineNos)) {
                        testLineNos <- append(testLineNos, aNum)
                        testLineCount <- testLineCount + 1
                    }
                }
                testLineNos <- sort(testLineNos)
            }
            else {
                writeLines(paste("    Location To Read Lines not valid:", locationToReadLines))
                return(list(list(), list()))
            }
        }
    }
    if(length(testLineNos) == 0) {
        writeLines("    No test lines found. Aborting.")
        return(list(), list())
    }
    
    ### BASED ON LINE NUMBERS ABOVE, READ LINES TO TEST
    
    writeLines(paste("    Reading", noLinesToRead, "lines from file",
                     anInputFilename, locText))
    
    con <- file(anInputFilename, "r")
    if(locationToReadLines == "top") {
        linesFromInputFile <- readLines(con=con,
                                        n=noLinesToRead,
                                        skipNul=TRUE,
                                        warn=FALSE)
        linesFromInputFile <- linesFromInputFile[testLineNos]
    } else {
        if(locationToReadLines == "random") {
            allLinesFromInputFile <- readLines(con=con,
                                               n=minTotalLines,
                                               skipNul=TRUE,
                                               warn=FALSE)
            linesFromInputFile <- allLinesFromInputFile[testLineNos]
            rm(allLinesFromInputFile)
        }
    }
    close(con)
    writeLines(paste("    finished reading lines from files. Starting processing."))
    
    testCorpus <- Corpus(VectorSource(c(linesFromInputFile)))
    testCorpus <- CleanCorpus(testCorpus,
                              removeEmail=TRUE,
                              removeURL=TRUE,
                              removeHandles=TRUE,
                              removeHashtags=TRUE,
                              removeStopWords=FALSE, #Leave this as an option when predicting
                              appSpecWordsFile=FALSE,
                              removeWordSuffixes=FALSE, #Leave this as an option when predicting
                              myBadWordsFile="myTermsToBlock.csv", #Leave for now, sigh.
                              convertPlainText=TRUE)

    testList <- data.frame(origLine = as.character(),
                           wordsToPredictBy = as.character(),
                           wordNoToTest = as.integer(),
                           #nMin4Word = as.character(),
                           #nMin3Word = as.character(),
                           #nMin2Word = as.character(),
                           #nMin1Word = as.character(),
                           testWord  = as.character())
    
    writeLines(paste("    finished creating clean corpus. Generating test word position."))
    #lineCountPrint <- max(as.integer(noLinesToread /5), 1)
    #lineCountPrint <- 1 #temp for debug
    lineNo <- 0
        
    for(aLineNo in 1:length(linesFromInputFile)) {
        thisLine <- linesFromInputFile[aLineNo]
        if(length(thisLine) == 0) next
        thisLine <- trimws(testCorpus[[aLineNo]]$content)
        aLineOfWords <- strsplit(thisLine, " ")[[1]]
        
        if(length(aLineOfWords) < 2) { #can't predict with only one word on line
            next
        }
        if(length(aLineOfWords) == 2) {
            wordPosToPredict <- 2    #Use one word to predict word #2
        } else {
            wordPosToPredict <- sample(seq(from=2, to=length(aLineOfWords), by=1),1)
        }
        
        testWord  <- aLineOfWords[wordPosToPredict]
        wordsToPredictBy <- paste(aLineOfWords[1:wordPosToPredict-1], collapse=" ")
        
        
        #if(wordPosToPredict >= 5) {
        #    nMin4Word <- aLineOfWords[wordPosToPredict-4]
        #} else {
        #    nMin4Word <- NA
        #}
        
        # if(wordPosToPredict >= 4) {
        #     nMin3Word <- aLineOfWords[wordPosToPredict-3]
        # } else {
        #     nMin3Word <- NA
        # }
        # if(wordPosToPredict >= 3) {
        #     nMin2Word <- aLineOfWords[wordPosToPredict-2]
        # } else {
        #     nMin2Word <- NA
        # }
        # 
        # nMin1Word <- aLineOfWords[wordPosToPredict-1]
        # testWord  <- aLineOfWords[wordPosToPredict]
        
        tempTestList <- data.frame(origLine = linesFromInputFile[aLineNo],
                                   wordsToPredictBy = wordsToPredictBy,
                                   wordNoToTest = wordPosToPredict,
                                   #nMin4Word = nMin4Word,
                                   #nMin3Word = nMin3Word,
                                   #nMin2Word = nMin2Word,
                                   #nMin1Word = nMin1Word,
                                   testWord  = testWord)
        testList <- rbind(testList, tempTestList)
        
    }
    writeLines(paste("    Finished building testlist from file:", anInputFilename))
    return(list(testList, testLineNos))
    }
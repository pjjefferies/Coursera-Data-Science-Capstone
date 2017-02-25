#
#Function to Read files line by line to create Markov Chain Word Matrix
#

library(Matrix)
library(tm)
library(ggplot2)
library(SnowballC)

buildMarkovChainWordSpMatrix = function(inputDataFilenames,
                                        noLinesToReadFromEach,
                                        trainSkipPenalty,
                                        locationToReadLines,
                                        trainPercent,
                                        #trainLineNos,
                                        maxCumFreq#,
                                        #aRunDataMCSpFilename,
                                        #predictorWordListFilename,
                                        #predictedWordListFilename
                                        ) {
    trainSkipPenalty=as.integer(trainSkipPenalty)
    #maxSkip = 2L
    #wordListDF <- data.frame(count=as.integer())
    #linesFromInputFilesWordList <- list()
    dataIntoCorpus <- c()
    source("CleanCorpus.R")
    source("CreateNGrams.R")
    
    for(anInputFilename in inputDataFilenames) {
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
            trainLineNos <- sort(sample(noLinesToRead,
                                        as.integer(noLinesToRead*trainPercent)))
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
    wordPredictData <- Corpus(VectorSource(dataIntoCorpus))
    
    #Memory Clean-up time
    rm(dataIntoCorpus, linesFromInputFile)
    
    wordPredictData <- CleanCorpus(wordPredictData,
                                   removeEmail=TRUE,
                                   removeURL=TRUE,
                                   removeHandles=TRUE,
                                   removeHashtags=TRUE,
                                   removeStopWords=TRUE, #ok for 2, 3, 4-grams?
                                   appSpecWordsFile=FALSE,
                                   removeWordSuffixes=TRUE,  #TDM fails with FALSE ???
                                   myBadWordsFile=FALSE,
                                   #myBadWordsFile="myTermsToBlock.csv",
                                   convertPlainText=TRUE)
    
    #Not sure if this needs to be used. Perhaps for exploratory analysis only
    #dtm <- DocumentTermMatrix(wordPredictData)
    #freq <- colSums(as.matrix(dtm))
    #ord <- order(freq, decreasing=TRUE)
    #freq <- freq[order(freq, decreasing=TRUE)]
    
    #Create N-Grams - they come back in ordered from highest to lowest freq.
    #wordList <- unigram(wordPredictData)

    #Initial try at reducing n-grams. Put as options in runQueue later
    #wordList <- wordList[grep("[â€]",wordList$ngrams, value=FALSE, invert=TRUE),
    #                     , drop=FALSE]
    #wordList <- wordList[wordList$freq > 1, "freq", drop=FALSE]

    writeLines("    Creating 2-Grams")
    biGrams <- bigram(wordPredictData)
    biGrams <- biGrams[grep("[â€]",biGrams$ngrams, value=FALSE, invert=TRUE),
                       , drop=FALSE]
    #biGrams <- biGrams[biGrams$freq > 1, , drop=FALSE] #elim for not to change to powe based
    if(nrow(biGrams) != 0) {
        biGrams$ngrams <- as.character(biGrams$ngrams)
        biGramsNames <- biGrams$ngrams
        for(aBiGramNo in 1:nrow(biGrams)) {
            thisNGram <- strsplit(biGramsNames[aBiGramNo], " ")[[1]]
            if(nchar(thisNGram[2]) < 2) next
            biGrams[aBiGramNo, "predictor"] <- thisNGram[1]
            biGrams[aBiGramNo, "predicted"] <- thisNGram[2]
        }
        biGrams <- biGrams[!is.na(biGrams$predicted), , drop=FALSE]
        rm(biGramsNames)
    }
    
    
    writeLines("    Creating 3-Grams")
    triGrams <- trigram(wordPredictData)
    triGrams <- triGrams[grep("[â€]",triGrams$ngrams, value=FALSE, invert=TRUE),
                       , drop=FALSE]
    triGrams <- triGrams[triGrams$freq > 1, , drop=FALSE]
    if(nrow(triGrams) != 0) {
        triGrams$ngrams <- as.character(triGrams$ngrams)
        triGramsNames <- triGrams$ngrams
        for(aTriGramNo in 1:nrow(triGrams)) {
            thisNGram <- strsplit(triGramsNames[aTriGramNo], " ")[[1]]
            if(nchar(thisNGram[3]) < 2) next
            triGrams[aTriGramNo, "predictor"] <- paste0(thisNGram[1],"+",thisNGram[2])
            triGrams[aTriGramNo, "predicted"] <- thisNGram[3]
        }
        triGrams <- triGrams[!is.na(triGrams$predicted), , drop=FALSE]
        rm(triGramsNames)
    }
    

    writeLines("    Creating 4-Grams")
    quadGrams <- quadgram(wordPredictData)
    quadGrams <- quadGrams[grep("[â€]",quadGrams$ngrams, value=FALSE, invert=TRUE),
                           , drop=FALSE]
    quadGrams <- quadGrams[quadGrams$freq > 1, , drop=FALSE]
    if(nrow(quadGrams) != 0) {
        quadGrams$ngrams <- as.character(quadGrams$ngrams)
        quadGramsNames <- quadGrams$ngrams
        for(aQuadGramNo in 1:nrow(quadGrams)) {
            thisNGram <- strsplit(quadGramsNames[aQuadGramNo], " ")[[1]]
            if(nchar(thisNGram[4]) < 2) next
            quadGrams[aQuadGramNo, "predictor"] <- paste0(thisNGram[1],"+",
                                                          thisNGram[2],"+",
                                                          thisNGram[3])
            quadGrams[aQuadGramNo, "predicted"] <- thisNGram[4]
        }
        quadGrams <- quadGrams[!is.na(quadGrams$predicted), , drop=FALSE]
        rm(quadGramsNames)
    }
        
    #Memory Clean-up time
    rm(wordPredictData)
    writeLines("    Done creating n-Grams")
    
    #Before creating Markov Matrix, remove words from wordlist above specified cum freq
    #wordListDF <- wordListDF[order(wordListDF$count, decreasing = TRUE), , drop=FALSE]
    #sumWordListCount <- sum(wordListDF$count)
    #wordListDF$freq <- wordListDF$count / sumWordListCount
    #wordListDF[1,"cumFreq"] <- wordListDF[1, "freq"]
    #for(aWordNo in 2:nrow(wordListDF)) {
    #  wordListDF[aWordNo, "cumFreq"] <- wordListDF[aWordNo-1, "cumFreq"] +
    #                                    wordListDF[aWordNo, "freq"]
    #}
    #wordListDF <- wordListDF[wordListDF$cumFreq <= maxCumFreq,]
    #noWords <- nrow(wordListDF)
    noNGrams <- nrow(biGrams) + nrow(triGrams) + nrow(quadGrams)

    #Create Markov Matrix - skip words for predictor or prediected if not in wordList?
    #
    #   PREDICTORS ARE IN ROWS
    #
    #   PREDICTEES ARE IN COLUMNS
    #
    #writeLines(paste("  Finished cleaning-up words list. Start creating Markov Matrix for list of",
    #                 length(linesFromInputFilesWordList), "lines."))
    #mCWordSpMatrix <- Matrix(data=0, nrow=noNGrams, ncol=noNGrams,
    #                         sparse=TRUE, doDiag=FALSE)
    
    #writeLines(paste("Lengths", nrow(biGrams), nrow(triGrams), nrow(quadGrams)))
    
    nGramsToAdd <-  biGrams
    if(nrow(triGrams) > 0) {
        nGramsToAdd <- rbind(nGramsToAdd, triGrams)
        writeLines("    2-Grams and 3-Grams added successfully")
    }
    
    if(nrow(quadGrams) > 0) {
        nGramsToAdd <- rbind(nGramsToAdd, quadGrams)
        writeLines("    4-Grams added successfully")
    }
    
    writeLines(c("    Finished cleaning-up words list",
                 "    Start creating Markov Matrix"))
    predictorWordList <- unique(nGramsToAdd$predictor)
    predictedWordList <- unique(nGramsToAdd$predicted)
    mCWordSpMatrix <- Matrix(data=0,
                             nrow=length(predictorWordList),
                             ncol=length(predictedWordList),
                             sparse=TRUE, doDiag=FALSE)
    
    nGramsToProcess <- nrow(nGramsToAdd)
    lineCountPrint <- max(as.integer(nGramsToProcess /5),1)
    lineNo <- 0
    
    for(anNGramNo in 1:nGramsToProcess) {
        lineNo <- lineNo + 1
        if(lineNo %% lineCountPrint == 0) {
            writeLines(paste("      Processing line", lineNo, "of", nGramsToProcess))
        }
        predictorWordNo <- match(nGramsToAdd[anNGramNo, "predictor"],
                                 predictorWordList, nomatch=-1)
        predictedWordNo <- match(nGramsToAdd[anNGramNo, "predicted"],
                                 predictedWordList, nomatch=-1)
        mCWordSpMatrix[predictorWordNo, predictedWordNo] <- 
            nGramsToAdd[anNGramNo, "freq"]
    }
    
    writeLines(paste("    Finished creating Markov Matrix"))
    
    #Return the data for size analysis
    return(list(mCWordSpMatrix, predictorWordList, predictedWordList, trainLineNos))
}
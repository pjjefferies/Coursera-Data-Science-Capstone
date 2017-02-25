#
#Function to Read files line by line to create Markov Chain Word Matrix
#

buildMarkovChainWordMatrix = function(inputDataFilenames,
                                      linesToRead,
                                      skipPenalty,
                                      trainLineNos,
                                      maxCumFreq) {
    mCWordDF <- data.frame()
    skipPenalty=as.integer(skipPenalty)
    maxSkip = 2L
    wordListDF <- data.frame(count=as.integer())
    linesFromInputFilesWordList <- list()
    
    for(anInputFilename in inputDataFilenames) {
        writeLines(paste("  Reading", linesToRead, "lines from file", anInputFilename))
        con <- file(anInputFilename, "r")
        #lineCountPrint <- max(as.integer(readLines /5),1)
        #lineNo <- 0
        #while ( TRUE ) {
        linesFromInputFile <- readLines(con=con, n=linesToRead)
        #writeLines(linesFromInputFile)
        close(con)
        #    lineNo <- lineNo + 1
        #    if(((lineNo %% lineCountPrint) & readLines > 20) == 0) {
        #        writeLines(paste("   Processing line", lineNo, "of", readLines))
        #    }
        #    aLine <- readLines(con, n = 1)
        
        #First make a word list to filter by maxCumFreq to minimize size of Markov Matrix up-front
        #Save word list dataframe for each line for use in building markov chain
        for(aLineNo in 1:length(linesFromInputFile)) {
            #print(paste("1:", aLine[1], lineNo, readLines))
            thisLine <- linesFromInputFile[aLineNo]
            #writeLines(paste("thisLine:", thisLine))
            if (length(thisLine) == 0) { 
                next
            }
            if(!(aLineNo %in% trainLineNos)) next   #skip line if not for training
            aLine <- strsplit(tolower(gsub("[^a-zA-Z \']", "", thisLine )), " ")[[1]]
            #print(paste("2:", aLine, aLineNo))
            #Capture line list of words for use in building Markov Matrix
            #writeLines(paste0("0: ", aLine))
            linesFromInputFilesWordList[length(linesFromInputFilesWordList)+1] <- list(aLine)
            #writeLines(paste0("3: ", 
            #                  linesFromInputFilesWordList[[length(linesFromInputFilesWordList)]]))
            #writeLines(paste(linesFromInputFilesWordList))
            
            #Keep tracks of words seen also for prioritizing most seen words to trade-off db size
            #wordListDF
            for(aWord in aLine) {
                #print(paste("Should be adding:", aWord))
                #print(paste("It was:", aWord, wordList[aWord, "count"]))
                if(is.na(aWord)) next
                if(aWord == "") next
                ifelse(aWord %in% row.names(wordListDF),
                       wordListDF[aWord, "count"] <- wordListDF[aWord, "count"] + 1L,
                       wordListDF[aWord, "count"] <- 1L)
                #print(paste("It  is:", aWord, wordList[aWord, "count"]))
            }
        }
    }
    writeLines(paste("   Finished reading files and creating word list. Clean-up words list first."))
    #Before creating Markov Matrix, remove words from wordlist above specified cum freq
    wordListDF < wordListDF[order(wordListDF$count,
                                  decreasing = TRUE),]
    sumWordListCount <- sum(wordListDF$count)
    wordListDF$freq <- wordListDF$count / sumWordListCount
    wordListDF[1,"cumFreq"] <- wordListDF[1, "freq"]
    for(aWordNo in 2:nrow(wordListDF)) {
      wordListDF[aWordNo, "cumFreq"] <- wordListDF[aWordNo-1, "cumFreq"] +
                                        wordListDF[aWordNo, "freq"]
    }
    wordListDF <- wordListDF[wordListDF$cumFreq <= maxCumFreq,]

    #Create Markov Matrix - skip words for predictor or prediected if not in wordListDF
    writeLines(paste("   Finished cleaning-up words list. Start creating Markov Matrix for list of",
                     length(linesFromInputFilesWordList), "lines."))
    linesToProcess <- length(linesFromInputFilesWordList)
    lineCountPrint <- max(as.integer(linesToProcess /5),1)
    lineNo <- 0
    wordsToUse <- row.names(wordListDF)
    
    for(aLineListNo in 1:linesToProcess) {
        lineNo <- lineNo + 1
        if((lineNo %% lineCountPrint == 0) & linesToProcess > 20) {
          writeLines(paste("   Processing line", lineNo, "of", linesToProcess))
        }
        thisLineWordList <- linesFromInputFilesWordList[[aLineListNo]]
        for(aWordNo in 1:(length(thisLineWordList)-1)) {
            if(length(thisLineWordList) < 2) next
            word1 <- thisLineWordList[aWordNo]     #predictor
            word2 <- thisLineWordList[aWordNo+1]   #predicted
            if(!(word1 %in% wordsToUse & word2 %in% wordsToUse)) next
            #print(paste("3: aWordNo:", aWordNo, ", word1:", word1, ", word2:", word2))
            if(is.na(word1) | is.na(word2)) next
            if(word1 == "" | word2 == "") next
            if(word1 %in% row.names(mCWordDF)) {
              if(word2 %in% names(mCWordDF)) {
                mCWordDF[word1, word2] <- mCWordDF[word1, word2] +
                                                 1L * skipPenalty**maxSkip
              } else {
                mCWordDF[word1, word2] <- 1L * skipPenalty**maxSkip
              }
            } else {
              mCWordDF[word1, word2] <- 1L * skipPenalty**maxSkip
            }
        }

        for(aWordNo in 1:(length(thisLineWordList)-2)) {
            if(length(thisLineWordList) < 3) next
            word1 <- thisLineWordList[aWordNo]      #predictor
            word3 <- thisLineWordList[aWordNo+2]    #predicted
            #print(paste("4: aWordNo:", aWordNo, ", word1:", word1, ", word2:", word2,
            #            ", word3:", word3))
            if(is.na(word1) | is.na(word3)) next
            if(word1 == "" | word3 == "") next
            if(word1 %in% row.names(mCWordDF)) {
              if(word3 %in% names(mCWordDF)) {
                mCWordDF[word1, word3] <- mCWordDF[word1, word3] +
                                          1L * skipPenalty**(maxSkip-1)
              } else {
                mCWordDF[word1, word3] <- 1L * skipPenalty**(maxSkip-1)
              }
            } else {
              mCWordDF[word1, word3] <- 1L * skipPenalty**(maxSkip-1)
            }
        }
        
        for(aWordNo in 1:(length(thisLineWordList)-3)) {
            if(length(thisLineWordList) < 4) next
            word1 <- thisLineWordList[aWordNo]      #predictor
            word4 <- thisLineWordList[aWordNo+3]    #predicted
            #print(paste("4: aWordNo:", aWordNo, ", word1:", word1, ", word2:", word2,
            #            ", word3:", word3, ", word4:", word4))
            if(is.na(word1) | is.na(word4)) next
            if(word1 == "" | word4 == "") next
            if(word1 %in% row.names(mCWordDF)) {
              if(word4 %in% names(mCWordDF)) {
                mCWordDF[word1, word4] <- mCWordDF[word1, word4] +
                                          1L * skipPenalty**(maxSkip-2)
              } else {
                mCWordDF[word1, word4] <- 1L * skipPenalty**(maxSkip-2)
              }
            } else {
              mCWordDF[word1, word4] <- 1L * skipPenalty**(maxSkip-2)
            }
        }

    }
    
    mCWordMatrix <- as.matrix(mCWordDF)
    mCWordMatrix[is.na(mCWordMatrix)] <- 1L
    storage.mode(mCWordMatrix) <- "integer"
    
    return(mCWordMatrix)
}
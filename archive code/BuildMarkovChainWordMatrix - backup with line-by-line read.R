#
#Function to Read files line by line to create Markov Chain Word Matrix
#

buildMarkovChainWordMatrix = function(inputDataFilenames,
                                      readLines=linesToReadFromEach,
                                      skipPenalty=skipPenalty,
                                      trainLineNos = trainLineNos) {
    mCWordDF <- data.frame()
    skipPenalty=as.integer(skipPenalty)
    maxSkip = 2L
    wordListDF <- data.frame(count=as.integer())
    
    for(anInputFilename in inputDataFilenames) {
        writeLines(paste("Reading", readLines, "lines from file", anInputFilename))
        con <- file(anInputFilename, "r")
        lineCountPrint <- max(as.integer(readLines /5),1)
        lineNo <- 0
        while ( TRUE ) {
            lineNo <- lineNo + 1
            if(((lineNo %% lineCountPrint) & readLines > 20) == 0) {
                writeLines(paste("   Processing line", lineNo, "of", readLines))
            }
            aLine <- readLines(con, n = 1)
            #print(paste("1:", aLine[1], lineNo, readLines))
            if (length(aLine) == 0 | lineNo > readLines) { #stop if at end of file or have read enough lines
                break
            }
            if(!(lineNo %in% trainLineNos)) next   #skip line if not for training
            aLine <- strsplit(tolower(gsub("[^a-zA-Z \']", "", aLine )), " ")[[1]]
            #print(paste("2:", aLine, lineNo, readLines))
            
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
            
            for(aWordNo in 1:(length(aLine)-1)) {
                if(length(aLine) < 2) next
                word1 <- aLine[aWordNo]     #predictor
                word2 <- aLine[aWordNo+1]   #predicted
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

            for(aWordNo in 1:(length(aLine)-2)) {
                if(length(aLine) < 3) next
                word1 <- aLine[aWordNo]      #predictor
                word3 <- aLine[aWordNo+2]    #predicted
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
            
            for(aWordNo in 1:(length(aLine)-3)) {
                if(length(aLine) < 4) next
                word1 <- aLine[aWordNo]      #predictor
                word4 <- aLine[aWordNo+3]    #predicted
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
        close(con)
    }
    wordListDF$word <- row.names(wordListDF)
    list(mCWordDF, wordListDF)
}
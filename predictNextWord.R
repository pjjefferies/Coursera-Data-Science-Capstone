#
#
#
library(tm)
#library(combinat)
source("CleanCorpus.R")
source("AddToPredictionDF.R")

predictNextWord <- function(wordsToPredictBy,
                            mCWordSpMatrix,
                            predictorWordDF,
                            predictedWordDF,
                            noWordsToReturn = 1,
                            skipPenalty = 2,
                            removeStopWords=TRUE,
                            removeWordSuffixes=TRUE) {
    #writeLines(paste0("pNW 1.0: ", newWordList))
    
    # shortList <- data.frame(count=as.integer(),
    #                         basis=as.character(),
    #                         prediction=as.character(),
    #                         word=as.character(),
    #                         rowCount=as.integer(),
    #                         freq=as.numeric(),
    #                         cumFreq=as.numeric())

    #print(newWordList)
    
    wordsToPredictBy <- as.character(wordsToPredictBy)
    
    aShortCorpus <- Corpus(VectorSource(c(wordsToPredictBy)))
    
    aShortCleanCorpus <- CleanCorpus(aShortCorpus,
                                     removeEmail=TRUE,
                                     removeURL=TRUE,
                                     removeHandles=TRUE,
                                     removeHashtags=TRUE,
                                     removeStopWords=removeStopWords,
                                     appSpecWordsFile=FALSE,
                                     removeWordSuffixes=removeWordSuffixes,
                                     myBadWordsFile=FALSE,
                                     convertPlainText=TRUE)
    
    textOutOfCorpus <- aShortCleanCorpus[[1]]$content
    
    #writeLines("pNW 1.1: newWordDF:")
    #print(newWordDF)
    #writeLines("end")
    
    aLineOfWords <- stripWhitespace(trimws(strsplit(textOutOfCorpus, " ")[[1]]))
    aLOWLen <- length(aLineOfWords)
    #Take the last 6 words at most
    aLineOfWords <- aLineOfWords[max(aLOWLen-min(5,aLOWLen), 1):aLOWLen]
    aLOWLen <- length(aLineOfWords)
    newWordDF <- data.frame(word=aLineOfWords,
                            stringsAsFactors=FALSE)
    
    predictionDF <- data.frame(word=as.character(),
                               power=as.numeric(),
                               sourceAlgo=as.character(),
                               stringsAsFactors = FALSE)
    
    
    ### PREDICTION - PRIORITY 1 - 4-grams, 3-grams, 2-grams with words in-order together

    # Find 2-Gram Matches
    if(aLOWLen >= 1) {
        predictorWords <- newWordDF[aLOWLen, "word", drop=TRUE]
        if(predictorWords %in% predictorWordDF$word) { #Found a 2-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="2"))
        }
    } else {
        return(data.frame(word=c(FALSE), stringsAsFactors = FALSE))
    }
    
    # Second find 3-Gram; 2-Gram, Skip-1 Matches
    if(aLOWLen >= 2) {
        #3-Grams
        predictorWords <- paste(newWordDF[(aLOWLen-1):aLOWLen, "word",
                                          drop=TRUE],
                                collapse="+")
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3/2-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="3"))
        }
        #2-Grams, Skip-1
        predictorWords <- newWordDF[(aLOWLen-1), "word", drop=TRUE]
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3/2-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=1,
                                                    sourceAlgo="21"))
        }
    } else {
        if(nrow(predictionDF) > 0) {
            predictionDF <- predictionDF[order(predictionDF$power,
                                               decreasing = TRUE),,drop=FALSE]
            predictionDF <- predictionDF[1:min(noWordsToReturn,
                                               nrow(predictionDF)),,drop=FALSE]
            return(predictionDF)
        } else {
            return(data.frame(word=c(FALSE), stringsAsFactors = FALSE))
        }
    }
    
    
    # First find 4-Grams; 3-Grams, Skip-1 Matches
    if(aLOWLen >= 3) {
        #4-Grams
        predictorWords <-
            paste(newWordDF[(aLOWLen-2):aLOWLen, "word", drop=TRUE],
                  collapse="+")
        if(predictorWords %in% predictorWordDF$word) { #Found a 4-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=4,
                                                    sourceAlgo="4"))
        }
        #3-Grams, Skip-1A
        predictorWords <- paste0(newWordDF[(aLOWLen-2), "word", drop=TRUE],"+",
            newWordDF[aLOWLen, "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="31A"))
        }
        #3-Grams, Skip-1B
        predictorWords <- paste0(newWordDF[(aLOWLen-2), "word", drop=TRUE], "+",
            newWordDF[(aLOWLen-1), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="31B"))
        }
    } else {
        if(nrow(predictionDF) > 0) {
            predictionDF <- predictionDF[order(predictionDF$power,
                                               decreasing = TRUE),,drop=FALSE]
            predictionDF <- predictionDF[1:min(noWordsToReturn,
                                               nrow(predictionDF)),,drop=FALSE]
            return(predictionDF)
        } else {
            return(data.frame(word=c(FALSE), stringsAsFactors = FALSE))
        }
    }
        
    
    
    # First find 5-Gram; 4-Gram, Skip-1; 3-Gram, Skip-2 Matches
    if(aLOWLen >= 4) {
        #5-Grams
        predictorWords <- 
            paste(newWordDF[(aLOWLen-3):aLOWLen, "word", drop=TRUE],
                  collapse="+")
        if(predictorWords %in% predictorWordDF$word) { #Found a 5-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=5,
                                                    sourceAlgo="5"))
        }
        #4-Grams, Skip-1A
        predictorWords <- paste0(newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 paste(newWordDF[(aLOWLen-1):aLOWLen, "word",
                                                 drop=TRUE],
                                       collapse="+"))
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="41A"))
        }
        #4-Grams, Skip-1B
        predictorWords <- paste0(paste(newWordDF[(aLOWLen-3):(aLOWLen-2),
                                                 "word", drop=TRUE],
                                       collapse="+"), "+",
                                 newWordDF[aLOWLen, "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="41B"))
        }
        #4-Grams, Skip-1C
        predictorWords <- paste(newWordDF[(aLOWLen-3):(aLOWLen-1), "word",
                                          drop=TRUE], collapse="+")
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="41C"))
        }
        #3-Grams, Skip-2A
        predictorWords <- paste0(newWordDF[aLOWLen-3, "word", drop=TRUE], "+",
                                 newWordDF[aLOWLen, "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=1,
                                                    sourceAlgo="32A"))
        }
        #3-Grams, Skip-2B
        predictorWords <- paste0(newWordDF[aLOWLen-3, "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=1,
                                                    sourceAlgo="32B"))
        }
        #3-Grams, Skip-2C
        predictorWords <- paste0(newWordDF[aLOWLen-3, "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=1,
                                                    sourceAlgo="32C"))
        }
    } else {
        if(nrow(predictionDF) > 0) {
            #print(predictionDF)
            predictionDF <- predictionDF[order(predictionDF$power,
                                               decreasing = TRUE),,drop=FALSE]
            predictionDF <- predictionDF[1:min(noWordsToReturn,
                                               nrow(predictionDF)),,drop=FALSE]
            return(predictionDF)
        } else {
            return(data.frame(word=c(FALSE), stringsAsFactors = FALSE))
        }
    }
    

    # First find 5-Gram, Skip-1; 4-Gram, Skip-2 Matches
    if(aLOWLen >= 5) {
        #5-Grams, Skip-1A
        predictorWords <- paste0(newWordDF[aLOWLen-4, "word", drop=TRUE], "+",
                                 paste(newWordDF[(aLOWLen-2):aLOWLen, "word",
                                                 drop=TRUE],
                                       collapse="+"))
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=4,
                                                    sourceAlgo="51A"))
        }
        #5-Grams, Skip-1B
        predictorWords <- paste0(paste(newWordDF[(aLOWLen-4):(aLOWLen-3),
                                                 "word", drop=TRUE],
                                       collapse="+"), "+", 
                                 paste(newWordDF[(aLOWLen-1):aLOWLen,
                                                 "word", drop=TRUE],
                                       collapse="+"))
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=4,
                                                    sourceAlgo="51B"))
        }
        #5-Grams, Skip-1C
        predictorWords <- paste0(newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE], "+",
                                 newWordDF[aLOWLen, "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a 5-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=4,
                                                    sourceAlgo="51C"))
        }
        #5-Grams, Skip-1D
        predictorWords <- paste0(newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 paste(newWordDF[(aLOWLen-2):(aLOWLen-1),
                                                 "word", drop=TRUE],
                                       collapse="+"))
        if(predictorWords %in% predictorWordDF$word) { #Found a 5-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=4,
                                                    sourceAlgo="51D"))
        }
        #4-Grams, Skip-2A
        predictorWords <- paste0(newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 paste(newWordDF[(aLOWLen-1):aLOWLen, "word",
                                                 drop=TRUE],
                                       collapse="+"))
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="42A"))
        }
        #4-Grams, Skip-2B
        predictorWords <- paste0(newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE], "+",
                                 newWordDF[aLOWLen, "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="42B"))
        }
        #4-Grams, Skip-2C
        predictorWords <- paste0(newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE],"+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="42C"))
        }
        #4-Grams, Skip-2D
        predictorWords <- paste0(newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[aLOWLen, "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="42D"))
        }
        #4-Grams, Skip-2E
        predictorWords <- paste0(newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="42E"))
        }
        #4-Grams, Skip-2F
        predictorWords <- paste0(newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=2,
                                                    sourceAlgo="42F"))
        }
    } else {
        if(nrow(predictionDF) > 0) {
            predictionDF <- predictionDF[order(predictionDF$power,
                                               decreasing = TRUE),,drop=FALSE]
            predictionDF <- predictionDF[1:min(noWordsToReturn,
                                               nrow(predictionDF)),,drop=FALSE]
            return(predictionDF)
        } else {
            return(data.frame(word=c(FALSE), stringsAsFactors = FALSE))
        }
    }
    
    # First find 5-Gram, Skip-2 Matches
    if(aLOWLen >= 6) {
        #5-Grams, Skip-2A
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-0), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52A"))
        }
        #5-Grams, Skip-2B
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-0), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52B"))
        }
        #5-Grams, Skip-2C
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-0), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52C"))
        }
        #5-Grams, Skip-2D
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52D"))
        }
        #5-Grams, Skip-2E
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-0), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52E"))
        }
        #5-Grams, Skip-2F
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-0), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52F"))
        }
        #5-Grams, Skip-2G
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52G"))
        }
        #5-Grams, Skip-2H
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-0), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52H"))
        }
        #5-Grams, Skip-2I
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-1), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52I"))
        }
        #5-Grams, Skip-2J
        predictorWords <- paste0(newWordDF[(aLOWLen-5), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-4), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-3), "word", drop=TRUE], "+",
                                 newWordDF[(aLOWLen-2), "word", drop=TRUE])
        if(predictorWords %in% predictorWordDF$word) { #Found a match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn,
                                                    multiplier=3,
                                                    sourceAlgo="52J"))
        }
    } else {
        if(nrow(predictionDF) > 0) {
            predictionDF <- predictionDF[order(predictionDF$power,
                                               decreasing = TRUE),,drop=FALSE]
            predictionDF <- predictionDF[1:min(noWordsToReturn,
                                               nrow(predictionDF)),,drop=FALSE]
            return(predictionDF)
        } else {
            return(data.frame(word=c(FALSE), stringsAsFactors = FALSE))
        }
    }
    

    # ### PREDICTION - PRIORITY 2 - Permutations of 4/3-grams match - with no skips
    # 
    # combWordList <- c()
    # 
    # #First do for 4-grams
    # if(aLOWLen == 3) {
    #     #aLineOfWords contains list of words
    #     combWordPermList <- permn(aLineOfWords)
    #     for(aPerm in combWordPermList) {
    #         #print(aPerm)
    #         newCombList <- paste(aPerm, collapse="+")
    #         #if(newCombList == origWordList) next    #skip if alredy checked in orig. order
    #         #print(newCombList)
    #         combWordList <- append(combWordList, c(newCombList))
    #     }
    # }
    # 
    # #Do the same for 3-grams
    # if(aLOWLen >= 2) {
    #     #aLineOfWords contains list of words - take last two words and permutate
    #     combWordPermList <- permn(aLineOfWords[aLOWLen-(1:0)])
    #     #origWordList <- paste0(newWordList[1:3], collaps="+")
    #     for(aPerm in combWordPermList) {
    #         #print(aPerm)
    #         newCombList <- paste(aPerm, collapse="+")
    #         #if(newCombList == origWordList) next    #skip if alredy checked in orig. order
    #         #print(newCombList)
    #         combWordList <- append(combWordList, c(newCombList))
    #     }
    # }
    # 
    # #Now see if any of permutations are in list of perdictors
    # for(predictorWords in combWordList) {
    #     if(predictorWords %in% predictorWordDF$word) { #Found a 4/3-gram permutation match
    #         predictionDF <- rbind(predictionDF,
    #                               addToPredictionDF(predictorWords,
    #                                                 mCWordSpMatrix,
    #                                                 predictorWordDF,
    #                                                 predictedWordDF,
    #                                                 noWordsToReturn))
    #     }
    # }
    # 
    # #Return result if any were found with 4/3-gram permutations
    # lengthToKeep <- min(nrow(predictionDF), noWordsToReturn)
    # if(lengthToKeep > 0) {
    #     predictionDF <- predictionDF[seq(1:lengthToKeep), , drop=FALSE]
    # }
    # if(nrow(predictionDF) > 0) {
    #     return(predictionDF)
    # }
    
    if(nrow(predictionDF) > 0) {
        predictionDF <- predictionDF[order(predictionDF$power,
                                           decreasing = TRUE),,drop=FALSE]
        predictionDF <- predictionDF[1:min(noWordsToReturn,
                                           nrow(predictionDF)),,drop=FALSE]
        return(predictionDF)
    } else {
        return(data.frame(word=c(FALSE), stringsAsFactors = FALSE))
    }
}

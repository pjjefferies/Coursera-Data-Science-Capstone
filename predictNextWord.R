#
#
#
library(tm)
library(combinat)
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
    aLineOfWords <- aLineOfWords[max(aLOWLen-min(2,aLOWLen), 1):aLOWLen]
    aLOWLen <- length(aLineOfWords)
    newWordDF <- data.frame(word=aLineOfWords,stringsAsFactors=FALSE)
    
    #Can't do this because it eliminates 4/3-grams that don't contain first term of 2-grams
    # newWordDF$PredictorWordSeqNo <- match(newWordDF$word,
    #                                       predictorWordList$word,
    #                                       nomatch=-1)
    # newWordDF <- newWordDF[newWordDF$PredictorWordSeqNo > 0,,drop=FALSE]
    
    #topNWords <- data.frame()
    #newWordListLen <- length(newWordList)
    
    predictionDF <- data.frame(word=as.character(),
                               power=as.numeric(),
                               stringsAsFactors = FALSE)
    
    
    ### PREDICTION - PRIORITY 1 - 4-grams, 3-grams, 2-grams with words in-order together

    # First find 4-Gram Matches
    if(aLOWLen >= 3) {
        predictorWords <- paste(newWordDF$word, collapse="+")
        if(predictorWords %in% predictorWordDF$word) { #Found a 4-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn))
            return(predictionDF)
        }
    }

    # Second find 3-Gram Matches
    if(aLOWLen >= 2) {
        predictorWords <- paste(newWordDF[(aLOWLen-1):aLOWLen, "word", drop=TRUE], collapse="+")
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3/2-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn))
            return(predictionDF)
        }
    }

    # Third find 2-Gram Matches
    if(aLOWLen >= 1) {
        predictorWords <- newWordDF[aLOWLen, "word", drop=TRUE]
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3/2-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn))
            return(predictionDF)
        }
    }
    
    
        
    ### PREDICTION - PRIORITY 2 - Permutations of 4/3-grams match - with no skips
    
    combWordList <- c()
    
    #First do for 4-grams
    if(aLOWLen == 3) {
        #aLineOfWords contains list of words
        combWordPermList <- permn(aLineOfWords)
        for(aPerm in combWordPermList) {
            #print(aPerm)
            newCombList <- paste(aPerm, collapse="+")
            #if(newCombList == origWordList) next    #skip if alredy checked in orig. order
            #print(newCombList)
            combWordList <- append(combWordList, c(newCombList))
        }
    }
    
    #Do the same for 3-grams
    if(aLOWLen >= 2) {
        #aLineOfWords contains list of words - take last two words and permutate
        combWordPermList <- permn(aLineOfWords[aLOWLen-(1:0)])
        #origWordList <- paste0(newWordList[1:3], collaps="+")
        for(aPerm in combWordPermList) {
            #print(aPerm)
            newCombList <- paste(aPerm, collapse="+")
            #if(newCombList == origWordList) next    #skip if alredy checked in orig. order
            #print(newCombList)
            combWordList <- append(combWordList, c(newCombList))
        }
    }
    
    #Now see if any of permutations are in list of perdictors
    for(predictorWords in combWordList) {
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3-gram permutation match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn))
        }
    }
    
    #Return result if any were found with 4/3-gram permutations
    lengthToKeep <- min(nrow(predictionDF), noWordsToReturn)
    if(lengthToKeep > 0) {
        predictionDF <- predictionDF[seq(1:lengthToKeep), , drop=FALSE]
    }
    if(nrow(predictionDF) > 0) {
        return(predictionDF)
    }
    
    ### PREDICTION - PRIORITY 3 - Skip-1 Grams - 3/2-gram predictions
    
    combWordList <- c()
    
    if(aLOWLen >= 3) {
        combWordList <- append(combWordList, paste(c(aLineOfWords[3], aLineOfWords[1]),
                                                   collapse="+"))
        combWordList <- append(combWordList, paste(c(aLineOfWords[3], aLineOfWords[2]),
                                                   collapse="+"))
    }
    if(aLOWLen >= 2) {
        combWordList <- append(combWordList, c(aLineOfWords[2]))
    }
    
    #Now see if any of skip-1 words are in list of perdictors
    for(predictorWords in combWordList) {
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3-gram permutation match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn))
        }
    }
    
    #Return result if any were found with 3/2-gram, skip-1's
    lengthToKeep <- min(nrow(predictionDF), noWordsToReturn)
    if(lengthToKeep > 0) {
        predictionDF <- predictionDF[seq(1:lengthToKeep), , drop=FALSE]
    }
    if(nrow(predictionDF) > 0) {
        return(predictionDF)
    }
    
    ### PREDICTION - PRIORITY 4 - Skip-2 Grams - 2-gram predictions
    
    if(aLOWLen >= 3) {
        predictorWords <- newWordDF[1, "word", drop=TRUE]
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3/2-gram match
            predictionDF <- rbind(predictionDF,
                                  addToPredictionDF(predictorWords,
                                                    mCWordSpMatrix,
                                                    predictorWordDF,
                                                    predictedWordDF,
                                                    noWordsToReturn))
        }
        
        #Return result if any were found with 2-gram, skip-s's
        lengthToKeep <- min(nrow(predictionDF), noWordsToReturn)
        if(lengthToKeep > 0) {
            predictionDF <- predictionDF[seq(1:lengthToKeep), , drop=FALSE]
        }
        if(nrow(predictionDF) > 0) {
            return(predictionDF)
        }
    }
    
    ### No predictions found! :-( Return FALSE
    #writeLines("No predictions found. Return False")
    return(data.frame(word=c(FALSE), stringsAsFactors = FALSE))
}

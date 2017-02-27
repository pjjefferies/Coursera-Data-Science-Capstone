#
#
#

predictNextWord <- function(wordsToPredictBy,
                            mCWordSpMatrix,
                            predictorWordDF,
                            predictedWordDF,
                            noWordsToReturn = 1,
                            skipPenalty = 2,
                            removeStopWords=TRUE,
                            removeWordSuffixes=TRUE) {
    library(tm)
    library(combinat)
    source("CleanCorpus.R")
    #writeLines(paste0("pNW 1.0: ", newWordList))
    
    # shortList <- data.frame(count=as.integer(),
    #                         basis=as.character(),
    #                         prediction=as.character(),
    #                         word=as.character(),
    #                         rowCount=as.integer(),
    #                         freq=as.numeric(),
    #                         cumFreq=as.numeric())

    #print(newWordList)
    
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
    
    aLineOfWords <- strsplit(textOutOfCorpus, " ")[[1]]
    newWordDF <- data.frame(word=aLineOfWords,stringsAsFactors=FALSE)
    
    #Can't do this because it eliminates 4/3-grams that don't contain first term of 2-grams
    # newWordDF$PredictorWordSeqNo <- match(newWordDF$word,
    #                                       predictorWordList$word,
    #                                       nomatch=-1)
    # newWordDF <- newWordDF[newWordDF$PredictorWordSeqNo > 0,,drop=FALSE]
    
    #topNWords <- data.frame()
    #newWordListLen <- length(newWordList)
    
    predictionDF <- data.frame(prediction=as.character(),
                               power=as.numeric())
    
    
    ### PREDICTION - PRIORITY 1 - 4-grams, 3-grams, 2-grams with words in-order together

    # First find 4-Gram Matches
    if(length(aLineOfWords) >= 3) {
        predictorWords <- paste(newWordDF$word, collapse="+")
        if(predictorWords %in% predictorWordDF$word) { #Found a 4-gram match
            predictorRow <- match(predictorWords,
                                  predictorWordDF$word)
            predictRow <- mCWordSpMatrix[predictorRow, , drop=FALSE]
            
            #to prevent sequencing problem when collapsing sparse matrix
            predictRow <- as.matrix(predictRow)
            
            #add seq to keep track of orig. columns
            predictRow <- rbind(predictRow, seq(1:ncol(predictRow)))
            
            #In predictRow:
            #         #    Row 1: Number of Observations in Training
            #         #    Row 2: Sequence of Word being predicted
            
            #Filter-out all non-zero word columns
            predictRow <- predictRow[, predictRow[1,]>0, drop=FALSE]
            
            #This should alwasy be true since predictor was is predictor list
            if(ncol(predictRow) > 0) {
                totalPredictorObs <- sum(predictRow[1,])
                predictionDF <- rbind(predictionDF,
                                      data.frame(predictedWordNo=predictRow[2,],
                                                 power=(predictRow[1,] / totalPredictorObs)))
                #sort resulting prediction by power
                predictionDF <- predictionDF[sort(predictionDF$power, decreasing=TRUE),,drop=FALSE]
                #only save top number of predictions requested
                predictionDF <- predictionDF[seq(1:noWordsToReturn), , drop=FALSE]
                predictionDF$word <- predictedWordDF[predictionDF$predictedWordNo, "word"]
                predictionDF <- predictionDF[,c("word", "power")]
                #return(predictionDF)
            }
        }
    }

    # Second find 3-Gram Matches
    if(length(aLineOfWords) >= 2) {
        predictorWords <- paste(newWordDF[2:3, "word", drop=FALSE], collapse="+")
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3/2-gram match
            predictorRow <- match(predictorWords,
                                  predictorWordDF$word)
            predictRow <- mCWordSpMatrix[predictorRow, , drop=FALSE]
            
            #to prevent sequencing problem when collapsing sparse matrix
            predictRow <- as.matrix(predictRow)
            
            #add seq to keep track of orig. columns
            predictRow <- rbind(predictRow, seq(1:ncol(predictRow)))
            
            #In predictRow:
            #         #    Row 1: Number of Observations in Training
            #         #    Row 2: Sequence of Word being predicted
            
            #Filter-out all non-zero word columns
            predictRow <- predictRow[, predictRow[1,]>0, drop=FALSE]
            
            #This should alwasy be true since predictor was is predictor list
            if(ncol(predictRow) > 0) {
                totalPredictorObs <- sum(predictRow[1,])
                predictionDF <- rbind(predictionDF,
                                      data.frame(predictedWordNo=predictRow[2,],
                                                 power=(predictRow[1,] / totalPredictorObs)))
                #sort resulting prediction by power
                predictionDF <- predictionDF[sort(predictionDF$power, decreasing=TRUE),,drop=FALSE]
                #only save top number of predictions requested
                predictionDF <- predictionDF[seq(1:noWordsToReturn), , drop=FALSE]
                predictionDF$word <- predictedWordDF[predictionDF$predictedWordNo, "word"]
                predictionDF <- predictionDF[,c("word", "power")]
                #return(predictionDF)
            }
        }
    }

    # Third find 2-Gram Matches
    if(length(aLineOfWords) >= 1) {
        predictorWords <- newWordDF[3, "word", drop=TRUE]
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3/2-gram match
            predictorRow <- match(predictorWords,
                                  predictorWordDF$word)
            predictRow <- mCWordSpMatrix[predictorRow, , drop=FALSE]
            
            #to prevent sequencing problem when collapsing sparse matrix
            predictRow <- as.matrix(predictRow)
            
            #add seq to keep track of orig. columns
            predictRow <- rbind(predictRow, seq(1:ncol(predictRow)))
            
            #In predictRow:
            #         #    Row 1: Number of Observations in Training
            #         #    Row 2: Sequence of Word being predicted
            
            #Filter-out all non-zero word columns
            predictRow <- predictRow[, predictRow[1,]>0, drop=FALSE]
            
            #This should alwasy be true since predictor was is predictor list
            if(ncol(predictRow) > 0) {
                totalPredictorObs <- sum(predictRow[1,])
                predictionDF <- rbind(predictionDF,
                                      data.frame(predictedWordNo=predictRow[2,],
                                                 power=(predictRow[1,] / totalPredictorObs)))
                #sort resulting prediction by power
                predictionDF <- predictionDF[sort(predictionDF$power, decreasing=TRUE),,drop=FALSE]
                #only save top number of predictions requested
                predictionDF <- predictionDF[seq(1:noWordsToReturn), , drop=FALSE]
                predictionDF$word <- predictedWordDF[predictionDF$predictedWordNo, "word"]
                predictionDF <- predictionDF[,c("word", "power")]
                #return(predictionDF)
            }
        }
    }
    
    #Return result if any were found with 4/3/2-grams
    if(nrow(predictionDF) > 0) {
        return(predictionDF)
    }
    
        
    ### PREDICTION - PRIORITY 2 - Permutations of 4/3-grams match - with no skips
    
    combWordList <- c()
    
    #First do for 4-grams
    if(length(aLineOfWords) == 3) {
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
    if(length(aLineOfWords) >= 2) {
        #aLineOfWords contains list of words - take last two words and permutate
        combWordPermList <- permn(aLineOfWords[length(aLineOfWords)-(1:0)])
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
            predictorRow <- match(predictorWords,
                                  predictorWordDF$word)
            predictRow <- mCWordSpMatrix[predictorRow, , drop=FALSE]
            
            #to prevent sequencing problem when collapsing sparse matrix
            predictRow <- as.matrix(predictRow)
            
            #add seq to keep track of orig. columns
            predictRow <- rbind(predictRow, seq(1:ncol(predictRow)))
            
            #Filter-out all non-zero word columns
            predictRow <- predictRow[, predictRow[1,]>0, drop=FALSE]
            
            #This should alwasy be true since predictor was is predictor list
            if(ncol(predictRow) > 0) {
                totalPredictorObs <- sum(predictRow[1,])
                predictionDF <- rbind(predictionDF,
                                      data.frame(predictedWordNo=predictRow[2,],
                                                 power=(predictRow[1,] / totalPredictorObs)))
                #sort resulting prediction by power
                predictionDF <- predictionDF[sort(predictionDF$power, decreasing=TRUE),,drop=FALSE]
                #only save top number of predictions requested
                predictionDF <- predictionDF[seq(1:noWordsToReturn), , drop=FALSE]
                predictionDF$word <- predictedWordDF[predictionDF$predictedWordNo, "word"]
                predictionDF <- predictionDF[,c("word", "power")]
            }
        }
    }
    
    #Return result if any were found with 4/3-gram permutations
    if(nrow(predictionDF) > 0) {
        return(predictionDF)
    }
    
    ### PREDICTION - PRIORITY 3 - Skip-1 Grams - 3/2-gram predictions
    
    combWordList <- c()
    
    if(length(aLineOfWords) >= 3) {
        combWordList <- append(combWordList, paste(c(aLineOfWords[3], aLineOfWords[1]),
                                                   collapse="+"))
        combWordList <- append(combWordList, paste(c(aLineOfWords[3], aLineOfWords[2]),
                                                   collapse="+"))
    }
    if(length(aLineOfWords) >= 2) {
        combWordList <- append(combWordList, c(aLineOfWords[2]))
    }
    
    #Now see if any of skip-1 words are in list of perdictors
    for(predictorWords in combWordList) {
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3-gram permutation match
            predictorRow <- match(predictorWords,
                                  predictorWordDF$word)
            predictRow <- mCWordSpMatrix[predictorRow, , drop=FALSE]
            
            #to prevent sequencing problem when collapsing sparse matrix
            predictRow <- as.matrix(predictRow)
            
            #add seq to keep track of orig. columns
            predictRow <- rbind(predictRow, seq(1:ncol(predictRow)))
            
            #Filter-out all non-zero word columns
            predictRow <- predictRow[, predictRow[1,]>0, drop=FALSE]
            
            #This should alwasy be true since predictor was is predictor list
            if(ncol(predictRow) > 0) {
                totalPredictorObs <- sum(predictRow[1,])
                predictionDF <- rbind(predictionDF,
                                      data.frame(predictedWordNo=predictRow[2,],
                                                 power=(predictRow[1,] / totalPredictorObs)))
                #sort resulting prediction by power
                predictionDF <- predictionDF[sort(predictionDF$power, decreasing=TRUE),,drop=FALSE]
                #only save top number of predictions requested
                predictionDF <- predictionDF[seq(1:noWordsToReturn), , drop=FALSE]
                predictionDF$word <- predictedWordDF[predictionDF$predictedWordNo, "word"]
                predictionDF <- predictionDF[,c("word", "power")]
            }
        }
    }
    
    #Return result if any were found with 3/2-gram, skip-1's
    if(nrow(predictionDF) > 0) {
        return(predictionDF)
    }
    
    ### PREDICTION - PRIORITY 4 - Skip-2 Grams - 2-gram predictions
    
    if(length(aLineOfWords) >= 3) {
        predictorWords <- newWordDF[1, "word", drop=TRUE]
        if(predictorWords %in% predictorWordDF$word) { #Found a 4/3/2-gram match
            predictorRow <- match(predictorWords,
                                  predictorWordDF$word)
            predictRow <- mCWordSpMatrix[predictorRow, , drop=FALSE]
            
            #to prevent sequencing problem when collapsing sparse matrix
            predictRow <- as.matrix(predictRow)
            
            #add seq to keep track of orig. columns
            predictRow <- rbind(predictRow, seq(1:ncol(predictRow)))
            
            #In predictRow:
            #         #    Row 1: Number of Observations in Training
            #         #    Row 2: Sequence of Word being predicted
            
            #Filter-out all non-zero word columns
            predictRow <- predictRow[, predictRow[1,]>0, drop=FALSE]
            
            #This should alwasy be true since predictor was is predictor list
            if(ncol(predictRow) > 0) {
                totalPredictorObs <- sum(predictRow[1,])
                predictionDF <- rbind(predictionDF,
                                      data.frame(predictedWordNo=predictRow[2,],
                                                 power=(predictRow[1,] / totalPredictorObs)))
                #sort resulting prediction by power
                predictionDF <- predictionDF[sort(predictionDF$power, decreasing=TRUE),,drop=FALSE]
                #only save top number of predictions requested
                predictionDF <- predictionDF[seq(1:noWordsToReturn), , drop=FALSE]
                predictionDF$word <- predictedWordDF[predictionDF$predictedWordNo, "word"]
                predictionDF <- predictionDF[,c("word", "power")]
                #return(predictionDF)
            }
        }
        
        #Return result if any were found with 4/3/2-grams
        if(nrow(predictionDF) > 0) {
            return(predictionDF)
        }
    }
    
    ### No predictions found! :-( Return FALSE
    return(FALSE)
}
#
#
#

predictNextWord <- function(newWordList,
                            mCWordSpMatrix,
                            predictorWordList,
                            predictedWordList,
                            testLineNos,
                            noWordsToReturn = 1,
                            skipPenalty = 2) {
    
    #writeLines(paste0("pNW 1.0: ", newWordList))
    prediction <- NA
    shortList <- data.frame(count=as.integer(),
                            basis=as.character(),
                            prediction=as.character(),
                            word=as.character(),
                            rowCount=as.integer(),
                            freq=as.numeric(),
                            cumFreq=as.numeric())

    #print(newWordList)
    newWordDF <- data.frame(word=newWordList)
    #writeLines("pNW 1.1: newWordDF:")
    #print(newWordDF)
    #writeLines("end")
    
    newWordDF$wordSeqNo <- match(newWordDF$word, row.names(wordListDF), nomatch=-1)
    topNWords <- data.frame()
    newWordListLen <- length(newWordList)
    for(aWordNo in 1:newWordListLen) {
        #print(c("aWordNo: ", aWordNo, newWordList[aWordNo]))
        if(newWordDF[aWordNo, "wordSeqNo"] != -1) {  #only look for word in if wordListDF
            #add predictions with 'power' from first word with no discounting
            #Get Row from Sparse Matrix based on Word in Word List
            predictRow <- mCWordSpMatrix[newWordDF[aWordNo, "wordSeqNo"],,
                                         drop=FALSE]
            #print("predictRow 1.11")
            #print(predictRow)
            
            
            #Convert to matrix as it's probably small enough to be reasonable now
            #and adding sequence in next step seems to put columns out-of-order in
            #sparse matrix
            predictRow <- as.matrix(predictRow)

            #print("predictRow 1.12")
            #print(predictRow[,1:15])
            
            #Add sequence to Sparse Matrix Column so we know what word in word list
            #that each column corresponds to after sorting/filtering
            predictRow <- rbind(predictRow, seq(1:ncol(predictRow)))
            
            #In predictRow:
            #    Row 1: Power of Word Prediction
            #    Row 2: Sequence of Word being predicted
            
            #print("predictRow 1.13")
            #print(predictRow[,1:15])
            
            #Filter-out all non-zero word columns
            predictRow <- predictRow[, predictRow[1,]>0, drop=FALSE]
            
            #print("predictRow 1.14")
            #print(predictRow)
            
            if(ncol(predictRow) == 0) {    #No matches found with current word in wordList
                next
            }
            
            
            #print("0.3:")
            #print("predictRow: ")
            #print(predictRow)

            #Sort remaining columns by 'power' predict
            predictRow <- predictRow[, order(predictRow[1,],
                                             decreasing=TRUE), drop=FALSE]

            #print("0.35:")
            #print("predictRow: ")
            #print(predictRow)
            
            #Select number of words desired or max available, whichever is less
            predictRow <- predictRow[, 1:min(noWordsToReturn,
                                             ncol(predictRow)),
                                     drop=FALSE]
            
            #print("0.4:")
            #print("predictRow: ")
            #print(predictRow)
            #print("head(wordListDF):")
            #print(head(wordListDF))
            #print("end 0.4")
            
            #Take top number of words desired by number from words list
            topNWordsTemp <- wordListDF[predictRow[2,], , drop=FALSE]
            #Add predictive power to temp word list so we can sort/select by in
            #combined list
            topNWordsTemp <- cbind(topNWordsTemp, t(predictRow[1,,drop=FALSE]))[, 2, drop=FALSE]
            colnames(topNWordsTemp) <- c("predictCount")

            #print("0.9:")
            #print(topNWordsTemp)
            
            #penalize n-2, n-3, etc. words
            thisSkipPenalty <- skipPenalty**(newWordListLen-aWordNo+1)
            
            topNWordsTemp$predictCount <- pmax.int(as.integer(topNWordsTemp$predictCount /
                                                         (thisSkipPenalty)), 1L)
            
            #Add temp word list from n'th predictor word, if already in list, add predictive
            #values
            #print("1:")
            #print(topNWordsTemp)
            
            for(aPredWordNo in 1:nrow(topNWordsTemp)) {
                if(row.names(topNWordsTemp[aPredWordNo,,drop=FALSE]) %in% row.names(topNWords)) {
                    topNWords[row.names(topNWordsTemp[aPredWordNo,,drop=FALSE]), "predictCount"] <-
                        topNWords[row.names(topNWordsTemp[aPredWordNo,,drop=FALSE]), "predictCount"] +
                        topNWordsTemp[aPredWordNo,"predictCount"]
                } else {
                    topNWords <- rbind(topNWords, topNWordsTemp[aPredWordNo,,drop=FALSE])
                }
            }
        }
        
    }
    #print("2:")
    #print(topNWords)
    if(nrow(topNWords)>0) {
        topNWords <- topNWords[order(topNWords[,1], decreasing = TRUE),
                               ,drop=FALSE][1:min(noWordsToReturn,
                                                  nrow(topNWords)),,
                                            drop=FALSE]
        return(row.names(topNWords))
    }
    return(FALSE)  #No matches found
}
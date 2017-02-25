library(combinat)

predictNextWord <- function(newWordList,
                            wordList = NA, pairList = NA,
                            tripList = NA, quadList = NA,
                            noWordsToReturn) {
    prediction <- NA
    newWordList <- rev(newWordList)
    shortList <- data.frame(count=as.integer(),
                            basis=as.character(),
                            prediction=as.character(),
                            word=as.character(),
                            rowCount=as.integer(),
                            freq=as.numeric(),
                            cumFreq=as.numeric())

    #First try Quad List if available and newWordList has at least 3 words
    #Try words only in original order
    #writeLines("pNW 1: newWordList:")
    #print(newWordList)
    if(length(newWordList) >= 3) {
        combWord <- paste(newWordList[3], newWordList[2], newWordList[1],
                          sep="+")
        if(combWord %in% quadList$basis) {
            shortList <- rbind(shortList, quadList[quadList$basis == combWord, ])
            #shortList <- quadList[quadList$basis == combWord, ]
            #prediction <- shortList[which.max(shortList$freq), ]$prediction
            #writeLines(paste("pNW 2: Found a prediction with quadList in order", prediction, collaps=","))
            #return(prediction)
        }
    }
    
    #Second Try Trip List in order if newWordList has at least 2 words
    if(length(newWordList) >= 2) {
        combWord <- paste(newWordList[2], newWordList[1], sep="+")
        if(combWord %in% tripList$basis) {
            shortList <- rbind(shortList, tripList[tripList$basis == combWord, ])
            #shortList <- tripList[tripList$basis == combWord, ]
            #prediction <- shortList[which.max(shortList$freq), ]$prediction
            #return(prediction)
        }
    }
    
    #Third Try Pair List if newWordList is not empty
    if(length(newWordList) >= 1) {
        combWord <- newWordList[1]
        #writeLines(c(combWord, pairList$basis==combWord))
        if(combWord %in% pairList$basis) {
            shortList <- rbind(shortList, pairList[pairList$basis == combWord, ])
            #shortList <- pairList[pairList$basis == combWord, ]
            #prediction <- shortList[which.max(shortList$freq), ]$prediction
            #return(prediction)
        }
    }

    #Fourth try Quad List again with 3 of last 4 words if there are that many
    #in any order
    if(length(newWordList) >= 4) {
        combWordPermList <- permn(newWordList[1:4])
        origWordList <- paste0(newWordList[1:3], collaps="+")
        combWordList <- c()
        for(aPerm in combWordPermList) {
            aPerm <- aPerm[1:3]
            #print(aPerm)
            newCombList <- paste(aPerm, collapse="+")
            if(newCombList == origWordList) next    #skip if alredy checked in orig. order
            #print(newCombList)
            combWordList <- append(combWordList, c(newCombList))
        }
        #shortList <- data.frame()
        for(combWord in combWordList) {
            #print(combWord)
            if(combWord %in% quadList$basis) {
                #print("Found One")
                shortList <- rbind(shortList,
                                   quadList[quadList$basis == combWord,])
            }
        }
        #shortList <- shortList[!shortList$prediction %in% newWordList[1:4],]
        #if(nrow(shortList)>0) {
        #    prediction <- shortList[which.max(shortList$freq), ]$prediction
        #    return(prediction)
        #}
        #if(combWord %in% quadList$basis) {
        #    shortList <- quadList[quadList$basis == combWord, ]
        #    prediction <- shortList[which.max(shortList$freq), ]$prediction
        #    #return(prediction)
        #}
    }
    
    
    #Fifth try Trip List again with 2 of last 3 words if there are that many
    #in any order
    if(length(newWordList) >= 3) {
        combWordPermList <- permn(newWordList[1:3])
        origWordList <- paste0(newWordList[1:2], collaps="+")
        combWordList <- c()
        for(aPerm in combWordPermList) {
            aPerm <- aPerm[1:2]
            #print(aPerm)
            newCombList <- paste(aPerm, collapse="+")
            if(newCombList == origWordList) next    #skip if alredy checked in orig. order
            #print(newCombList)
            combWordList <- append(combWordList, c(newCombList))
        }
        #shortList <- data.frame()
        for(combWord in combWordList) {
            #print(combWord)
            if(combWord %in% tripList$basis) {
                #print("Found One")
                shortList <- rbind(shortList,
                                   tripList[tripList$basis == combWord,])
            }
        }
    }

    #Sixth try Pair List again with 1 of last 2 words if there are that many
    #in any order
    if(length(newWordList) >= 2) {
        combWordPermList <- permn(newWordList[1:2])
        origWordList <- newWordList[1]
        combWordList <- c()
        for(aPerm in combWordPermList) {
            aPerm <- aPerm[1]
            #print(aPerm)
            newCombList <- aPerm
            if(newCombList == origWordList) next    #skip if alredy checked in orig. order
            #print(newCombList)
            combWordList <- append(combWordList, c(newCombList))
        }
        #shortList <- data.frame()
        for(combWord in combWordList) {
            #print(combWord)
            if(combWord %in% pairList$basis) {
                #print("Found One")
                shortList <- rbind(shortList,
                                   pairList[pairList$basis == combWord,])
            }
        }
    }
    
    shortList <- shortList[order(shortList$freq),]
    prediction <- shortList[1:min(noWordsToReturn,nrow(shortList)),]
    if(nrow(prediction) >= 1) {
        return(prediction)
    } else {
        return("No prediction for this word list yet")
    }
}
#
#
#
#testlist format to return
#(Orig. String, list of words to a point, n-3 word, n-2 word, n-1 word, n/test word)

buildTestList <- function(anInputFilename,
                           readLines,
                           trainLineNos) {
    testList <- as.data.frame(origLine = as.character(),
                              partialLine = as.character(),
                              nMin3Word = as.character(),
                              nMin2Word = as.character(),
                              nMin1Word = as.character(),
                              testWord  = as.character())

    con <- file(fileName, "r")
    lineCountPrint <- as.integer(readLines /5)
    lineNo <- 0
    while ( TRUE ) {
        lineNo <- lineNo + 1
        if((lineNo %% lineCountPrint) == 0) {
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
        if(colWord) {
            for(aWord in aLine) {
                #print(paste("Should be adding:", aWord))
                #print(paste("It was:", aWord, wordList[aWord, "count"]))
                if(aWord == "") next
                ifelse(aWord %in% row.names(wordList),
                       wordList[aWord, "count"] <- wordList[aWord, "count"] + 1,
                       wordList[aWord, "count"] <- 1)
                #print(paste("It  is:", aWord, wordList[aWord, "count"]))
            }
        }
        if(colPair) {
            for(aWordNo in 1:(length(aLine)-1)) {
                if(length(aLine) < 2) next
                word1 <- aLine[aWordNo]
                word2 <- aLine[aWordNo+1]
                #print(paste("3: aWordNo:", aWordNo, ", word1:", word1, ", word2:", word2))
                if(is.na(word1) | is.na(word2)) next
                if(word1 == "" | word2 == "") next
                combWords <- paste(word1, word2, sep="+")
                ifelse(combWords %in% row.names(pairList),
                       pairList[combWords, "count"] <- pairList[combWords, "count"] + 1,
                       pairList[combWords, "count"] <- 1)
                pairList[combWords, "basis"] <- word1
                pairList[combWords, "prediction"] <- word2
            }
        }
        if(colTrip){
            for(aWordNo in 1:(length(aLine)-2)) {
                if(length(aLine) < 3) next
                word1 <- aLine[aWordNo]
                word2 <- aLine[aWordNo+1]
                word3 <- aLine[aWordNo+2]
                #print(paste("4: aWordNo:", aWordNo, ", word1:", word1, ", word2:", word2,
                #            ", word3:", word3))
                if(is.na(word1) | is.na(word2) | is.na(word3)) next
                if(word1 == "" | word2 == "" | word3 == "") next
                combWords <- paste(word1, word2, word3, sep="+")
                ifelse(combWords %in% row.names(tripList),
                       tripList[combWords, "count"] <- tripList[combWords, "count"] + 1,
                       tripList[combWords, "count"] <- 1)
                tripList[combWords, "basis"] <- paste(word1, "+", word2, sep="")
                tripList[combWords, "prediction"] <- word3
            }
        }
        if(colQuad){
            for(aWordNo in 1:(length(aLine)-2)) {
                if(length(aLine) < 4) next
                word1 <- aLine[aWordNo]
                word2 <- aLine[aWordNo+1]
                word3 <- aLine[aWordNo+2]
                word4 <- aLine[aWordNo+3]
                #print(paste("4: aWordNo:", aWordNo, ", word1:", word1, ", word2:", word2,
                #            ", word3:", word3, ", word4:", word4))
                if(is.na(word1) | is.na(word2) | is.na(word3) | is.na(word4)) next
                if(word1 == "" | word2 == "" | word3 == "" | word4 == "") next
                combWords <- paste(word1, word2, word3, word4, sep="+")
                ifelse(combWords %in% row.names(quadList),
                       quadList[combWords, "count"] <- quadList[combWords, "count"] + 1,
                       quadList[combWords, "count"] <- 1)
                quadList[combWords, "basis"] <- paste(word1, "+", word2, "+",
                                                      word3, sep="")
                quadList[combWords, "prediction"] <- word4
            }
        }
    }
    close(con)
    if(colWord) {
        wordList <- cleanWordsList(wordList,
                                   addFreq=addFreq, addCumFreq=addCumFreq,
                                   removeSingleCount = removeSingleCount,
                                   removeAboveCumPercent = removeAboveCumPercent)
    }
    if(colPair) {
        pairList <- cleanWordsList(pairList,
                                   addFreq=addFreq, addCumFreq=addCumFreq,
                                   removeSingleCount = removeSingleCount,
                                   removeAboveCumPercent = removeAboveCumPercent)
    }
    if(colTrip) {
        tripList <- cleanWordsList(tripList,
                                   addFreq=addFreq, addCumFreq=addCumFreq,
                                   removeSingleCount = removeSingleCount,
                                   removeAboveCumPercent = removeAboveCumPercent)
    }
    if(colQuad) {
        quadList <- cleanWordsList(quadList,
                                   addFreq=addFreq, addCumFreq=addCumFreq,
                                   removeSingleCount = removeSingleCount,
                                   removeAboveCumPercent = removeAboveCumPercent)
    }
    list("wordList" = wordList, "pairList" = pairList,
         "tripList" = tripList, "quadList" = quadList)
}
}
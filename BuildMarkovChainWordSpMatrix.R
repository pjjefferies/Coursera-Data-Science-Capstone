#
#Function to Read files line by line to create Markov Chain Word Matrix
#

library(Matrix)
library(tm)
#library(ngram)
#library(ggplot2)
#library(SnowballC)
source("CleanCorpus.R")
source("CreateNGrams.R")
#debugSource("CreateNGrams.R")

buildMarkovChainWordSpMatrix <- function(inputDataFilenames,
                                         noLinesToReadFromEach,
                                         trainSkipPenalty,
                                         locationToReadLines,
                                         trainPercent,
                                         maxCumFreq,
                                         removeStopWords,
                                         removeWordSuffixes) {
    set.seed(123456)
    trainSkipPenalty=as.integer(trainSkipPenalty)
    #maxSkip = 2L
    #wordListDF <- data.frame(count=as.integer())
    #linesFromInputFilesWordList <- list()
    dataIntoCorpus <- c()
    #Use line number of smallest file for simplicity of using common line numbers
        minTotalLines <- .Machine$integer.max
        for(anInputFilename in inputDataFilenames) {
            minTotalLines <- min(minTotalLines, as.integer(strsplit(system2("wc",
                                                                            args=c("-l", anInputFilename),
                                                                            stdout=TRUE),
                                                                    " ")[[1]][1]))
        }
        #minTotalLines <- 850000L
    for(anInputFileNo in 1:length(inputDataFilenames)) {
        anInputFilename <- inputDataFilenames[anInputFileNo]
        if(noLinesToReadFromEach <= 1) {  #if <= 1, interprate as a fraction of whole file
            noLinesToRead <- as.integer(noLinesToReadFromEach * minTotalLines)
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
                trainLineNos <- sort(sample(minTotalLines,
                                    as.integer(noLinesToRead*trainPercent)))
            }
        }
        writeLines(paste("    Reading", noLinesToRead, "lines from file",
                         anInputFilename, locText))
        
        con <- file(anInputFilename, "r")
        if(locationToReadLines == "top") {
            linesFromInputFile <- readLines(con=con,
                                            n=noLinesToRead,
                                            skipNul=TRUE,
                                            warn=FALSE)
        } else {
            if(locationToReadLines == "random") {
                allLinesFromInputFile <- readLines(con=con,
                                                   n=minTotalLines,
                                                   skipNul=TRUE,
                                                   warn=FALSE)
                linesFromInputFile <- allLinesFromInputFile[trainLineNos]
                rm(allLinesFromInputFile)
            }
        }
        close(con)
        dataIntoCorpus <- append(dataIntoCorpus, linesFromInputFile)
    }
    #Filter emails
    dataIntoCorpus <- gsub("\\S+@\\S+", "", dataIntoCorpus)
    #Filter URLs
    dataIntoCorpus <- gsub("http[[:alnum:]]*", "", dataIntoCorpus)
    #Filter Handles
    dataIntoCorpus <- gsub("@[[:alnum:]]*", "", dataIntoCorpus)
    #Filter Hashtags
    dataIntoCorpus <- gsub("#[[:alnum:]]*", "", dataIntoCorpus)
    
    #wordPredictData <- Corpus(VectorSource(dataIntoCorpus)) #Not working with Corpus
    wordPredictData <- VCorpus(VectorSource(dataIntoCorpus))
    
    #Memory Clean-up time
    rm(dataIntoCorpus, linesFromInputFile)
    
    wordPredictData <- CleanCorpus(wordPredictData,
                                   removeEmail=FALSE,  #not working
                                   removeURL=FALSE,
                                   removeHandles=FALSE,
                                   removeHashtags=FALSE,
                                   removeStopWords=FALSE,  #keep as FALSE for 3+Grams
                                   appSpecWordsFile=FALSE,
                                   removeWordSuffixes=removeWordSuffixes,
                                   myBadWordsFile=FALSE,
                                   #myBadWordsFile="myTermsToBlock.csv",
                                   convertPlainText=FALSE) #tdm crashes with TRUE
    

    wordPredictDataFor2Grams <- tm_map(wordPredictData,
                                       removeWords,
                                       stopwords("english")) #for 2-Grams only

    #dataOutOfCorpusString <- concatenate(content(wordPredictData))
    #dataOutOfCorpusFor2GramsString <- concatenate(content(wordPredictDataFor2Grams))
    #rm(wordPredictData, wordPredictDataFor2Grams)
    
    skipGrams <- data.frame(freq=as.integer(),
                            predictor=as.character(),
                            predicted=as.character(),
                            stringsAsFactors = FALSE)

    writeLines("    Creating 2-Grams")
    biGrams <- xgram(wordPredictDataFor2Grams, 2)  #not working?
    #biGrams <- get.phrasetable(ngram(str=dataOutOfCorpusFor2GramsString, n=2))[,c("ngrams", "freq"), drop=FALSE]
    #rm(wordPredictDataFor2Grams)
    biGrams <- biGrams[grep("[â€]",biGrams$ngrams, value=FALSE, invert=TRUE),
                       , drop=FALSE]
    #biGrams <- biGrams[biGrams$freq > 1, , drop=FALSE] #elim for not to change to powe based
    # if(maxCumFreq < 1) {     #Eliminate cumulative frequencies for 2-grams only
    #     totalBigrams <- sum(biGrams$freq)
    #     biGrams$frac <- biGrams$freq / totalBigrams
    #     biGrams[1, "totalFrac"] <- biGrams[1, "frac"]
    #     for(aBiGramRowNo in 2:nrow(biGrams)) {
    #         biGrams[aBiGramRowNo, "totalFrac"] <- biGrams[(aBiGramRowNo-1), "totalFrac"] +
    #             biGrams[aBiGramRowNo, "frac"]
    #     }
    #     biGrams <- biGrams[biGrams$totalFrac < maxCumFreq, , drop=FALSE]
    # }
    if(nrow(biGrams) != 0) {
        #biGrams$ngrams <- as.character(biGrams$ngrams) #I think this can be eliminated with stingsAsFactor=FALSE in CNG
        for(aBiGramNo in 1:nrow(biGrams)) {
            thisNGram <- strsplit(biGrams[aBiGramNo, "ngrams"], " ")[[1]]
            if(is.na(thisNGram[1]) || is.na(thisNGram[2])) next
            biGrams[aBiGramNo, "predictor"] <- thisNGram[1]
            biGrams[aBiGramNo, "predicted"] <- thisNGram[2]
            biGrams[aBiGramNo, "freq"] <- biGrams[aBiGramNo, "freq"] * 2
        }
        #biGrams <- biGrams[!is.na(biGrams$predicted), , drop=FALSE] #shouldn't be necessary
        biGrams <- biGrams[, c("freq", "predictor", "predicted"), drop=FALSE]
    }

    
    writeLines("    Creating 3-Grams and 2-Grams, Skip-1")
    triGrams <- xgram(wordPredictData, 3)
    #triGrams <- get.phrasetable(ngram(str=dataOutOfCorpusString, n=3))[,c("ngrams", "freq"), drop=FALSE]
    triGrams <- triGrams[grep("[â€]",triGrams$ngrams, value=FALSE, invert=TRUE),
                       , drop=FALSE]
    #triGrams <- triGrams[triGrams$freq > 1, , drop=FALSE]
    if(nrow(triGrams) != 0) {
        #triGrams$ngrams <- as.character(triGrams$ngrams)
        #triGramsNames <- triGrams$ngrams
        for(aTriGramNo in 1:nrow(triGrams)) {
            thisNGram <- strsplit(triGrams[aTriGramNo, "ngrams"], " ")[[1]]
            #if(nchar(thisNGram[3]) < 2) next
            if(is.na(thisNGram[1]) || is.na(thisNGram[2]) || is.na(thisNGram[3])) next
            triGrams[aTriGramNo, "predictor"] <- paste0(thisNGram[1],"+",thisNGram[2])
            triGrams[aTriGramNo, "predicted"] <- thisNGram[3]
            triGrams[aTriGramNo, "freq"] <- triGrams[aTriGramNo, "freq"] * 3
            skipGrams <- rbind(skipGrams,
                               data.frame(freq=triGrams[aTriGramNo, "freq"] * 1,
                                          predictor=thisNGram[1],
                                          predicted=thisNGram[3]))
        }
        triGrams <- triGrams[, c("freq", "predictor", "predicted"), drop=FALSE]
        #triGrams <- triGrams[!is.na(triGrams$predicted), , drop=FALSE]
        #rm(triGramsNames)
    }
    

    writeLines("    Creating 4-Grams and 3-Grams, Skip-1")
    quadGrams <- xgram(wordPredictData, 4)
    #quadGrams <- get.phrasetable(ngram(str=dataOutOfCorpusString, n=4))[,c("ngrams", "freq"), drop=FALSE]
    quadGrams <- quadGrams[grep("[â€]",quadGrams$ngrams, value=FALSE, invert=TRUE),
                           , drop=FALSE]
    #quadGrams <- quadGrams[quadGrams$freq > 1, , drop=FALSE]
    if(nrow(quadGrams) != 0) {
        #quadGrams$ngrams <- as.character(quadGrams$ngrams)
        #quadGramsNames <- quadGrams$ngrams
        for(aQuadGramNo in 1:nrow(quadGrams)) {
            thisNGram <- strsplit(quadGrams[aQuadGramNo, "ngrams"], " ")[[1]]
            #if(nchar(thisNGram[4]) < 2) next
            if(is.na(thisNGram[1]) || is.na(thisNGram[2]) ||
               is.na(thisNGram[3]) || is.na(thisNGram[4])) next
            quadGrams[aQuadGramNo, "predictor"] <- paste0(thisNGram[1],"+",
                                                          thisNGram[2],"+",
                                                          thisNGram[3])
            quadGrams[aQuadGramNo, "predicted"] <- thisNGram[4]
            quadGrams[aQuadGramNo, "freq"] <- quadGrams[aQuadGramNo, "freq"] * 4
            skipGrams <- rbind(skipGrams,
                               data.frame(freq=quadGrams[aQuadGramNo, "freq"] * 2,
                                          predictor=paste0(thisNGram[1],"+",thisNGram[3]),
                                          predicted=thisNGram[4]),
                               data.frame(freq=quadGrams[aQuadGramNo, "freq"] * 2,
                                          predictor=paste0(thisNGram[1],"+",thisNGram[2]),
                                          predicted=thisNGram[4]))
        }
        quadGrams <- quadGrams[, c("freq", "predictor", "predicted"), drop=FALSE]
        #quadGrams <- quadGrams[!is.na(quadGrams$predicted), , drop=FALSE]
        #rm(quadGramsNames)
    }


    writeLines("    Creating 5-Grams and 4-Grams, Skip-1")
    quintGrams <- xgram(wordPredictData, 5)
    #quintGrams <- get.phrasetable(ngram(str=dataOutOfCorpusString, n=5))[,c("ngrams", "freq"), drop=FALSE]
    quintGrams <- quintGrams[grep("[â€]",quintGrams$ngrams, value=FALSE, invert=TRUE),
                           , drop=FALSE]
    #quintGrams <- quintGrams[quintGrams$freq > 1, , drop=FALSE]
    if(nrow(quintGrams) != 0) {
        #quintGrams$ngrams <- as.character(quintGrams$ngrams)
        #quintGramsNames <- quintGrams$ngrams
        for(aQuintGramNo in 1:nrow(quintGrams)) {
            thisNGram <- strsplit(quintGrams[aQuintGramNo, "ngrams"], " ")[[1]]
            #if(nchar(thisNGram[4]) < 2) next
            if(is.na(thisNGram[1]) || is.na(thisNGram[2]) ||
               is.na(thisNGram[3]) || is.na(thisNGram[4]) ||
               is.na(thisNGram[5])) next
            quintGrams[aQuintGramNo, "predictor"] <- paste0(thisNGram[1],"+",
                                                            thisNGram[2],"+",
                                                            thisNGram[3],"+",
                                                            thisNGram[4])
            quintGrams[aQuintGramNo, "predicted"] <- thisNGram[5]
            quintGrams[aQuintGramNo, "freq"] <- quintGrams[aQuintGramNo, "freq"] * 5
            skipGrams <- rbind(skipGrams,
                               data.frame(freq=quintGrams[aQuintGramNo, "freq"] * 3,
                                          predictor=paste0(thisNGram[1],"+",
                                                           thisNGram[3],"+",
                                                           thisNGram[4]),
                                          predicted=thisNGram[5]),
                               data.frame(freq=quintGrams[aQuintGramNo, "freq"] * 3,
                                          predictor=paste0(thisNGram[1],"+",
                                                           thisNGram[2],"+",
                                                           thisNGram[4]),
                                          predicted=thisNGram[5]),
                               data.frame(freq=quintGrams[aQuintGramNo, "freq"] * 3,
                                          predictor=paste0(thisNGram[1],"+",
                                                           thisNGram[2],"+",
                                                           thisNGram[3]),
                                          predicted=thisNGram[5]))
        }
        quintGrams <- quintGrams[, c("freq", "predictor", "predicted"), drop=FALSE]
        #quadGrams <- quadGrams[!is.na(quadGrams$predicted), , drop=FALSE]
        #rm(quadGramsNames)
    }
    

    writeLines("    Creating 6-Grams for 5-Grams, Skip-1 Only")
    hexGrams <- xgram(wordPredictData, 6)
    #hexGrams <- get.phrasetable(ngram(str=dataOutOfCorpusString, n=6))[,c("ngrams", "freq"), drop=FALSE]
    hexGrams <- hexGrams[grep("[â€]",hexGrams$ngrams, value=FALSE, invert=TRUE),
                         , drop=FALSE]
    #hexGrams <- hexGrams[hexGrams$freq > 1, , drop=FALSE]
    if(nrow(hexGrams) != 0) {
        #hexGrams$ngrams <- as.character(hexGrams$ngrams)
        #quintGramsNames <- hexGrams$ngrams
        for(aHexGramNo in 1:nrow(hexGrams)) {
            thisNGram <- strsplit(hexGrams[aHexGramNo, "ngrams"], " ")[[1]]
            if(is.na(thisNGram[1]) || is.na(thisNGram[2]) ||
               is.na(thisNGram[3]) || is.na(thisNGram[4]) ||
               is.na(thisNGram[5]) || is.na(thisNGram[6])) next
            #if(nchar(thisNGram[4]) < 2) next
            # hexGrams[aHexGramNo, "predictor"] <- paste0(thisNGram[1],"+",
            #                                                 thisNGram[2],"+",
            #                                                 thisNGram[3],"+",
            #                                                 thisNGram[4])
            # hexGrams[aHexGramNo, "predicted"] <- thisNGram[5]
            # hexGrams[aHexGramNo, "freq"] <- hexGrams[aHexGramNo, "freq"] * 6
            skipGrams <- rbind(skipGrams,
                               data.frame(freq=hexGrams[aHexGramNo, "freq"] * 4,
                                          predictor=paste0(thisNGram[1],"+",
                                                           thisNGram[3],"+",
                                                           thisNGram[4],"+",
                                                           thisNGram[5]),
                                          predicted=thisNGram[6]),
                               data.frame(freq=hexGrams[aHexGramNo, "freq"] * 4,
                                          predictor=paste0(thisNGram[1],"+",
                                                           thisNGram[2],"+",
                                                           thisNGram[4],"+",
                                                           thisNGram[5]),
                                          predicted=thisNGram[6]),
                               data.frame(freq=hexGrams[aHexGramNo, "freq"] * 4,
                                          predictor=paste0(thisNGram[1],"+",
                                                           thisNGram[2],"+",
                                                           thisNGram[3],"+",
                                                           thisNGram[5]),
                                          predicted=thisNGram[6]),
                               data.frame(freq=hexGrams[aHexGramNo, "freq"] * 4,
                                          predictor=paste0(thisNGram[1],"+",
                                                           thisNGram[2],"+",
                                                           thisNGram[3],"+",
                                                           thisNGram[4]),
                                          predicted=thisNGram[6]))
        }
        #quintGrams <- quintGrams[, c("freq", "predictor", "predicted"), drop=FALSE]
        #quadGrams <- quadGrams[!is.na(quadGrams$predicted), , drop=FALSE]
        rm(hexGrams)
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
    noNGrams <- nrow(biGrams) + nrow(triGrams) + nrow(quadGrams) +
        nrow(quintGrams) + nrow(skipGrams)

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
    
    nGramsToAdd <-  biGrams#[, c("ngrams", "freq", "predictor", "predicted"),
                            #drop=FALSE]
    if(nrow(triGrams) > 0) {
        nGramsToAdd <- rbind(nGramsToAdd, triGrams)
        writeLines("    2-Grams and 3-Grams added successfully")
    }
    
    if(nrow(quadGrams) > 0) {
        nGramsToAdd <- rbind(nGramsToAdd, quadGrams)
        writeLines("    4-Grams added successfully")
    }
    
    if(nrow(quintGrams) > 0) {
        nGramsToAdd <- rbind(nGramsToAdd, quintGrams)
        writeLines("    5-Grams added successfully")
    }
    
    if(nrow(skipGrams) > 0) {
        nGramsToAdd <- rbind(nGramsToAdd, skipGrams)
        writeLines("    Skip-Grams added successfully")
    }

    writeLines(c("    Finished cleaning-up words list",
                 "    Trimming to maxCumFreq limit"))

    rm(biGrams, triGrams, quadGrams, quintGrams, skipGrams)

    #Sort by frequency in decending order even if not filtering
    nGramsToAdd <- nGramsToAdd[order(nGramsToAdd$freq, decreasing=TRUE),
                               , drop=FALSE]
    
    #Limit nGramsToAdd per maxCumFreq
    if(maxCumFreq < 1) {
        nGramsTotalFreq <- sum(nGramsToAdd$freq)
        nGramsToAdd$frac <- nGramsToAdd$freq / nGramsTotalFreq
        nGramsToAdd[1, "cumFrac"] <- nGramsToAdd[1, "frac"]
        for(anNGramNo in 2:nrow(nGramsToAdd)) {
            nGramsToAdd[anNGramNo, "cumFrac"] <- nGramsToAdd[(anNGramNo-1), "cumFrac"] +
                nGramsToAdd[anNGramNo, "frac"]
        }
        nGramsToAdd <- nGramsToAdd[nGramsToAdd$cumFrac < maxCumFreq,
                                   c("freq", "predictor", "predicted"),
                                   drop=FALSE]
    }

    writeLines(c("    Finished trimming to maxCumFreq limit",
                 "    Start creating Markov Matrix"))
    predictorWordDF <- data.frame(word=unique(nGramsToAdd$predictor))
    predictedWordDF <- data.frame(word=unique(nGramsToAdd$predicted))
    mCWordSpMatrix <- Matrix(data=0,
                             nrow=nrow(predictorWordDF),
                             ncol=nrow(predictedWordDF),
                             sparse=TRUE, doDiag=FALSE)
    
    nGramsToProcess <- nrow(nGramsToAdd)
    lineCountPrint <- max(as.integer(nGramsToProcess /5),1)
    #lineNo <- 0
    
    for(anNGramNo in 1:nGramsToProcess) {
        #lineNo <- lineNo + 1
        if(anNGramNo %% lineCountPrint == 0) {
            writeLines(paste("      Processing line", anNGramNo,
                             "of", nGramsToProcess))
        }
        predictorWordNo <- match(nGramsToAdd[anNGramNo, "predictor"],
                                 predictorWordDF$word, nomatch=-1)
        predictedWordNo <- match(nGramsToAdd[anNGramNo, "predicted"],
                                 predictedWordDF$word, nomatch=-1)
        mCWordSpMatrix[predictorWordNo, predictedWordNo] <- 
            nGramsToAdd[anNGramNo, "freq"]
    }
    
    writeLines(paste("    Finished creating Markov Matrix"))
    
    #Return the data for size analysis
    return(list(mCWordSpMatrix, predictorWordDF, predictedWordDF, trainLineNos))
}





test_buildMarkovChainWordSpMatrix <- function() {
    inputDataFilenames <- c("en_US.blogs.txt" #,
                            #"en_US.news.txt",
                            #"en_US.twitter.txt"
                            )
    noLinesToReadFromEach <- 100
    trainSkipPenalty <- 2
    locationToReadLines <- "top"
    trainPercent <- 0.6
    maxCumFreq <- 0.9
    removeStopWords <- FALSE
    removeWordSuffixes <- FALSE
    
    mCWordSMWLTemp <- buildMarkovChainWordSpMatrix(inputDataFilenames,
                                 noLinesToReadFromEach,
                                 trainSkipPenalty,
                                 locationToReadLines,
                                 trainPercent,
                                 maxCumFreq,
                                 removeStopWords,
                                 removeWordSuffixes) 
    
    mCWordSpMatrix <- mCWordSMWLTemp[1][[1]]
    predictorWordDF<- mCWordSMWLTemp[2][[1]]
    predictedWordDF <- mCWordSMWLTemp[3][[1]]
    trainLineNos <- mCWordSMWLTemp[4][[1]]
}

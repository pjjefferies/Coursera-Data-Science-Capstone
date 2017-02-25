#
#
#
library(caret)
library(Matrix)
#source("BuildTestList.R")
source("PredictNextWord.R")

testWordPrediction <- function(predictModelBaseFilename, runQueueFilename) {
    #Load Run Queue
    runQueue <- read.csv(runQueueFilename, comment.char = "#", row.names=1)
    
    for(aTestNo in 1:nrow(runQueue)) {
        writeLines(c("",paste0("Predicting for test run ", aTestNo, " of ", nrow(runQueue))))
        #if(!is.na(runQueue[aRunNo, "Accuracy"])) {
        #    writeLines("   Results already in runQueue file. Skipping")
        #    next
        #}

        aRunDataMCSpFilename <- paste0(predictModelBaseFilename,
                                       "SpMC.txt")
        aRunDataMCSpWordListFilename <- paste0(predictModelBaseFilename,
                                               "SpWL.csv")

        if(all(file.exists(aRunDataMCSpFilename,
                           aRunDataMCSpWordListFilename))) {
            #Load trained data and training line numbers from files
            mCWordSpMatrix <- readMM(aRunDataMCSpFilename)
            wordListDF <- read.csv(aRunDataMCSpWordListFilename,
                                   comment.char="#", row.names=1)
        } else {
            writeLines(paste0("   All files for testing are not available for ",
                              aRunDataBaseFilename, "."))
            next
        }

        #Do a prediction test for the line in testList and save results.
        #predictSkipPenalty <- runQueue[aRunNo, "predictSkipPenalty"]
        predictSkipPenalty <- 2

        thisLine <- runQueue[aTestNo, "Sentence.Fragment"]
        if(length(thisLine) == 0) next
        aLineOfWords <- strsplit(tolower(gsub("[^a-zA-Z \']", "", thisLine )), " ")[[1]]
        
        wordPosToPredict <- length(aLineOfWords) + 1
        
        if(wordPosToPredict >= 5) {
            nMin4Word <- aLineOfWords[wordPosToPredict-4]
        } else {
            nMin4Word <- NA
        }
        if(wordPosToPredict >= 4) {
            nMin3Word <- aLineOfWords[wordPosToPredict-3]
        } else {
            nMin3Word <- NA
        }
        if(wordPosToPredict >= 3) {
            nMin2Word <- aLineOfWords[wordPosToPredict-2]
        } else {
            nMin2Word <- NA
        }
        nMin1Word <- aLineOfWords[wordPosToPredict-1]
        
        
        if(is.na(nMin2Word)) {
            newWordList <- c(nMin1Word)
        } else {
            if(is.na(nMin3Word)) {
                newWordList <- c(nMin2Word,
                                 nMin1Word)
                } else {
                if(is.na(nMin4Word)) {
                    newWordList <- c(nMin3Word,
                                     nMin2Word,
                                     nMin1Word)
                } else {
                    newWordList <- c(nMin4Word,
                                     nMin3Word,
                                     nMin2Word,
                                     nMin1Word)
                }
            }
        }
        
        #noWordsToReturn <- runQueue[aRunNo, "noWordsToPredict"]
        noWordsToReturn <- 10
        tempListOfPredictions <- as.character(predictNextWord(newWordList = newWordList,
                                                              mCWordSpMatrix = mCWordSpMatrix,
                                                              wordListDF= wordListDF,
                                                              noWordsToReturn=noWordsToReturn,
                                                              skipPenalty=predictSkipPenalty))
        writeLines("tWP 2.1: tempListOfPreidictions: ")
        print(tempListOfPredictions)
        for(aPredictNo in 1:length(tempListOfPredictions)) {
            aPredict <- tempListOfPredictions[aPredictNo]
            thisPredictName <- paste0("prediction", as.character(aPredictNo))
            if(aPredict == FALSE) {    #No matches found
                runQueue[aTestNo, thisPredictName] <- NA
                break
            }
            runQueue[aTestNo, thisPredictName] <- aPredict
        }
    }

        #print(c("prediction: ", testList[aTestNo, "prediction"], "actual: ", testList[aTestNo, "testWord"]))
        
        writeLines("  Finished predicting all lines in file. Writing testList and saving metrics")
        write.csv(runQueue, runQueueFilename)
}



testWordPrediction("MarkovChains//markovChain1000cumPer100wFNo7TSP2",
                   "Quiz2-NLP1QuestionListTry1.csv")
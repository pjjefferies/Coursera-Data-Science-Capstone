#
#
#
library(caret)
library(Matrix)
#source("BuildTestList.R")
debugSource("PredictNextWord.R")

testWordPrediction <- function(predictModelBaseFilename, runQueueFilename) {
    #Load Run Queue
    runQueue <- read.csv(runQueueFilename,
                         comment.char = "#",
                         row.names=1,
                         as.is=TRUE)
    
    for(aTestNo in 1:nrow(runQueue)) {
        writeLines(c("",paste0("Predicting for test run ", aTestNo, " of ", nrow(runQueue))))
        #if(!is.na(runQueue[aRunNo, "Accuracy"])) {
        #    writeLines("   Results already in runQueue file. Skipping")
        #    next
        #}

        aRunDataMCSpFilename <- paste0(predictModelBaseFilename,
                                       "SpMC.txt")
        predictorWordDFFilename <- paste0(predictModelBaseFilename, "SpORWL.csv")
        predictedWordDFFilename <- paste0(predictModelBaseFilename, "SpEDWL.csv")
        
        if(all(file.exists(aRunDataMCSpFilename,
                           predictorWordDFFilename,predictedWordDFFilename))) {
            #Load trained data and training line numbers from files
            mCWordSpMatrix <- readMM(aRunDataMCSpFilename)
            predictorWordDF <- read.csv(predictorWordDFFilename,
                                        comment.char="#", row.names=1, as.is=TRUE)
            predictedWordDF <- read.csv(predictedWordDFFilename,
                                        comment.char="#", row.names=1, as.is=TRUE)
        } else {
            writeLines(paste0("   All files for testing are not available for ",
                              predictModelBaseFilename, "."))
            next
        }

        #Do a prediction test for the line in testList and save results.
        #predictSkipPenalty <- runQueue[aRunNo, "predictSkipPenalty"]
        predictSkipPenalty <- 2

        thisLine <- runQueue[aTestNo, "Sentence.Fragment"]
        if(length(thisLine) == 0) next
        
        
        #noWordsToReturn <- runQueue[aRunNo, "noWordsToPredict"]
        noWordsToReturn <- 10
        tempListOfPredictions <- predictNextWord(wordsToPredictBy = thisLine,
                                                 mCWordSpMatrix = mCWordSpMatrix,
                                                 predictorWordDF=predictorWordDF,
                                                 predictedWordDF=predictedWordDF,
                                                 noWordsToReturn = noWordsToReturn,
                                                 skipPenalty = 2,
                                                 removeStopWords=FALSE,
                                                 removeWordSuffixes=FALSE)
        
        writeLines("tWP 2.1: tempListOfPreidictions: ")
        print(tempListOfPredictions)
        
        correctFlag <- FALSE
        
        for(aPredictNo in 1:nrow(tempListOfPredictions)) {
            aPredict <- tempListOfPredictions[aPredictNo, "word"]
            thisPredictName <- paste0("prediction", as.character(aPredictNo))
            if(aPredict == FALSE) {    #No matches found
                runQueue[aTestNo, thisPredictName] <- NA
                break
            }
            runQueue[aTestNo, thisPredictName] <- aPredict
            if(runQueue[aTestNo, thisPredictName] == runQueue[aTestNo, "try1"]) {
                correctFlag <- TRUE
            }
        }
        runQueue[aTestNo, "Correct"] <- correctFlag
    }

        #print(c("prediction: ", testList[aTestNo, "prediction"], "actual: ", testList[aTestNo, "testWord"]))
        
        writeLines("  Finished predicting all lines in file. Writing testList and saving metrics")
        write.csv(runQueue, runQueueFilename)
}



testWordPrediction("MarkovChains//markovChain1000randomcumPer100wFNo7TSP2RSWFRSXF",
                   "Quiz3-NLP2QuestionListTry.csv")
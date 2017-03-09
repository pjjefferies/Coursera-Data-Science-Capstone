#
#
#
#library(caret)
library(Matrix)
source("BuildTestList.R")
#debugSource("BuildTestList.R")
source("PredictNextWord.R")
#debugSource("PredictNextWord.R")

testWordPrediction <- function(inputDataFilenames, runQueueFilename) {
    #Load Run Queue
    runQueue <- read.csv(runQueueFilename, comment.char = "#", row.names=1,
                         as.is=TRUE)
    
    for(aRunNo in 1:nrow(runQueue)) {
        writeLines(c("",paste0("Building a list for test run ", aRunNo, " of ", nrow(runQueue))))
        if(!(is.null(runQueue[aRunNo, "Accuracy"]) ||
             is.na(runQueue[aRunNo, "Accuracy"]))) {
            writeLines("   Results already in runQueue file. Skipping")
            next
        }
        
        fileNoToLoad <- as.integer(runQueue[aRunNo, "filesToLoad"])
        if(fileNoToLoad < 1 | fileNoToLoad > 7) {
            fileNoToLoad <- 7
            runQueue[aRunNo, "filesToLoad"] <- 7
        }
        
        inputDataFilenamesToUse <- c()
        if(fileNoToLoad >= 4) {
            inputDataFilenamesToUse <- c(inputDataFilenames[3])
            fileNoToLoad <- fileNoToLoad - 4
        }
        if(fileNoToLoad >= 2) {
            inputDataFilenamesToUse <- append(inputDataFilenames[2],
                                              inputDataFilenamesToUse)
            fileNoToLoad <- fileNoToLoad - 2
        }
        if(fileNoToLoad == 1) {
            inputDataFilenamesToUse <- append(inputDataFilenames[1],
                                              inputDataFilenamesToUse)
            fileNoToLoad <- fileNoToLoad - 1
        }
        if(fileNoToLoad != 0) {
            writeLines(c("We have a problem.",fileNoToLoad))
            next
        }
        
        removeStopWordsCode <- ifelse(runQueue[aRunNo, "removeStopWords"],
                                      "T", "F")
        removeSuffixes <- ifelse(runQueue[aRunNo, "removeWordSuffixes"],
                                 "T", "F")
        aRunDataBaseFilename <- paste0("MarkovChains//markovChain",
                                       runQueue[aRunNo, "NoLinesEachFileOrFraction"],
                                       runQueue[aRunNo, "LocToReadLines"],
                                       "cumPer",
                                       as.integer(
                                           as.numeric(runQueue[aRunNo,
                                                               "cumPercent"])*100),
                                       "wFNo", runQueue[aRunNo, "filesToLoad"],
                                       "TSP",
                                       round(as.numeric(runQueue[aRunNo,
                                                                 "trainSkipPenalty"]),1),
                                       "RSW", removeStopWordsCode,
                                       "RSX", removeSuffixes)
        aRunDataMCSpFilename <- paste0(aRunDataBaseFilename, "SpMC.txt")
        predictorWordDFFilename <- paste0(aRunDataBaseFilename, "SpORWL.csv")
        predictedWordDFFilename <- paste0(aRunDataBaseFilename, "SpEDWL.csv")
        aRunDataTrainNosFilename <- paste0(aRunDataBaseFilename, "TrainNos.csv")
        aRunDataTestLineNosFilename <- paste0(aRunDataBaseFilename, "TestNos.csv")
        aRunTestListFilename <- paste0(aRunDataBaseFilename, "PSP",
                                       round(runQueue[aRunNo, "predictSkipPenalty"],1),
                                       "noPred", runQueue[aRunNo, "noWordsToPredict"],
                                       "testList.csv")
        
        if(all(file.exists(aRunDataMCSpFilename,
                           aRunDataTrainNosFilename,
                           predictorWordDFFilename,
                           predictedWordDFFilename))) {
            #Load trained data and training line numbers from files
            mCWordSpMatrix <- readMM(aRunDataMCSpFilename)
            predictorWordDF <- read.csv(predictorWordDFFilename,
                                          comment.char="#", row.names=1, as.is=TRUE)
            predictedWordDF <- read.csv(predictedWordDFFilename,
                                          comment.char="#", row.names=1, as.is=TRUE)
            trainLineNos <- read.csv(aRunDataTrainNosFilename,
                                     comment.char="#", row.names=1, as.is=TRUE)[,1]
        } else {
            writeLines(paste0("   All files for testing are not available for ",
                              aRunDataBaseFilename, "."))
            next
        }
        
        if(file.exists(aRunTestListFilename)) {
            testList <- read.csv(aRunTestListFilename, comment.char = "#", row.names=1, as.is=TRUE)
            writeLines(paste0("   Test list file exists. Reading file", aRunTestListFilename))
        } else {
            writeLines(paste0("   Test list file does not exists. Creating file: ", aRunTestListFilename))
            #Create test list from text data files
            testList <- data.frame(origLine = as.character(),
                                   cleanedLine = as.character(),
                                   wordNoToTest = as.character(),
                                   #nMin4Word = as.character(),
                                   nMin3Word = as.character(),
                                   nMin2Word = as.character(),
                                   nMin1Word = as.character(),
                                   testWord  = as.character())
            noLinesToReadFromEach <- runQueue[aRunNo, "NoLinesEachFileOrFraction"]
            locationToReadLines <- runQueue[aRunNo, "LocToReadLines"]
            trainPercent <- runQueue[aRunNo, "trainPercent"]
            
            minTotalLines <- 1000000000L
            # for(anInputFilename in inputDataFilenames) {
            #     minTotalLines <- min(minTotalLines,
            #                          as.integer(strsplit(system2("wc",
            #                                                      args=c("-l",
            #                                                             anInputFilename),
            #                                                      stdout=TRUE),
            #                                              " ")[[1]][1]))
            # }
            minTotalLines <- 850000L
            
            testLineNos <- c()
            for(anInputFilename in inputDataFilenamesToUse) {
                tempTestList <- buildTestList(anInputFilename=anInputFilename,
                                              noLinesToReadFromEach=noLinesToReadFromEach,
                                              minTotalLines=minTotalLines,
                                              locationToReadLines=locationToReadLines,
                                              trainLineNos = trainLineNos,
                                              testLineNos = testLineNos,
                                              testPercent <- (1-trainPercent))
                testList <- rbind(testList, tempTestList[1][[1]])
                testLineNos <- tempTestList[2][[1]]
                #testlist format
                #(Orig. String, list of words to a point, n-3 word, n-2 word, n-1 word, n/test word)
            }

            #For newly created test lists, set 'prediction1' as NA
            testList$prediction1 <- NA
            
            #Save Test List to File with test but not results
            write.csv(testList, aRunTestListFilename)
            
            #Save Test Line Numbers
            write.csv(testLineNos, aRunDataTestLineNosFilename)
        }
        
        ### START OF PREDICTING ###
        #stop("Stop before predicting")
        
        #If no prediction already, do a prediction test on each line in
        #testList and judge results. Add results to testList
        writeLines(c("",paste0("Starting tests for list ", aRunNo, " of ", nrow(runQueue))))
        predictSkipPenalty <- runQueue[aRunNo, "predictSkipPenalty"]
        removeStopWords <- runQueue[aRunNo, "removeStopWords"]
        removeWordSuffixes <- runQueue[aRunNo, "removeWordSuffixes"]
        noWordsToReturn <- runQueue[aRunNo, "noWordsToPredict"]
        lineNo <- 0
        nRowTestList <- nrow(testList)
        #print(paste("nRowTestList:", nRowTestList))
        #return("FALSE")
        lineCountPrint <- max(as.integer(nRowTestList/5), 1)
        for(aTestNo in (1:nRowTestList)) {
            #writeLines(c(paste("In 1:nRowTestList loop. Current: ")))
            #print(testList[aTestNo,,drop=FALSE])
            #writeLines(c(paste(".end.")))
            lineNo <- lineNo + 1
            if(!is.na(testList[aTestNo, "prediction1"])) next
            if((lineNo %% lineCountPrint) == 0) {
                writeLines(paste("   Predicting line", lineNo, "of", nRowTestList))
            }
            startTime <- proc.time()

            wordsToPredictBy <- testList[aTestNo, "wordsToPredictBy"]

            listOfPredictions <- predictNextWord(
                                    wordsToPredictBy = wordsToPredictBy,
                                    mCWordSpMatrix = mCWordSpMatrix,
                                    predictorWordDF= predictorWordDF,
                                    predictedWordDF= predictedWordDF,
                                    noWordsToReturn=noWordsToReturn,
                                    skipPenalty=predictSkipPenalty,
                                    removeStopWords=removeStopWords,
                                    removeWordSuffixes=removeWordSuffixes)

            count <- 1
            correctFlag <- FALSE
            for(aPredictNo in 1:nrow(listOfPredictions)) {
                aPredict <- listOfPredictions[aPredictNo, "word"]
                if(aPredict == FALSE) break    #No matches found
                thisPredictName <- paste0("prediction", as.character(count))
                thisSourceName <- paste0("source", as.character(count))
                testList[aTestNo, thisPredictName] <- aPredict
                testList[aTestNo, thisSourceName] <-
                    listOfPredictions[aPredictNo, "sourceAlgo"]
                if(testList[aTestNo, thisPredictName] == testList[aTestNo, "testWord"]) {
                    correctFlag <- TRUE
                }
                count <- count + 1
            }
            if(!is.na(testList[aTestNo, "prediction1"])) {
                testList[aTestNo, "timeToPredict"] <- (proc.time() - startTime)[1]
            } else {
                testList[aTestNo, "timeToPredict"] <- NA
            }
            
            #print(c("prediction: ", testList[aTestNo, "prediction"], "actual: ", testList[aTestNo, "testWord"]))
            
            if(correctFlag) {
                testList[aTestNo, "Correct"] <- TRUE
            } else {
                testList[aTestNo, "Correct"] <- FALSE
            }
        } 
        
        writeLines("  Finished predicting all lines in file. Writing testList and saving metrics")
        write.csv(testList, aRunTestListFilename)
        runQueue[aRunNo, "Accuracy"] <- round(sum(testList$Correct * 1) / length(testList$Correct),4)
        runQueue[aRunNo, "avgTimeToPredict"] <- round(mean(testList$timeToPredict, na.rm=TRUE),3)
        runQueue[aRunNo, "sdTimeToPredict"] <- round(sd(testList$timeToPredict, na.rm=TRUE),3)
        write.csv(runQueue, runQueueFilename)
    }
}
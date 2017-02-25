#
#
#

testPredictionsInQueue <- function(inputDataFilenames, runQueueFilename) {
    source("BuildTestList.R")
    source("PredictNextWord.R")
    library(caret)
    
    #Load Run Queue
    runQueue <- read.csv(runQueueFilename, comment.char = "#", row.names=1)
    
    for(aRunNo in 1:nrow(runQueue)) {
        writeLines(c("",paste0("Building a list for test run ", aRunNo, " of ", nrow(runQueue))))
        if(!is.na(runQueue[aRunNo, "Accuracy"])) {
            writeLines("   Results already in runQueue file. Skipping")
            next
        }
        aRunDataBaseFilename <- paste0("wordlists//wordLists", runQueue[aRunNo, "NoLinesEachFile"],
                                       "CumPer", as.integer(runQueue[aRunNo, "cumPercent"]*100))
        aRunDataWordFilename <- paste0(aRunDataBaseFilename, "word.csv")
        aRunDataPairFilename <- paste0(aRunDataBaseFilename, "pair.csv")
        aRunDataTripFilename <- paste0(aRunDataBaseFilename, "trip.csv")
        aRunDataQuadFilename <- paste0(aRunDataBaseFilename, "quad.csv")
        aRunDataTrainNosFilename <- paste0(aRunDataBaseFilename, "TrainNos.csv")
        aRunTestListFilename <- paste0(aRunDataBaseFilename, "testList.csv")
        
        if(all(file.exists(aRunDataPairFilename, aRunDataTripFilename,
                           aRunDataQuadFilename, aRunDataTrainNosFilename))) {
            #Load trained data and training line numbers from files
            pairList <- read.csv(aRunDataPairFilename)
            tripList <- read.csv(aRunDataTripFilename)
            quadList <- read.csv(aRunDataQuadFilename)
            trainLineNos <- read.csv(aRunDataTrainNosFilename)
        } else {
            writeLines(paste0("   All files for testing are not available for ",
                              aRunDataBaseFilename, "."))
            next
        }
        
        #Create test list from text data files
        testList <- data.frame(origLine = as.character(),
                               #partialLine = as.character(),
                               nMin4Word = as.character(),
                               nMin3Word = as.character(),
                               nMin2Word = as.character(),
                               nMin1Word = as.character(),
                               testWord  = as.character())
        linesToReadFromEach <- runQueue[aRunNo, "NoLinesEachFile"]
        for(anInputFilename in inputDataFilenames) {
            tempTestList <- buildTestList(anInputFilename,
                                          readLines=linesToReadFromEach,
                                          trainLineNos = trainLineNos$trainSampNos)
            testList <- rbind(testList, tempTestList)
            #testlist format
            #(Orig. String, list of words to a point, n-3 word, n-2 word, n-1 word, n/test word)
        }
        #Convert testList to character variables as they are being coerced into factors
        testList$nMin4Word <- as.character(testList$nMin4Word)
        testList$nMin3Word <- as.character(testList$nMin3Word)
        testList$nMin2Word <- as.character(testList$nMin2Word)
        testList$nMin1Word <- as.character(testList$nMin1Word)
        testList$testWord  <- as.character(testList$testWord)
        testList$origLine  <- as.character(testList$origLine)
                
        #Do a prediction test on each line in testList and judge results. Add results to testList
        writeLines(c("",paste0("Starting tests for list ", aRunNo, " of ", nrow(runQueue))))
        lineNo <- 0
        nRowTestList <- nrow(testList)
        lineCountPrint <- max(as.integer(nRowTestList <- nrow(testList) /5), 1)
        for(aTestNo in (1:nRowTestList)) {
            lineNo <- lineNo + 1
            if((lineNo %% lineCountPrint) == 0) {
                writeLines(paste("   Predicting line", lineNo, "of", nRowTestList))
            }
            startTime <- proc.time()
            #writeLines(c(paste("tWP 1:", testList[aTestNo, "nMin4Word"], testList[aTestNo, "nMin3Word"], testList[aTestNo, "nMin2Word"], testList[aTestNo, "nMin1Word"],
            #                   aTestNo, "of", nrow(testList))))
            if(is.na(testList[aTestNo, "nMin2Word"])) {
                #writeLines(c(paste("tWP 1.11:", testList[aTestNo, "nMin1Word"])))
                newWordList <- c(testList[aTestNo, "nMin1Word"])
                #writeLines(c(paste("tWP 1.12:", testList[aTestNo, "nMin1Word"])))
                #writeLines(c(paste("tWP 1.13: newWordList:", newWordList)))
            } else {
                if(is.na(testList[aTestNo, "nMin3Word"])) {
                    #writeLines(c(paste("tWP 1.21:", testList[aTestNo, "nMin2Word"], testList[aTestNo, "nMin1Word"])))
                    newWordList <- c(testList[aTestNo, "nMin2Word"],
                                     testList[aTestNo, "nMin1Word"])
                    #writeLines(c(paste("tWP 1.22:", testList[aTestNo, "nMin2Word"], testList[aTestNo, "nMin1Word"])))
                    #writeLines(c(paste("tWP 1.23: newWordList:", newWordList)))
                    } else {
                    if(is.na(testList[aTestNo, "nMin4Word"])) {
                        #writeLines(c(paste("tWP 1.31:", testList[aTestNo, "nMin3Word"], testList[aTestNo, "nMin2Word"], testList[aTestNo, "nMin1Word"])))
                        newWordList <- c(testList[aTestNo, "nMin3Word"],
                                         testList[aTestNo, "nMin2Word"],
                                         testList[aTestNo, "nMin1Word"])
                        #writeLines(c(paste("tWP 1.32:", testList[aTestNo, "nMin3Word"], testList[aTestNo, "nMin2Word"], testList[aTestNo, "nMin1Word"])))
                        #writeLines(c(paste("tWP 1.33: newWordList:", newWordList)))
                    } else {
                        newWordList <- c(testList[aTestNo, "nMin4Word"],
                                         testList[aTestNo, "nMin3Word"],
                                         testList[aTestNo, "nMin2Word"],
                                         testList[aTestNo, "nMin1Word"])
                        #writeLines(c(paste("tWP 1.42:", testList[aTestNo, "nMin4Word"], testList[aTestNo, "nMin3Word"], testList[aTestNo, "nMin2Word"], testList[aTestNo, "nMin1Word"])))
                        #print(testList)
                        #writeLines(c(paste("tWP 1.43: newWordList:", newWordList)))
                    }
                }
            }
            #writeLines("tWP 2: newWordList: ")
            #print(newWordList)
            noWordsToReturn <- 3
            tempListOfPredictions <- as.character(predictNextWord(newWordList = newWordList,
                                                                  wordList = NA,
                                                                  pairList = pairList,
                                                                  tripList = tripList,
                                                                  quadList = quadList,
                                                                  noWordsToReturn))
            count <- 1
            correctFlag <- FALSE
            for(aPredict in tempListOfPredictions) {
                thisPredictName <- paste0("prediction", as.character(count))
                testList[aTestNo, thisPredictName] <- aPredict
                if(testList[aTestNo, thisPredictName] == testList[aTestNo, "testWord"]) {
                    correctFlag <- TRUE
                }
                count <- count + 1
            }
            testList[aTestNo, "timeToPredict"] <- (proc.time() - startTime)[1]
            #print(c("prediction: ", testList[aTestNo, "prediction"], "actual: ", testList[aTestNo, "testWord"]))
            
            if(correctFlag) {
                testList[aTestNo, "Correct"] <- TRUE
            } else {
                testList[aTestNo, "Correct"] <- FALSE
            }
        }
        write.csv(testList, aRunTestListFilename)
        runQueue[aRunNo, "Accuracy"] <- sum(testList$Correct * 1) / length(testList$Correct)
        runQueue[aRunNo, "avgTimeToPredict"] <- mean(testList$timeToPredict)
        runQueue[aRunNo, "sdTimeToPredict"] <- sd(testList$timeToPredict)
    }
    write.csv(runQueue, runQueueFilename)
}
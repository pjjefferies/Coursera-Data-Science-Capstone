#
#
#


generateMarkovChains <- function(inputDataFilenames, runQueueFilename) {
    # General format of inputs
    #inputDataFilenames <- c("en_US.blogs.txt",
    #                        "en_US.news.txt",
    #                        "en_US.twitter.txt")
    #runQueueFilename <- "NextWordPredictionResults.csv"
    #
    source("BuildMarkovChainWordSpMatrix.R")
    
    runQueue <- read.csv(runQueueFilename, comment.char = "#", row.names=1)
    
    for(aRunNo in 1:nrow(runQueue)) {
        writeLines(c("",paste0("Starting build run ", aRunNo, " of ",
                               nrow(runQueue))))
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
                                                                 "trainSkipPenalty"]),1))
        aRunDataMCSpFilename <- paste0(aRunDataBaseFilename, "SpMC.txt")
        predictorWordDFFilename <- paste0(aRunDataBaseFilename, "SpORWL.csv")
        predictedWordDFFilename <- paste0(aRunDataBaseFilename, "SpEDWL.csv")
        aRunDataTrainNosFilename <- paste0(aRunDataBaseFilename, "TrainNos.csv")
        
        fileNoToLoad <- as.integer(runQueue[aRunNo, "filesToLoad"])
        if(fileNoToLoad < 1 | fileNoToLoad > 7) {
            fileNoToLoad <- 7
            runQueue[aRunNo, "filesToLoad"] <- 7
        }
        inputDataFilenamesToUse <- c()
        if(fileNoToLoad >= 4) {
            inputDataFilenamesToUse <- append(inputDataFilenamesToUse,
                                              inputDataFilenames[3])
            fileNoToLoad <- fileNoToLoad - 4
        }
        if(fileNoToLoad >= 2) {
            inputDataFilenamesToUse <- append(inputDataFilenamesToUse,
                                              inputDataFilenames[2])
            fileNoToLoad <- fileNoToLoad - 2
        }
        if(fileNoToLoad == 1) {
            inputDataFilenamesToUse <- append(inputDataFilenamesToUse,
                                              inputDataFilenames[1])
            fileNoToLoad <- fileNoToLoad - 1
        }
        if(fileNoToLoad != 0) {
            writeLines(c("We have a problem.", fileNoToLoad))
            return(FALSE)
        }
        if(all(file.exists(aRunDataMCSpFilename,
                           predictorWordDFFilename,
                           predictedWordDFFilename,
                           aRunDataTrainNosFilename))) {
            #Load data from files if using, otherwise, skip
            writeLines("  Markov Chain File exists, go to next in queue")
        } else {
            #generate files and save
            startTime <- proc.time()
            #linesToReadFromEach can be a number if > 1 or a fraction of total lines
            #if <= 1. This will be intrepreted in buildMCWSM function
            noLinesToReadFromEach <- runQueue[aRunNo, "NoLinesEachFileOrFraction"]
            #locationToReadLines shoud be "top" or "random"
            locationToReadLines <- runQueue[aRunNo, "LocToReadLines"]
            if(!(locationToReadLines %in% c("top", "random"))) {
                writeLines("   Invalid read lines location in queue. Skipping run.")
                next
            }
            keepPercent <- as.numeric(runQueue[aRunNo, "cumPercent"])
            trainPercent <- runQueue[aRunNo, "trainPercent"]
            trainSkipPenalty <- runQueue[aRunNo, "trainSkipPenalty"]
            removeStopWords <- runQueue[aRunNo, "removeStopWords"]
            removeWordSuffixes <- runQueue[aRunNo, "removeWordSuffixes"]
            ###
            #markovChainWordList <- data.frame()
            ###
            #trainLineNos <- sort(sample(linesToReadFromEach,
            #                            as.integer(linesToReadFromEach*trainPercent)))
            mCWordSMWLTemp <- buildMarkovChainWordSpMatrix(inputDataFilenamesToUse,
                                         noLinesToReadFromEach=noLinesToReadFromEach,
                                         trainSkipPenalty=trainSkipPenalty,
                                         locationToReadLines=locationToReadLines,
                                         trainPercent=trainPercent,
                                         #trainLineNos = trainLineNos,
                                         maxCumFreq = keepPercent,
                                         removeStopWords=removeStopWords,
                                         removeWordSuffixes=removeWordSuffixes)
            mCWordSpMatrix <- mCWordSMWLTemp[1][[1]]
            predictorWordDF<- mCWordSMWLTemp[2][[1]]
            predictedWordDF <- mCWordSMWLTemp[3][[1]]
            trainLineNos <- mCWordSMWLTemp[4][[1]]

            writeLines("    Saving Markov Matrix, word lists and training line numbers")
            writeMM(mCWordSpMatrix, aRunDataMCSpFilename)
            write.csv(predictorWordDF, predictorWordDFFilename)
            write.csv(predictedWordDF, predictedWordDFFilename)
            write.csv(trainLineNos, aRunDataTrainNosFilename)
            writeLines(paste("    Finished saving data"))
            
            runQueue[aRunNo, "timeToTrain"] <- as.integer((proc.time() - startTime)[1], 1)
            runQueue[aRunNo, "sizeOfTrainDB"] <- object.size(mCWordSpMatrix) +
                                                 object.size(predictorWordDF) +
                                                 object.size(predictedWordDF)
            runQueue[aRunNo, "NoTrainingFileLines"] <- length(trainLineNos) *
                                                       length(inputDataFilenames)
            runQueue[aRunNo, "noPredictorWordsTotal"] <- nrow(predictorWordDF)
            runQueue[aRunNo, "noPredictedWordsTotal"] <- nrow(predictedWordDF)
            
            #Write updated runQueue file each run iteration in case of problems
            write.csv(runQueue, runQueueFilename)
        }
    }
    writeLines(paste("Successfully created wordfiles in queue and updated queue file"))
    return(TRUE)
}


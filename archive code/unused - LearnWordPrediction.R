#
#
#

generateWordLists <- function(inputDataFilenames, runQueueFilename) {
    # General format of inputs
    #inputDataFilenames <- c("en_US.blogs.txt",
    #                        "en_US.news.txt",
    #                        "en_US.twitter.txt")
    #runQueueFilename <- "NextWordPredictionResults.csv"
    #
    source("buildWordLists.R")
    
    runQueue <- read.csv(runQueueFilename, comment.char = "#", row.names=1)
    
    for(aRunNo in 1:nrow(runQueue)) {
        writeLines(c("",paste0("Starting build run ", aRunNo, " of ", nrow(runQueue))))
        aRunDataBaseFilename <- paste0("wordlists//wordLists", runQueue[aRunNo, "NoLinesEachFile"],
                                       "CumPer", as.integer(runQueue[aRunNo, "cumPercent"]*100))
        aRunDataWordFilename <- paste0(aRunDataBaseFilename, "word.csv")
        aRunDataPairFilename <- paste0(aRunDataBaseFilename, "pair.csv")
        aRunDataTripFilename <- paste0(aRunDataBaseFilename, "trip.csv")
        aRunDataQuadFilename <- paste0(aRunDataBaseFilename, "quad.csv")
        aRunDataTrainNosFilename <- paste0(aRunDataBaseFilename, "TrainNos.csv")
        
        if(file.exists(aRunDataPairFilename) & file.exists(aRunDataTripFilename) &
           file.exists(aRunDataQuadFilename) & file.exists(aRunDataTrainNosFilename)) {
            #Load data from files if using, otherwise, skip
            writeLines("Word Files exists, go to next in queue")
            #pairList <- read.csv(aRunDataPairFilename)
            #tripList <- read.csv(aRunDataTripFilename)
            #quadList <- read.csv(aRunDataQuadFilename)
            
        } else {
            #generate files and save
            startTime <- proc.time()
            linesToReadFromEach <- runQueue[aRunNo, "NoLinesEachFile"]
            keepPercent <- runQueue[aRunNo, "cumPercent"]
            trainPercent <- runQueue[aRunNo, "trainPercent"]
            wordList <- data.frame(count=as.integer(),
                                   basis=as.character(),
                                   prediction=as.character(),
                                   word=as.character(),
                                   rowCount=as.integer(),
                                   freq=as.numeric(),
                                   cumFreq=as.numeric())
            pairList <- wordList
            tripList <- wordList
            quadList <- wordList
            trainLineNos <- data.frame(trainSampNos = sort(sample(linesToReadFromEach,
                                        as.integer(linesToReadFromEach*0.6))))
            for(anInputFilename in inputDataFilenames) {
                tempLists <- buildWordLists(anInputFilename,
                                            readLines=linesToReadFromEach,
                                            colWord = FALSE, colPair = TRUE,
                                            colTrip = TRUE, colQuad = TRUE,
                                            addFreq=TRUE, addCumFreq=TRUE,
                                            removeSingleCount = FALSE,
                                            removeAboveCumPercent = keepPercent,
                                            trainLineNos = trainLineNos$trainSampNos)
                pairList <- rbind(pairList, tempLists$pairList)
                tripList <- rbind(tripList, tempLists$tripList)
                quadList <- rbind(quadList, tempLists$quadList)
            }
            write.csv(pairList, aRunDataPairFilename)
            write.csv(tripList, aRunDataTripFilename)
            write.csv(quadList, aRunDataQuadFilename)
            write.csv(trainLineNos, aRunDataTrainNosFilename)
            
            runQueue[aRunNo, "timeToTrain"] <- (proc.time() - startTime)[1]
            runQueue[aRunNo, "sizeOfTrainDB"] <- object.size(pairList) +
                object.size(tripList) +
                object.size(quadList)
            runQueue[aRunNo, "NoTrainingFileLines"] <- nrow(trainLineNos) *
                                                       length(inputDataFilenames)
            runQueue[aRunNo, "pairsKnown"] <- nrow(pairList)
            runQueue[aRunNo, "tripsKnown"] <- nrow(tripList)
            runQueue[aRunNo, "quadsKnown"] <- nrow(quadList)
            
        }
        
    }
    write.csv(runQueue, runQueueFilename)
    return("Successfully created wordfiles in queue and updated queue file")
}


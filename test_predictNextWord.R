#
#
#
debugSource("PredictNextWord.R")
source("PredictNextWord.R")


wordsToPredictBy <- "Now is the time for all good"

noLinesEachFileOrFraction <- "100"
LocToReadLines <- "top"
cumPercent <- "1"
filesToLoad <- "7"
trainSkipPenalty <- 2
aRunDataBaseFilename <- paste0("MarkovChains//markovChain",
                               noLinesEachFileOrFraction,
                               LocToReadLines,
                               "cumPer",
                               as.integer(as.numeric(cumPercent)*100),
                               "wFNo", filesToLoad,
                               "TSP", round(as.numeric(trainSkipPenalty),1))
aRunDataMCSpFilename <- paste0(aRunDataBaseFilename, "SpMC.txt")
predictorWordDFFilename <- paste0(aRunDataBaseFilename, "SpORWL.csv")
predictedWordDFFilename <- paste0(aRunDataBaseFilename, "SpEDWL.csv")

mCWordSpMatrix <- readMM(aRunDataMCSpFilename)
predictorWordDF <- read.csv(predictorWordDFFilename,
                            comment.char="#", row.names=1, as.is=TRUE)
predictedWordDF <- read.csv(predictedWordDFFilename,
                            comment.char="#", row.names=1, as.is=TRUE)

noWordsToReturn <- 5
skipPenalty <- 2
removeStopWords <- TRUE
removeWordSuffixes <- TRUE

nextWordDF <- predictNextWord(wordsToPredictBy,
                              mCWordSpMatrix,
                              predictorWordDF,
                              predictedWordDF,
                              noWordsToReturn,
                              skipPenalty,
                              removeStopWords,
                              removeWordSuffixes)

print(nextWordDF)
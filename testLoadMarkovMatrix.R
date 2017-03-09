#
# Test program for loading Markov Matrix
#


aRunDataBaseFilename <- paste0("MarkovChains//markovChain",
                               1000,
                               "random",
                               "cumPer", 100,
                               "wFNo", 7,
                               "TSP", 2,
                               "RSW", "F",
                               "RSX", "F")

#aRunDataBaseFilename <- "MarkovChains//markovChain1000randomcumPer100wFNo7TSP2RSWFRSXF"
aRunDataMCSpFilename <- paste0(aRunDataBaseFilename,
                               "SpMC.txt")
predictorWordDFFilename <- paste0(aRunDataBaseFilename, "SpORWL.csv")
predictedWordDFFilename <- paste0(aRunDataBaseFilename, "SpEDWL.csv")


mCWordSpMatrix <- readMM(aRunDataMCSpFilename)
predictorWordDF <- read.csv(predictorWordDFFilename,
                            comment.char="#", row.names=1, as.is=TRUE)
predictedWordDF <- read.csv(predictedWordDFFilename,
                            comment.char="#", row.names=1, as.is=TRUE)


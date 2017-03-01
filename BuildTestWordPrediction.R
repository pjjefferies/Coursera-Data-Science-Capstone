#
source("GenerateMarkovChains.R")
#source("TestWordPrediction.R")
debugSource("TestWordPrediction.R")

set.seed(123456)
inputDataFilenames <- c("en_US.blogs.txt",
                        "en_US.news.txt",
                        "en_US.twitter.txt")
runQueueFilename <- "runQueueResults.csv"

result <- generateMarkovChains(inputDataFilenames, runQueueFilename)
#writeLines(c("", result, ""))




caseSummary <- testWordPrediction(inputDataFilenames, runQueueFilename)

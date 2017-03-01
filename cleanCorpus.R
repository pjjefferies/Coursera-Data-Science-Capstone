#
#A function to conditionally clean a text Corpus
#


CleanCorpus <- function(data,
                        removeEmail=TRUE,
                        removeURL=TRUE,
                        removeHandles=TRUE,
                        removeHashtags=TRUE,
                        removeStopWords,
                        appSpecWordsFile=FALSE,
                        removeWordSuffixes,
                        myBadWordsFile="Terms-to-Block.csv",
                        convertPlainText=TRUE) {
    library(tm)
    library(SnowballC)
    data <- tm_map(data, content_transformer(tolower))
    if(removeEmail) data <- tm_map(data,
                                   function(x) {gsub("\\S+@\\S+", "", x)})
    if(removeURL)   data <- tm_map(data,
                                   function(x) {gsub("http[[:alnum:]]*",
                                                     "", x)})
    if(removeHashtags) data <- tm_map(data,
                                      function(x) {gsub("#[[:alnum:]]*",
                                                        "", x)})
    if(removeHandles) data <- tm_map(data,
                                     function(x) {gsub("@[[:alnum:]]*",
                                                       "", x)})

    if(removeStopWords == TRUE) {
        data <- tm_map(data, removeWords, stopwords("english"))
    }

    if(appSpecWordsFile != FALSE) {
        con <- file(appSpecWordsFile, "r")
        appSpecWordsToRemove <- readLines(con, warn=FALSE, skipNul=TRUE)
        close(con)
        data <- tm_map(data, removeWords, appSpecWordsToRemove)
    }
    
    if(myBadWordsFile != FALSE) {
        badWordsToBlock <- as.character(read.csv(myBadWordsFile, skip=3)[,1])
        data <- tm_map(data, removeWords, badWordsToBlock)
    }
    
    
    if(removeWordSuffixes) {
        data <- tm_map(data, stemDocument)
    }
    
    data <- tm_map(data, removePunctuation)
    data <- tm_map(data, removeNumbers)
    data <- tm_map(data, stripWhitespace)
    data <- tm_map(data, trimws)
    
    if(convertPlainText) {
        data <- tm_map(data, PlainTextDocument)
    }
    return(data)
}
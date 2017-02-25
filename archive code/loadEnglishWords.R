#Load list of English words from https://github.com/dwyl/english-words
#englishWordsFilename <- "words.txt"
#englishWords <- readLines(englishWordsFilename)

#Find all words in twitter word list that are not in English word list
#nonEnglishWords <- c()
#for(aWord in twitterWordList$word) {
#    if(!(aWord %in% englishWords)) {
#        nonEnglishWords <- append(nonEnglishWords, aWord)
#    }
#}
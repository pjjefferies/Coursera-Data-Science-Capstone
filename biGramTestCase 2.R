library("RWeka")
library("tm")

#data("crude")

someCleanText <- c("Congress shall make no law respecting an establishment of",
                   "religion, or prohibiting the free exercise thereof or",
                   "abridging the freedom of speech or of the press or the",
                   "right of the people peaceably to assemble and to petition",
                   "the Government for a redress of grievances")

aCorpus <- VCorpus(VectorSource(someCleanText))

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(aCorpus, control = list(tokenize = BigramTokenizer))

inspect(tdm)
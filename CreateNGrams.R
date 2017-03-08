#
#  Functions to create N-Grams
#

library(RWeka)

xgram <- function(aCorpus, grams) {
    xGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=grams, max=grams))
    tdm <- TermDocumentMatrix(aCorpus, control = list(tokenize=xGramTokenizer,
                                                      wordLengths=c(2,20)))
    termNos <- tdm$i
    docNos <- tdm$j
    termDocFreq <- tdm$v
    tempSpMatrix <- Matrix(data=0, nrow=nrow(tdm), ncol=ncol(tdm),
                           sparse=TRUE, doDiag=FALSE)
    for(aTermSeqNo in 1:length(termNos)) {
        tempSpMatrix[termNos[aTermSeqNo],
                     docNos[aTermSeqNo]] <- termDocFreq[aTermSeqNo]
    }
    #freq <- rowSums(tempSpMatrix)
    ngram <- data.frame(ngrams = as.character(tdm$dimnames$Terms),
                        freq = rowSums(tempSpMatrix),
                        stringsAsFactors = FALSE)
    ngram <- ngram[order(ngram$freq, decreasing = TRUE), ,drop=FALSE]
    row.names(ngram) <- NULL
    return(ngram)
}

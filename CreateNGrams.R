#
#  Functions to create N-Grams
#

library(RWeka)

uniGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
biGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
triGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
quadGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
quintGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=5, max=5))

unigram <- function(x) {
    tdm <- TermDocumentMatrix(x, control = list(tokenize=uniGramTokenizer,
                                                wordLengths=c(2,20)))
    #tdm <- removeSparseTerms(tdm, 0.9995)
    termNos <- tdm$i
    docNos <- tdm$j
    termDocFreq <- tdm$v
    tempSpMatrix <- Matrix(data=0, nrow=nrow(tdm), ncol=ncol(tdm),
                           sparse=TRUE, doDiag=FALSE)
    for(aTermSeqNo in 1:length(termNos)) {
        tempSpMatrix[termNos[aTermSeqNo],
                     docNos[aTermSeqNo]] <- termDocFreq[aTermSeqNo]
    }
    #freq <- rowSums(as.matrix(tdm))
    freq <- rowSums(tempSpMatrix)
    ngram <- data.frame(ngrams = as.character(tdm$dimnames$Terms),
                        freq = freq)
    ngram <- ngram[order(ngram$freq, decreasing = TRUE),]
    row.names(ngram) <- NULL
    return(ngram)
}

bigram <- function(x) {
    tdm <- TermDocumentMatrix(x, control = list(tokenize=biGramTokenizer,
                                                wordLengths=c(2,20)))
    #tdm <- removeSparseTerms(tdm, 0.9995)
    termNos <- tdm$i
    docNos <- tdm$j
    termDocFreq <- tdm$v
    tempSpMatrix <- Matrix(data=0, nrow=nrow(tdm), ncol=ncol(tdm),
                           sparse=TRUE, doDiag=FALSE)
    for(aTermSeqNo in 1:length(termNos)) {
        tempSpMatrix[termNos[aTermSeqNo],
                     docNos[aTermSeqNo]] <- termDocFreq[aTermSeqNo]
    }
    #freq <- rowSums(as.matrix(tdm))
    freq <- rowSums(tempSpMatrix)
    ngram <- data.frame(ngrams = as.character(tdm$dimnames$Terms),
                        freq = freq)
    ngram <- ngram[order(ngram$freq, decreasing = TRUE),]
    row.names(ngram) <- NULL
    return(ngram)
}

trigram <- function(x) {
    tdm <- TermDocumentMatrix(x, control = list(tokenize=triGramTokenizer,
                                                wordLengths=c(2,20)))
    #tdm <- removeSparseTerms(tdm, 0.9995)
    termNos <- tdm$i
    docNos <- tdm$j
    termDocFreq <- tdm$v
    tempSpMatrix <- Matrix(data=0, nrow=nrow(tdm), ncol=ncol(tdm),
                           sparse=TRUE, doDiag=FALSE)
    for(aTermSeqNo in 1:length(termNos)) {
        tempSpMatrix[termNos[aTermSeqNo],
                     docNos[aTermSeqNo]] <- termDocFreq[aTermSeqNo]
    }
    #freq <- rowSums(as.matrix(tdm))
    freq <- rowSums(tempSpMatrix)
    ngram <- data.frame(ngrams = as.character(tdm$dimnames$Terms),
                        freq = freq)
    ngram <- ngram[order(ngram$freq, decreasing = TRUE),]
    row.names(ngram) <- NULL
    return(ngram)
}

quadgram <- function(x) {
    tdm <- TermDocumentMatrix(x, control = list(tokenize=quadGramTokenizer,
                                                wordLengths=c(2,20)))
    #tdm <- removeSparseTerms(tdm, 0.9995)
    termNos <- tdm$i
    docNos <- tdm$j
    termDocFreq <- tdm$v
    tempSpMatrix <- Matrix(data=0, nrow=nrow(tdm), ncol=ncol(tdm),
                           sparse=TRUE, doDiag=FALSE)
    for(aTermSeqNo in 1:length(termNos)) {
        tempSpMatrix[termNos[aTermSeqNo],
                     docNos[aTermSeqNo]] <- termDocFreq[aTermSeqNo]
    }
    #freq <- rowSums(as.matrix(tdm))
    freq <- rowSums(tempSpMatrix)
    ngram <- data.frame(ngrams = as.character(tdm$dimnames$Terms),
                        freq = freq)
    ngram <- ngram[order(ngram$freq, decreasing = TRUE),]
    row.names(ngram) <- NULL
    return(ngram)
}

quintgram <- function(x) {
    tdm <- TermDocumentMatrix(x, control = list(tokenize=quintGramTokenizer,
                                                wordLengths=c(2,20)))
    #tdm <- removeSparseTerms(tdm, 0.9995)
    termNos <- tdm$i
    docNos <- tdm$j
    termDocFreq <- tdm$v
    tempSpMatrix <- Matrix(data=0, nrow=nrow(tdm), ncol=ncol(tdm),
                           sparse=TRUE, doDiag=FALSE)
    for(aTermSeqNo in 1:length(termNos)) {
        tempSpMatrix[termNos[aTermSeqNo],
                     docNos[aTermSeqNo]] <- termDocFreq[aTermSeqNo]
    }
    #freq <- rowSums(as.matrix(tdm))
    freq <- rowSums(tempSpMatrix)
    ngram <- data.frame(ngrams = as.character(tdm$dimnames$Terms),
                        freq = freq)
    ngram <- ngram[order(ngram$freq, decreasing = TRUE),]
    row.names(ngram) <- NULL
    return(ngram)
}
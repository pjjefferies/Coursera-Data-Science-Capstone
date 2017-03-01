#
#
#
library(Matrix)

addToPredictionDF <- function(predictorWords,
							  mCWordSpMatrix,
							  predictorWordDF,
							  predictedWordDF,
							  noWordsToReturn) {
    predictorRowNo <- match(predictorWords,
                      predictorWordDF$word)
    predictRow <- mCWordSpMatrix[predictorRowNo, , drop=FALSE]
    
    #to prevent sequencing problem when collapsing sparse matrix
    predictRow <- as.matrix(predictRow)
    
    #add seq to keep track of orig. columns
    predictRow <- rbind(predictRow, seq(1:ncol(predictRow)))
    
    #In predictRow:
    #         #    Row 1: Number of Observations in Training
    #         #    Row 2: Sequence of Word being predicted
    
    #Filter-out all non-zero word columns
    predictRow <- predictRow[, predictRow[1,]>0, drop=FALSE]
    
    predictionDF <- data.frame(prediction=as.character(),
                               power=as.numeric(),
                               stringsAsFactors = FALSE)
    
    #This should alwasy be true since predictor was is predictor list
    if(ncol(predictRow) > 0) {
        totalPredictorObs <- sum(predictRow[1,])
        predictionDF <- rbind(predictionDF,
                              data.frame(predictedWordNo=predictRow[2,],
                                         power=(predictRow[1,] / totalPredictorObs),
                                         stringsAsFactors = FALSE))
        #sort resulting prediction by power
        predictionDF <- predictionDF[order(predictionDF$power, decreasing=TRUE),,drop=FALSE]
        #only save top number of predictions requested
        lengthToKeep <- min(nrow(predictionDF), noWordsToReturn)
        predictionDF <- predictionDF[seq(1:lengthToKeep), , drop=FALSE]
        predictionDF$word <- predictedWordDF[predictionDF$predictedWordNo, "word"]
        predictionDF <- predictionDF[,c("word", "power"), drop=FALSE]
        #return(predictionDF)
    }

}
#' @export
sampleTruth <- function(longTruth, stepRate, nTrain, nTest, seed) {

  longTime <- longTruth[, 1]
  longX <- longTruth[, -1]
  totalSysLen <- stepRate*(nTrain + nTest)
  lastValidStartIndex <- nrow(longTruth) - totalSysLen
  startIndex <-
    withr::with_seed(
      seed,
      sample.int(lastValidStartIndex, 1)
    )
  trainIndices <- seq(startIndex, length.out=nTrain, by=stepRate)
  testIndices <- seq(startIndex+stepRate*nTrain, length.out=nTest, by=stepRate)
  longT0 <- longTime[trainIndices[nTrain]]

  list(
    longTrainIndices = trainIndices,
    longTestIndices = testIndices,
    xTrain = longX[trainIndices, ],
    xTest = longX[testIndices, ],
    tTrain = longTime[trainIndices] - longT0,
    tTest = longTime[testIndices] - longT0
  )
}

#' @export
sampleNTruthX <- function(longTruth, stepRate, len, nReps, seed) {
  limit <- nrow(longTruth) - ((len+1)*stepRate)
  startIdxs <-  withr::with_seed(seed, sample.int(limit, nReps))
  vapply(
    startIdxs,
    \(startIdx) longTruth[seq(startIdx, length.out=len, by=stepRate), -1],
    FUN.VALUE = matrix(double(), nrow=len, ncol=ncol(longTruth)-1)
  )
}

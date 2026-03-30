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
  testIndices <- seq(startIndex+stepRate*nTrain, length.out=nTrain, by=stepRate)
  longT0 <- longTime[trainIndices[nTrain]]

  list(
    longTrainIndices = trainIndices,
    longTestIndices = testIndices,
    xTrain = longX[trainIndices, ],
    xTest = longX[testIndices, ],
    xTrain = longTime[trainIndices] - longT0,
    tTest = longTime[testIndices] - longT0
  )
}

options(tidyverse.quiet = TRUE)
library(tidyverse)
source("dev/common_paths.R")


# default parameters
randomSeeds <- 1:2
truthName <- "L63_default"
noiseName <- "Gauss"
noiseScales <- c(0, 1e-1)
testDuration <- 20
nInSample <- 100
nOutOfSample <- 100
nLong <- 1e5
oosReps <- 100
nTrain <- 2^12
stepRate <- 2^2
methodStrings <- c(
  sprintf("RK4: systemName='L63';timeStep=2^%d", -10+log2(stepRate)),
  "PolyProp: nDeg=5",
  "EnKF: nDeg=2;coefUpdate='coupled'",
  "EchoBoost: nDeg=2;adjustResponse=FALSE",
  NULL
)
label <- NULL

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))


# set derived values

truth <- readInfoAndData(truthDirPath, truthName)

if (is.null(label)) label <- paste0(truthName, "_", noiseName)
outName <- sprintf("%s_%s_%03d", label, str_extract(methodStrings[1], "^[^:]+"), randomSeeds[1])
outDirPath <- file.path(forecastDirPath, label)

nTest <- testDuration / truth$info$timeStep / stepRate
truthMean <- colMeans(truth$data[, -1])
truthSd <- sqrt(sum(colMeans((truth$data[, -1] - rep(truthMean, each=nrow(truth$data)))^2)))



info <- lst(
  args,
  randomSeeds,
  truthName,
  noiseName,
  noiseScales,
  testDuration,
  nTrain,
  stepRate,
  methodStrings,
  label,
  outName
)

cat("Run forecast with following info:\n")
printInfo(info)

results <- list()

for (parentSeed in randomSeeds) {

  cat(sprintf("randomSeed: %d...\n", parentSeed))
  set.seed(parentSeed)
  seeds <- sample.int(.Machine$integer.max, 8L)
  names(seeds) <- c("truth", "oos", "noise", "fit", "assimilate", "predict", "longStart", "longCompare")

  truthSample <- sampleTruth(truth$data, stepRate, nTrain, nTest, seed=seeds["truth"])

  xOos <- sampleNTruthX(truth$data, stepRate, nOutOfSample+1, oosReps, seeds["oos"])

  xTrainTruth <- truthSample$xTrain
  xTest <- truthSample$xTest
  tTest <- truthSample$tTest
  tTrain <- truthSample$tTrain

  for (noiseScale in noiseScales) {

    cat(sprintf("\tnoiseScale: %g...\n", noiseScale))
    noiseSd <- noiseScale * truthSd
    xTrainTrain <- observe(xTrainTruth, noiseSd, type=noiseName, seed=seeds["noise"])

    for (methodString in methodStrings) {

      cat(sprintf("\t\tmethod: %s...", methodString))
      pt <- proc.time()

      fitResult <- fitMethod(methodString, xTrainTrain, seeds["fit"])
      model <- fitResult$model
      methodName <- fitResult$methodName


      # Assimilation Error
      assimilateResult <- assimilateMethod(methodName, model, xTrainTrain, seeds["assimilate"])
      assimilation <- drop(assimilateResult$assimilation)
      stopifnot(nrow(assimilation) == nTrain)
      assimilationErr <- sqrt(rowSums((xTrainTruth - assimilation)^2))
      assimilationErrTbl <- bind_cols(time = tTrain, error = assimilationErr)

      # Forecast Error
      initialCondAssi <- assimilation[nTrain, , drop=FALSE]
      predictDirectAssiResult <- predictMethod(methodName, model, initialCondAssi, nTest, seeds["predict"])
      forcastAssiErr <- sqrt(rowSums((xTest - drop(predictDirectAssiResult$prediction))^2))
      initialCondTrue <- xTrainTruth[nrow(xTrainTruth), , drop=FALSE]
      predictDirectTrueResult <- predictMethod(methodName, model, initialCondTrue, nTest, seeds["predict"])
      forcastTrueErr <- sqrt(rowSums((xTest - drop(predictDirectTrueResult$prediction))^2))
      forcastErrTbl <- bind_cols(time = tTest, assiErr = forcastAssiErr, trueErr = forcastTrueErr)

      # Assimilation Prediction Consistency
      startIdxes <- 1:(nTrain-nInSample)
      initialCondIs <- assimilation[startIdxes, , drop=FALSE]
      predictInSampleResult <- predictMethod(methodName, model, initialCondIs, nInSample, seeds["predict"])
      sqrErrTrajs <- sapply(startIdxes, \(i) {
        pred <- predictInSampleResult$prediction[,,i]
        target <- assimilation[(i+1):(i+nInSample), ]
        rowSums((pred - target)^2)
      })
      consistencyRmse <- sqrt(rowMeans(sqrErrTrajs))

      # Out-of-sample Prediction Error
      predictOosResult <- predictMethod(methodName, model, t(xOos[1,,]), nOutOfSample, seeds["predict"])
      diff <- (predictOosResult$prediction - xOos[-1, , ])^2
      oosRmse <- sqrt(rowMeans(apply(diff, 3, rowSums)))

      # long term
      longStart <-
        withr::with_seed(
          seeds["longStart"],
          truth$data[sample.int(nrow(truth$data), 1), -1, drop=FALSE]
        )
      predictLongResult <- predictMethod(methodName, model, longStart, nLong, seeds["predict"])
      ptLongCompare <- proc.time()
      longTermStats <-
        withr::with_seed(
          seeds["longCompare"],
          compareLong(
            query = predictLongResult$prediction[,,1],
            target = truth$data[seq(1, nrow(truth$data), by=stepRate), -1]
          )
        )
      cat(sprintf(" (compareLong: %.2fs) ", (proc.time()-ptLongCompare)["elapsed"]))

      measuredResults <- list(
        fit = fitResult,
        assimilate = assimilateResult,
        predictDirectTrue = predictDirectTrueResult,
        predictDirectAssi = predictDirectAssiResult,
        predictInSample = predictInSampleResult,
        predictOos = predictOosResult,
        predictLong = predictLongResult
      )
      time <- as_tibble_row(lapply(measuredResults, \(x) x$time))
      mem <- as_tibble_row(lapply(measuredResults, \(x) x$mem))

      result <- tibble(
        parentSeed,
        seeds = as_tibble_row(seeds),
        noiseScale,
        methodString,
        methodName,
        time = list(time),
        mem = list(mem),
        forecastError = list(forcastErrTbl),
        assimilationError = list(assimilationErrTbl),
        consistencyRmse = list(consistencyRmse),
        oosRmse = list(oosRmse),
        lyapunov = longTermStats$lyapunov,
        wasserstein = longTermStats$wasserstein,
        correlationDimension = longTermStats$correlationDimension,
        autocorrelation = list(longTermStats$autocorrelation)
      )

      results <- append(results, list(result))

      cat(sprintf("took %.2fs.\n", (proc.time()-pt)[3]))
    }
  }
}

resultTbl <- bind_rows(results)

writeInfoAndData(info, resultTbl, outDirPath, paste0(outName, "_results")) |> print()

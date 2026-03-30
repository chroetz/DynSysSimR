options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_method.R")


# default parameters
randomSeeds <- 1:10
truthName <- "L63_default"
noiseName <- "Gauss"
#noiseScales <- c(0, 8^-2, 8^-1, 1, 8)
noiseScales <- c(0, 1e-2, 1e-1, 1)
testDuration <- 20
nObs <- 2^10
stepRate <- 2^5
methods <- c(
  #"PolyPropR: nDeg=3;step=1",
  #"PolyPropREnKF: nDeg=3",
  "EnKF: nDeg=2;coefUpdate='coupled';v=2",
  #"PolyPropREb: nDeg=3",
  # "FullLoss: nDeg=3;kmax=10;targetUpdateFactor=0;reltol=1e-14",
  # "FullLoss: nDeg=3;kmax=10;targetUpdateFactor=0.5;reltol=1e-14",
  # "FullLoss: nDeg=3;kmax=10;targetUpdateFactor=1;reltol=1e-14",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:02",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:04",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:08",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:16",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:32",
  # "EnKF: nDeg=2;coefUpdate='coupled'",
  # "EnKF: nDeg=2;coefUpdate='decoupled'",
  # "PolyProp: nDeg=2",
  # "EchoBoostPolyProp: nDeg=2;adjustResponse=FALSE",
  # "EchoBoostPolyProp: nDeg=2;adjustResponse=TRUE",
  # "LocalSmootherPolyProp: nDeg=2",
  NULL
)
label <- NULL

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))


# set derived values

truth <- readInfoAndData(truthDirPath, truthName)

if (is.null(label)) label <- paste0(truthName, "_", noiseName)
outName <- sprintf("%s_%s_%03d", label, str_extract(methods[1], "^[^:]+"), randomSeeds[1])
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
  nObs,
  stepRate,
  methods,
  label,
  outName
)

cat("Run forecast with following info:\n")
printInfo(info)

forecastError <-
  expand_grid(method = methods, noiseScale = noiseScales, randomSeed = randomSeeds) |>
  mutate(result = rep(list(NULL), n())) |>
  rowid_to_column("idx")

assimilationError <-
  expand_grid(method = methods, noiseScale = noiseScales, randomSeed = randomSeeds) |>
  mutate(result = rep(list(NULL), n())) |>
  rowid_to_column("idx")



for (randomSeed in randomSeeds) {

  cat(sprintf("randomSeed: %d...\n", randomSeed))
  set.seed(randomSeed)
  seeds <- sample.int(.Machine$integer.max, 3L)

  truthSample <- sampleTruth(truth$data, stepRate, nObs, nTest, seed=seeds[1])

  xTrainTruth <- truthSample$xTrain
  xTest <- truthSample$xTest
  tTest <- truthSample$tTest

  for (noiseScale in noiseScales) {

    cat(sprintf("\tnoiseScale: %g...\n", noiseScale))
    noiseSd <- noiseScale * truthSd
    xTrainObs <- observe(xTrainTruth, noiseSd, type=noiseName, seed=seeds[2])

    for (method in methods) {

      cat(sprintf("\t\tmethod: %s...", method))
      pt <- proc.time()

      methodOutput <- runMethod(
        method,
        xTrainObs,
        nPred = nTest,
        seeds[3]
      )

      if (!is.null(methodOutput$forecast)) {
        err <- sqrt(rowSums((xTest - methodOutput$forecast)^2))
        result <- bind_cols(time = tTest, error = err)
        idx <- forecastError |>
          filter(.data$noiseScale == .env$noiseScale, .data$method == .env$method, .data$randomSeed == .env$randomSeed) |>
          pull(idx)
        stopifnot(length(idx) == 1)
        forecastError[[idx, "result"]] <- list(result)
      }
      if (!is.null(methodOutput$assimilation)) {
        err <- sqrt(rowSums((xTrainTruth - methodOutput$assimilation)^2))
        err0 <- sqrt(rowSums((xTrainTruth - xTrain)^2))
        cat(sprintf("[Analysis RMSE/RMSE0: %.2f] ", mean(err)/mean(err0)))
        result <- bind_cols(time = tTrain, error = err)
        idx <- assimilationError |>
          filter(.data$noiseScale == .env$noiseScale, .data$method == .env$method, .data$randomSeed == .env$randomSeed) |>
          pull(idx)
        stopifnot(length(idx) == 1)
        assimilationError[[idx, "result"]] <- list(result)
      }

      cat(sprintf("took %.2fs.\n", (proc.time()-pt)[3]))
    }
  }

}

writeInfoAndData(info, forecastError, outDirPath, paste0(outName, "_forecast")) |> print()
writeInfoAndData(info, assimilationError, outDirPath, paste0(outName, "_assimilation")) |> print()

dataDirPath <- "../SimuChaosWithNoiseData"
plotDirPath <- file.path("../img")
tableDirPath <- file.path("../tbl")
evalDirPath <- file.path(dataDirPath, "evaluation")
forecastDirPath <- file.path(dataDirPath, "forecast")
truthDirPath <- file.path(dataDirPath, "truth")
logDirPath <- file.path(dataDirPath, "_log")

outDirPaths <- dplyr::lst(
  dataDirPath,
  plotDirPath,
  tableDirPath,
  evalDirPath,
  forecastDirPath,
  truthDirPath,
)

lapply(outDirPaths, dir.create, showWarnings = FALSE, recursive = TRUE)

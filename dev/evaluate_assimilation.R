options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_errorMetrics.R")


# default parameters
folder <- "L63_default_Gaussian_D3"
label <- NULL
truthName <- NULL
systemLabel <- NULL

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived values
if (is.null(label)) label <- folder
if (is.null(truthName)) truthName <- str_extract(folder, "^[^_]*_[^_]*")
resultDirPath <- file.path(forecastDirPath, folder)
evaluationFilePath <- file.path(evalDirPath, sprintf("assimilation_eval_%s.csv", folder))


resultNames <-
  list.files(
    resultDirPath,
    pattern = sprintf("^%s_[^_]+_[0-9]+_assimilation_[0-9a-f]{32}\\.[^.]*$", folder)
  ) |>
  str_remove("\\.[^.]+$") |>
  unique() |>
  sort()

# HACK:
#resultNames <- resultNames |> str_subset("_001_")

if (length(resultNames) == 0) stop("Did not find any forecast results.")

evaluationDataList <-
  lapply(resultNames, \(resultName) {

    cat("Processing", resultName, "...\n")

    result <- readInfoAndData(resultDirPath, resultName)

    rmse <- sapply(
      result$data$result,
      \(res) sqrt(mean(res$error^2))
    )

    result$data |>
      select(-result, -idx) |>
      bind_cols(
        rmse = rmse
      ) |>
      mutate(
        truthName = result$info$truthName,
        noiseName = result$info$noiseName,
        name = result$info$name,
        hash = result$info$hash
      )
  })
evaluationData <- evaluationDataList |> bind_rows()

# HACK:
#evaluationData <- bind_rows(evaluationData, read_csv(evaluationFilePath)) |> distinct()


cat("Write", evaluationFilePath, "\n")
write_csv(evaluationData, evaluationFilePath)


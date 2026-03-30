options(tidyverse.quiet = TRUE)
library(tidyverse)
library(gt)
source("common_paths.R")
source("common_io.R")
source("common_defaults.R")



# default parameters
errorMetric <- "vpt0.5"
errorMetricLabel <- "VPT"
defaults <- getDefaults()

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived and const values
errorStatsFilePath <- file.path(evalDirPath, sprintf("errorStats_%s.csv", errorMetric))
errorStats <- read_csv(errorStatsFilePath)

experiments <-
  errorStats |>
  select(truthName, noiseName) |>
  distinct()


for (i in seq_len(nrow(experiments))) {

  experiment <- experiments[i,] |> as.list()

  outFileLabel <- sprintf(
    "forecast_%s_%s_%s",
    experiment$truthName,
    experiment$noiseName,
    errorMetricLabel
  )

  experiErrorStats <-
    errorStats |>
    filter(
      truthName == experiment$truthName,
      noiseName == experiment$noiseName,
    ) |>
    select(-all_of(names(experiment))) |>
    separate_wider_delim(method, delim = ": ", names = c("methodName", "options"), cols_remove=FALSE, too_few = "align_start")

  plt <-
    experiErrorStats |>
    filter(mean == max(mean), .by = c(noiseScale, methodName)) |>
    ggplot(aes(color = methodName, x = methodName)) +
      geom_errorbar(aes(ymin = ci95Lower, ymax = ci95Upper)) +
      geom_point(aes(y = mean)) +
      facet_wrap(vars(noiseScale), scales="free_y") +
      theme(axis.title.x=element_blank(),
          axis.text.x=element_blank()) +
        ggtitle("Best from each Method")

  saveGgplotAsPdf(plt, file.path(plotDirPath, sprintf("%s_errorbars.pdf", outFileLabel)), width=8, heigh=4)

  methodNames <- experiErrorStats$methodName |> unique()
  for (methodName in methodNames) {
    plt <-
      experiErrorStats |>
      filter(.data$methodName == .env$methodName) |>
      ggplot(aes(color = options, x = options)) +
        geom_errorbar(aes(ymin = ci95Lower, ymax = ci95Upper)) +
        geom_point(aes(y = mean)) +
        facet_wrap(vars(noiseScale), scales="free_y") +
        theme(
          axis.title.x=element_blank(),
          axis.text.x=element_blank()
        ) +
        ggtitle(methodName)
    saveGgplotAsPdf(
      plt,
      file.path(plotDirPath, sprintf("%s_%s_errorbars.pdf", outFileLabel, methodName)),
      width=8, heigh=4)
  }

}



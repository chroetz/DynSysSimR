#' @export
observe <- function(xTruth, noiseSd, type, seed) {
  observeSpecific <- switch(
    type,
    Gauss = observeGauss,
    stop("Unkown observer type: ", type)
  )
  withr::with_seed(
    seed,
    observeSpecific(xTruth, noiseSd)
  )
}

observeGauss <- function(xTruth, noiseSd) {
  xTruth + rnorm(length(xTruth), sd = noiseSd)
}

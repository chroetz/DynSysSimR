#' @export
parmsMethodString <- function(methodString) {
  method <- splitMethodString(methodString)
  list(
    methodName = method$name,
    parms = parmsMethod(method$name, method$parmsString)
  )
}

#' @export
parmsMethod <- function(methodName, parmsString = NULL) {
  do.call(paste0("parms", methodName), lst(parmsString)) # must return a list that can be converted to a tibble with one row via tibble::as_tibble_row() and the saved as arrow feather
}

#' @export
fitMethod <- function(methodName, parms, xTrain, seed) {

  args <- lst(xTrain, parms)
  memBefore <- gc(verbose=FALSE, reset=TRUE)
  pt <- proc.time()
  model <-
    withr::with_seed(
      seed,
      do.call(paste0("fit", methodName), args)
    )
  time <- unname((proc.time() - pt)["elapsed"])
  mem <- sum((gc(verbose=FALSE) - memBefore)[, 6])

  return(
    lst(
      model,
      time,
      mem
    )
  )
}

#' @export
predictMethod <- function(methodName, model, initialConditions, nPred, seed) {

  args <- lst(model, initialConditions, nPred)

  memBefore <- gc(verbose=FALSE, reset=TRUE)
  pt <- proc.time()
  prediction <-
    withr::with_seed(
      seed,
      do.call(paste0("predict", methodName), args)
    )
  time <- unname((proc.time() - pt)["elapsed"])
  mem <- sum((gc(verbose=FALSE) - memBefore)[, 6])

  return(
    lst(
      prediction,
      time,
      mem
    )
  )
}

#' @export
assimilateMethod <- function(methodName, model, xTrain, seed) {

  args <- lst(model, xTrain)

  memBefore <- gc(verbose=FALSE, reset=TRUE)
  pt <- proc.time()
  assimilation <-
    withr::with_seed(
      seed,
      do.call(paste0("assimilate", methodName), args)
    )
  time <- unname((proc.time() - pt)["elapsed"])
  mem <- sum((gc(verbose=FALSE) - memBefore)[, 6])

  return(
    lst(
      assimilation,
      time,
      mem
    )
  )
}

splitMethodString <- function(methodWithParms) {
  split <- str_split(methodWithParms, pattern = ":", n = 2)[[1]]
  list(
    name = str_trim(split[1]),
    parmsString = split[-1]
  )
}

#' @export
coefMethod <- function(methodName, model) {
  do.call(paste0("coef", methodName), lst(model))
}

#' @export
fitMethod <- function(methodString, xTrain, seed) {

  method <- splitMethodString(methodString)

  parms <- do.call(paste0("parms", method$name), list(parmsString = method$parms))

  args <- lst(xTrain, parms)
  memBefore <- gc(verbose=FALSE, reset=TRUE)
  pt <- proc.time()
  model <-
    withr::with_seed(
      seed,
      do.call(paste0("fit", method$name), args)
    )
  time <- unname((proc.time() - pt)["elapsed"])
  mem <- sum((gc(verbose=FALSE) - memBefore)[, 6])

  return(
    lst(
      methodName = method$name,
      model,
      parms,
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
    parms = split[2]
  )
}

parmsRollout0 <- function(parmsString) {
  normalization <- "none"
  nDeg <- 2
  nRollout <- 5
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(nDeg, nRollout, normalization))
}

fitRollout0 <- function(xTrain, parms) {
  model <- DynLossOptimR::fitCoef0Loss(
    xTrain,
    nDeg = parms$nDeg,
    layers = parms$nRollout,
    normalizationType = parms$normalization
  )
  return(model)
}

predictRollout0 <- function(model, initialConditions, nPred) {
  prediction <- DynLossOptimR::predictCoef0Loss(model, initialConditions, nPred)
  return(prediction)
}

assimilateRollout0 <- function(model, xTrain) {
  return(xTrain)
}

coefRollout0 <- function(model) {
  return(model$coef)
}


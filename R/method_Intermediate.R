parmsIntermediate <- function(parmsString) {
  normalization <- "none"
  nDeg <- 2
  nIntermediate <- 5
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(nDeg, nIntermediate, normalization))
}

fitIntermediate <- function(xTrain, parms) {
  ws <- DynLossOptimR::getWeightScheduleCoefIntermediate(nDeg = parms$nDeg, d = ncol(xTrain), intermediate = parms$nIntermediate)
  model <- DynLossOptimR::fitCoefLoss(
    xTrain,
    nDeg = parms$nDeg,
    weightSchedule = ws,
    normalizationType = parms$normalization
  )
  return(model)
}

predictIntermediate <- function(model, initialConditions, nPred) {
  prediction <- DynLossOptimR::predictCoefLoss(model, initialConditions, nPred)
  return(prediction)
}

assimilateIntermediate <- function(model, xTrain) {
  return(xTrain)
}

coefIntermediate <- function(model) {
  return(model$coef)
}


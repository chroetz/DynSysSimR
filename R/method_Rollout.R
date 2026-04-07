parmsRollout <- function(parmsString) {
  normalization <- "none"
  nDeg <- 2
  nRollout <- 5
  weightingType <- "const" # or "point "
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(nDeg, nRollout, normalization, weightingType))
}

fitRollout <- function(xTrain, parms) {
  ws <- DynLossOptimR::getWeightScheduleCoef(
    nDeg = parms$nDeg,
    d = ncol(xTrain),
    kmax = parms$nRollout,
    weightsObsBase = 1,
    type = parms$weightingType
  )
  model <- DynLossOptimR::fitCoefLoss(
    xTrain,
    nDeg = parms$nDeg,
    weightSchedule = ws,
    normalizationType = parms$normalization
  )
  return(model)
}

predictRollout <- function(model, initialConditions, nPred) {
  prediction <- DynLossOptimR::predictCoefLoss(model, initialConditions, nPred)
  return(prediction)
}

assimilateRollout <- function(model, xTrain) {
  return(xTrain)
}

coefRollout <- function(model) {
  return(model$coef)
}


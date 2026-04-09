parmsFullLoss <- function(parmsString) {
  normalization <- "none"
  nDeg <- 2
  nRollout <- 5
  weightsObsBase <- 0.2
  weightsAnaBase <- 0.2
  targetUpdateFactor <- 0
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(nDeg, nRollout, normalization, weightsObsBase, weightsAnaBase, targetUpdateFactor))
}

fitFullLoss <- function(xTrain, parms) {
  ws <- DynLossOptimR::getWeightScheduleFull(
    nDeg = parms$nDeg,
    d = ncol(xTrain),
    kmax = parms$nRollout,
    weightsObsBase = parms$weightsObsBase,
    weightsAnaBase = parms$weightsAnaBase
  )
  model <- DynLossOptimR::fitFullLoss(
    xTrain,
    nDeg = parms$nDeg,
    weightSchedule = ws,
    normalizationType = parms$normalization,
    targetUpdateFactor = parms$targetUpdateFactor
  )
  return(model)
}

predictFullLoss <- function(model, initialConditions, nPred) {
  prediction <- DynLossOptimR::predictFullLoss(model, initialConditions, nPred)
  return(prediction)
}

assimilateFullLoss <- function(model, xTrain) {
  return(model$analysis)
}

coefFullLoss <- function(model) {
  return(model$coef)
}


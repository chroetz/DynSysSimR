parmsEnKF <- function(parmsString) {
  normalization <- "none"
  nDeg <- 2
  l2Penalty <- 0
  nEns <- 300
  coefUpdate <- "coupled"
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(nDeg, normalization, l2Penalty, nEns, coefUpdate))
}

fitEnKF <- function(xTrain, parms) {
  model <- PolyPropR::fitEnkf(
    list(xTrain),
    featureSpec = PolyPropR::polyFeatures(parms$nDeg),
    normalizationType = parms$normalization,
    nEns = parms$nEns,
    coefUpdate = parms$coefUpdate,
    l2Penalty = parms$l2Penalty
  )
  return(model)
}

predictEnKF <- function(model, initialConditions, nPred) {
  prediction <- PolyPropR::predictEnkf(model, initialConditions, nPred)
  return(prediction)
}

assimilateEnKF <- function(model, xTrain) {
  return(model$analysis[[1]])
}


parmsPolyProp <- function(parmsString) {
  normalization <- "none"
  nDeg <- 2
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(nDeg, normalization))
}

fitPolyProp <- function(xTrain, parms) {
  model <- PolyPropR::fitPolyprop(
    list(xTrain),
    featureSpec = PolyPropR::polyFeatures(parms$nDeg),
    normalizationType = parms$normalization
  )
  return(model)
}

predictPolyProp <- function(model, initialConditions, nPred) {
  prediction <- PolyPropR::predictPolyprop(model, initialConditions, nPred)
  return(prediction)
}

assimilatePolyProp <- function(model, xTrain) {
  return(xTrain)
}

coefPolyProp <- function(model) {
  return(model$coeffs)
}


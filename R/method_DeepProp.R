parmsDeepProp <- function(parmsString) {
  normalization <- "none"
  nDeg <- 2
  nLayers <- 3
  learningRate <- 1.0
  l2Penalty <- 0.0
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(nDeg, normalization, nLayers, learningRate, l2Penalty))
}

fitDeepProp <- function(xTrain, parms) {
  model <- PolyPropR::fitDeepprop(
    list(xTrain),
    featureSpec = PolyPropR::polyFeatures(parms$nDeg),
    nLayers = as.integer(parms$nLayers),
    normalizationType = parms$normalization,
    l2Penalty = parms$l2Penalty,
    learningRate = parms$learningRate
  )
  return(model)
}

predictDeepProp <- function(model, initialConditions, nPred) {
  prediction <- PolyPropR::predictDeepprop(model, initialConditions, nPred)
  return(prediction)
}

assimilateDeepProp <- function(model, xTrain) {
  return(model$assimilation[[1]])
}

coefDeepProp <- function(model) {
  return(model$coeffsList)
}

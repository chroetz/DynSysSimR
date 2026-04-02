parmsEchoBoost <- function(parmsString) {
  normalization <- "none"
  nDeg <- 2
  kmax <- 1e3
  kmin <- 0
  alpha <- 0.1
  adjustResponse <- TRUE
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(nDeg, normalization, kmax, kmin, alpha, adjustResponse))
}

fitEchoBoost <- function(xTrain, parms) {
  model <- PolyPropR::fitEchoboost(
    list(xTrain),
    featureSpec = PolyPropR::polyFeatures(parms$nDeg),
    normalizationType = parms$normalization,
    kMax = parms$kmax,
    kMin = parms$kmin,
    alpha = parms$alpha,
    adjustResponses = parms$adjustResponse
  )
  return(model)
}

predictEchoBoost <- function(model, initialConditions, nPred) {
  prediction <- PolyPropR::predictEchoboost(model, initialConditions, nPred)
  return(prediction)
}

assimilateEchoBoost <- function(model, xTrain) {
  return(model$assimilation[[1]])
}

coefEchoBoost <- function(model) {
  return(model$coeffs)
}

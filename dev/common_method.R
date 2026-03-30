
runMethod <- function(method, xTrain, nPred, seed) {
  methodSplit <- str_split(method, pattern = ":", n = 2)[[1]]
  methodName <- str_trim(methodSplit[1])
  methodArgs <- methodSplit[-1]

  methodOutput <-
    withr::with_seed(
      seed,
      switch(
        methodName,
        PolyProp = runPolyProp(xTrain, nPred, methodArgs),
        EchoBoostPolyProp = runEchoBoostPolyProp(xTrain, nPred, methodArgs),
        EnKF = runEnKF(xTrain, nPred, methodArgs),
        stop("Unkown method ", method)
      )
    )

  return(methodOutput)
}


runPolyProp <- function(xTrain, nPred, methodArgs) {
  # parameters
  normalization <- "none"
  nDeg <- 2

  d <- ncol(xTrain)

  eval(parse(text = paste(methodArgs, collapse=";")))

  model <- PolyPropR::fitPolyprop(
    list(xTrain),
    featureSpec=PolyPropR::polyFeatures(nDeg),
    normalizationType = normalization
  )
  prediction <- PolyPropR::predictPolyprop(model,  xTrain[nrow(xTrain), , drop=FALSE], nPred)

  return(list(assimilation = xTrain, forecast = prediction[,,1]))
}



runEchoBoostPolyProp <- function(xTrain, nPred, methodArgs) {

  normalization <- "none"
  d <- ncol(xTrain)
  kmax <- 1e3
  kmin <- 0
  alpha <- 0.1
  adjustResponse <- TRUE

  eval(parse(text = paste(methodArgs, collapse=";")))

  model <- PolyPropR::fitEchoboost(
    list(xTrain),
    featureSpec=PolyPropR::polyFeatures(nDeg),
    normalizationType = normalization,
    kMax = kmax,
    kMin = kmin,
    alpha = alpha,
    adjustResponses = adjustResponse
  )
  last <- model$assimilation[[1]][nrow(model$assimilation[[1]]), , drop=FALSE]
  prediction <- PolyPropR::predictEchoboost(model, last, nPred)

  return(list(assimilation = model$assimilation[[1]], forecast = prediction[,,1]))
}



runEnKF <- function(xTrain, nPred, methodArgs) {

  normalization <- "none"
  nDeg <- 3       # Degree of polynomial features
  beta_ridge <- 0       # Regularization
  n_ens <- 300      # Ensemble size
  coefUpdate <- "coupled"

  d <- ncol(xTrain)

  eval(parse(text = paste(methodArgs, collapse=";")))

  model <- PolyPropR::fitEnkf(
    list(xTrain),
    featureSpec=PolyPropR::polyFeatures(nDeg),
    normalizationType = normalization,
    nEns = n_ens,
    coefUpdate = coefUpdate,
    l2Penalty = beta_ridge
  )
  last <- model$analysis[[1]][nrow(model$analysis[[1]]), , drop=FALSE]
  prediction <- PolyPropR::predictEnkf(model, last, nPred)

  return(list(assimilation = model$analysis[[1]], forecast = prediction[,,1]))
}

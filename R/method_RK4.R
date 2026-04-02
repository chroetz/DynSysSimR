parmsRK4 <- function(parmsString) {
  systemName <- "L63"
  timeStep <- 2^-10
  rtol <- 1e-12
  atol <- 1e-14
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(systemName, timeStep, rtol, atol))
}

fitRK4 <- function(xTrain, parms) {
  func <- switch(
    parms$systemName,
    L63 = lorenz63,
    stop("Unknown systemName ", systemName)
  )
  funcParms <- switch(
    parms$systemName,
    L63 = c(sigma = 10, rho = 28, beta = 8/3),
    stop("Unknown systemName ", systemName)
  )
  return(c(parms, lst(func, funcParms)))
}

predictRK4 <- function(model, initialConditions, nPred) {
  n <- nrow(initialConditions)
  times <- seq(0, length.out = nPred+1, by = model$timeStep)
  prediction <-
    vapply(
      seq_len(n),
      \(i) {
        out <- deSolve::ode(
          y = initialConditions[i, ],
          times = times,
          func = model$func,
          parms = model$funcParms,
          method = "ode45",
          rtol = model$rtol,
          atol =  model$atol
        )
        unclass(out)[-1, -1, drop = FALSE]
      },
      matrix(NA_real_, nrow = nPred, ncol = ncol(initialConditions))
    )
  return(prediction)
}

assimilateRK4 <- function(model, xTrain) {
  return(xTrain)
}

lorenz63 <- function(t, state, parms) {
  list(c(
    parms[1] * (state[2] - state[1]),
    state[1] * (parms[2] - state[3]) - state[2],
    state[1] * state[2] - parms[3] * state[3]
  ))
}

coefRK4 <- function(model) {
  return(model$funcParms)
}

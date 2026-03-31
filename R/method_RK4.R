parmsRK4 <- function(parmsString) {
  systemName <- "L63"
  timeStep <- 2^-10
  eval(parse(text = paste(parmsString, collapse=";")))
  return(lst(systemName, timeStep))
}

fitRK4 <- function(xTrain, parms) {
  return(parms)
}

predictRK4 <- function(model, initialConditions, nPred) {
  func <- switch(
    model$systemName,
    L63 = lorenz63,
    stop("Unknown systemName ", systemName)
  )
  funcParms <- switch(
    model$systemName,
    L63 = c(sigma = 10, rho = 28, beta = 8/3),
    stop("Unknown systemName ", systemName)
  )
  n <- nrow(initialConditions)
  times <- seq(0, length.out = nPred+1, by = model$timeStep)
  prediction <-
    vapply(
      seq_len(n),
      \(i) {
        out <- deSolve::ode(
          y = initialConditions[i, ],
          times = times,
          func = func,
          parms = funcParms,
          method = "ode45",
          rtol = 1e-12,
          atol = 1e-14
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

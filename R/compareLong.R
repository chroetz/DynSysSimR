###############################################################################
# compareLong.R
#
# Compare long-term statistics of two dynamical system trajectories.
# x, y: matrices with the same number of columns but possibly different
#        numbers of rows.  Each row is one sample of the state vector.
# All helpers return scalars or simple structures; the main function
# returns a list with one scalar error/distance per statistic.
#
# Designed for n ~ 1e6, d in 1..10.  No external packages required.
###############################################################################

# ---- 1. Largest Lyapunov exponent (Rosenstein-style) -----------------------
#
# Since x is already in full state space we skip delay embedding.
# For each reference point, find its nearest neighbour (with a temporal
# separation guard), then track log-divergence over time.  The slope of
# the mean log-divergence curve gives the largest Lyapunov exponent
# (in units of 1/dt, where dt is the sampling step).

estimateLyapunov <- function(X, maxIter  = 500L,
                             nRef      = 2000L,
                             nCand     = 5000L,
                             minSep    = 50L,
                             fitStart  = 10L,
                             fitEnd    = NULL) {
  n <- nrow(X);  d <- ncol(X)
  if (n < maxIter + minSep + 1L) {
    warning("Trajectory too short for the requested maxIter / minSep; returning NA.")
    return(NA_real_)
  }

  usable  <- n - maxIter
  refIdx  <- sample.int(usable, size = min(nRef, usable))
  candIdx <- sample.int(n,      size = min(nCand, n))
  xCand   <- X[candIdx, , drop = FALSE]

  logDiv <- matrix(NA_real_, nrow = length(refIdx), ncol = maxIter + 1L)

  for (k in seq_along(refIdx)) {
    i  <- refIdx[k]
    xi <- X[i, ]

    # squared distances to every candidate
    diff  <- sweep(xCand, 2L, xi)
    sqDst <- rowSums(diff * diff)

    # mask out candidates that are temporally close or whose forward
    # orbit would run past the end of the trajectory
    bad <- abs(candIdx - i) < minSep | (candIdx + maxIter) > n
    sqDst[bad] <- Inf

    jLocal <- which.min(sqDst)
    if (!is.finite(sqDst[jLocal])) next          # no valid neighbour
    j <- candIdx[jLocal]

    # vectorised divergence over time
    segI <- X[i:(i + maxIter), , drop = FALSE]
    segJ <- X[j:(j + maxIter), , drop = FALSE]
    logDiv[k, ] <- 0.5 * log(pmax(rowSums((segI - segJ)^2), 1e-300))
  }

  # average curve (drop rows that had no valid neighbour)
  good  <- !is.na(logDiv[, 1L])
  nGood <- sum(good)
  if (nGood == 0L) {
    warning("No valid reference pairs found; returning NA for Lyapunov exponent.")
    return(NA_real_)
  }
  if (nGood < 20L) warning("Very few valid reference pairs found.")
  S <- colMeans(logDiv[good, , drop = FALSE])

  # linear fit in the scaling region
  if (is.null(fitEnd)) fitEnd <- max(maxIter %/% 4L, fitStart + 10L)
  fitEnd   <- min(fitEnd, maxIter + 1L)
  fitStart <- max(fitStart, 1L)
  idx <- fitStart:fitEnd
  tt  <- idx - 1L                       # time in units of dt

  # guard against all-NA / all-NaN fit region
  ok <- is.finite(S[idx])
  if (sum(ok) < 2L) {
    warning("Too few finite values in the scaling region; returning NA.")
    return(NA_real_)
  }
  fit <- lm(S[idx][ok] ~ tt[ok])
  as.numeric(coef(fit)[2L])              # slope = Lyapunov exponent / dt
}


# ---- 2. Invariant measure  (sliced 1-D Wasserstein) -----------------------
#
# Full Wasserstein in d>2 is too expensive for 1e6 points.
# We use the *sliced* Wasserstein-1 distance: project onto many random
# unit vectors, compute the exact 1-D W1 via sort-and-match (both
# trajectories are truncated to equal length first), and average.

slicedWasserstein <- function(X, Y, nProj = 100L) {
  d <- ncol(X)
  stopifnot(ncol(Y) == d)

  # truncate the longer trajectory so both have equal length
  n <- min(nrow(X), nrow(Y))
  if (n < 2L) {
    warning("Trajectories too short for Wasserstein; returning NA.")
    return(NA_real_)
  }
  X <- X[seq_len(n), , drop = FALSE]
  Y <- Y[seq_len(n), , drop = FALSE]

  # random projection directions (d x nProj), unit normalised
  dirs <- matrix(rnorm(d * nProj), nrow = d, ncol = nProj)
  norms <- sqrt(colSums(dirs^2))
  # guard against zero-norm direction (astronomically unlikely but safe)
  norms[norms == 0] <- 1
  dirs <- dirs / rep(norms, each = d)

  # project: n x nProj for each trajectory
  pX <- X %*% dirs
  pY <- Y %*% dirs

  # exact 1-D W1 for equal-size samples: mean|sort(a) - sort(b)|
  w1 <- numeric(nProj)
  for (k in seq_len(nProj)) {
    w1[k] <- mean(abs(sort(pX[, k]) - sort(pY[, k])))
  }
  return(w1)
}


# ---- 3. Attractor (correlation) dimension  (Grassberger-Procaccia) ---------
#
# Subsample nSample points, compute all pairwise distances, then
# estimate the slope of log C(r) vs log r in the scaling region.

estimateCorrelationDimension <- function(X,
                                         nSample  = 5000L,
                                         nRadii   = 60L,
                                         qLo      = 0.01,
                                         qHi      = 0.40,
                                         fitFrac  = c(0.20, 0.80)) {
  n <- nrow(X)
  if (n < 3L) {
    warning("Too few points for correlation dimension; returning NA.")
    return(NA_real_)
  }
  idx <- sample.int(n, size = min(nSample, n))
  Xs  <- X[idx, , drop = FALSE]

  # drop rows containing NA/NaN/Inf so dist() stays clean
  finiteRows <- rowSums(!is.finite(Xs)) == 0L
  Xs <- Xs[finiteRows, , drop = FALSE]
  if (nrow(Xs) < 3L) {
    warning("Too few finite rows for correlation dimension; returning NA.")
    return(NA_real_)
  }

  dists <- as.vector(dist(Xs))              # N*(N-1)/2 distances
  dists <- dists[is.finite(dists) & dists > 0]

  if (length(dists) < 2L) {
    warning("No usable pairwise distances (all zero, NA, or NaN); returning NA.")
    return(NA_real_)
  }

  radii <- exp(seq(log(quantile(dists, qLo)),
                   log(quantile(dists, qHi)),
                   length.out = nRadii))

  Cr <- vapply(radii, function(r) mean(dists < r), numeric(1L))

  ok    <- Cr > 0
  logR  <- log(radii[ok])
  logCr <- log(Cr[ok])
  nOk   <- length(logR)

  if (nOk < 2L) {
    warning("No usable points in the correlation integral; returning NA.")
    return(NA_real_)
  }

  # fit the scaling (middle) portion of the log-log plot
  i1  <- max(1L, round(nOk * fitFrac[1]))
  i2  <- min(nOk, round(nOk * fitFrac[2]))
  if (i2 <= i1) {
    warning("Scaling region too narrow for fit; returning NA.")
    return(NA_real_)
  }
  fit <- lm(logCr[i1:i2] ~ logR[i1:i2])
  as.numeric(coef(fit)[2L])
}


# ---- 4. Autocorrelation function distance ----------------------------------
#
# For each coordinate j = 1..d compute the ACF out to maxLag for both
# trajectories, then report the mean (over dimensions) of the RMSE
# between the two ACF curves.

acfPerDim <- function(X, maxLag = 100L) {
  d <- ncol(X)
  n <- nrow(X)

  # need at least maxLag + 1 observations; clamp if needed
  if (n < 2L) {
    warning("Trajectory too short for ACF; returning NA matrix.")
    return(matrix(NA_real_, nrow = maxLag + 1L, ncol = d))
  }
  maxLag <- min(maxLag, n - 1L)

  acPerDim <- matrix(NA_real_, nrow = maxLag + 1L, ncol = d)
  for (j in seq_len(d)) {
    acPerDim[, j] <- as.numeric(acf(X[, j], lag.max = maxLag, plot = FALSE)$acf)
  }
  return(acPerDim)
}


# ---- Main comparison -------------------------------------------------------

#' @export
compareLong <- function(query, target,
                        # Lyapunov parameters
                        lyapMaxIter    = 500L,
                        lyapNRef       = 2000L,
                        lyapNCand      = 5000L,
                        lyapMinSep     = 50L,
                        lyapFitStart   = 10L,
                        lyapFitEnd     = NULL,
                        # Correlation dimension parameters
                        cordimNSample  = 5000L,
                        # Sliced Wasserstein parameters
                        swNProj        = 100L,
                        # ACF parameters
                        acfMaxLag      = 100L) {

  stopifnot(is.matrix(query), is.matrix(target), ncol(query) == ncol(target))

  leX <- estimateLyapunov(
    query, lyapMaxIter, lyapNRef, lyapNCand,
    lyapMinSep, lyapFitStart, lyapFitEnd
  )

  sw <- slicedWasserstein(query, target, swNProj)

  cdX <- estimateCorrelationDimension(query, cordimNSample)

  ac <- acfPerDim(query, acfMaxLag)

  # build Wasserstein summary; handle scalar NA from degenerate case
  if (length(sw) == 1L && is.na(sw)) {
    wsSummary <- list(mean = NA_real_, sd = NA_real_, n = 0L)
  } else {
    wsSummary <- list(mean = mean(sw), sd = sd(sw), n = length(sw))
  }

  list(
    lyapunov             = leX,
    wasserstein          = wsSummary,
    correlationDimension = cdX,
    autocorrelation      = ac,
    nNotFinite = sum(rowSums(is.finite(query))>0),
    n = nrow(query)
  )
}

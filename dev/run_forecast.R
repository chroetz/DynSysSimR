source("common_run.R")

label <- "fore"
args <- commandArgs(TRUE)
if (length(args) == 0) {
  runMode <- "s"
  randomSeeds <- "1:100"
} else {
  stopifnot(length(args)==2)
  runMode <- args[1]
  randomSeeds <- eval(parse(text = args[2]))
}


methods <- c(
  # "FullLoss: nDeg=3;kmax=10;targetUpdateFactor=0;reltol=1e-14",
  # "FullLoss: nDeg=3;kmax=10;targetUpdateFactor=0.5;reltol=1e-14",
  # "FullLoss: nDeg=3;kmax=10;targetUpdateFactor=1;reltol=1e-14",
  "PolyProp: nDeg=2;v=1",
  "PolyProp: nDeg=3;v=1",
  "EnKF: nDeg=2;coefUpdate=\\\"coupled\\\";v=1",
  "EnKF: nDeg=3;coefUpdate=\\\"coupled\\\";v=1",
  "EnKF: nDeg=2;coefUpdate=\\\"decoupled\\\";v=1",
  "EnKF: nDeg=3;coefUpdate=\\\"decoupled\\\";v=1",
  "EchoBoostPolyProp: nDeg=2;adjustResponse=FALSE;v=1",
  "EchoBoostPolyProp: nDeg=2;adjustResponse=TRUE;v=1",
  "EchoBoostPolyProp: nDeg=3;adjustResponse=FALSE;v=1",
  "EchoBoostPolyProp: nDeg=3;adjustResponse=TRUE;v=1",
  # # "EchoBoostPolyProp: nDeg=3;adjustResponse=FALSE",
  # "EchoBoostPolyProp: nDeg=3;adjustResponse=TRUE",
  #"PolyPropR: nDeg=3;step=1",
  # "PolyProp: nDeg=3;step=1",
  # "PolyProp: nDeg=3;step=2",
  # "PolyProp: nDeg=3;step=3",
  # "PolyProp: nDeg=3;step=4",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:02",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:04",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:08",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:16",
  # "PolyPropMultiStep: nDeg=3;forwardSteps=2:32",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=5;smootherDegree=2",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=5;smootherDegree=3",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=7;smootherDegree=2",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=7;smootherDegree=3",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=7;smootherDegree=4",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=7;smootherDegree=5",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=9;smootherDegree=2",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=9;smootherDegree=3",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=9;smootherDegree=4",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=9;smootherDegree=5",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=9;smootherDegree=6",
  # "LocalSmootherPolyProp: nDeg=3;windowSize=9;smootherDegree=7",
  NULL
)


cmdBase <- makeCmd(
  "forecast.R",
  c("randomSeeds=%s",
    "methods='%s'",
    NULL
  )
)

cmds <- NULL
for (method in methods) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeeds, method))
}


runCmds(cmds, runMode, label, qos="short", timeInMin=1440)


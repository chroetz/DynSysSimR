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


methodStrings <- c(
  "RK4: systemName=\\\'L63\\\';timeStep=2^-5",
  "PolyProp: nDeg=2",
  "PolyProp: nDeg=3",
  "PolyProp: nDeg=4",
  "PolyProp: nDeg=5",
  "EnKF: nDeg=2;coefUpdate=\\\'coupled\\\'",
  "EnKF: nDeg=3;coefUpdate=\\\'coupled\\\'",
  "EnKF: nDeg=4;coefUpdate=\\\'coupled\\\'",
  "EnKF: nDeg=5;coefUpdate=\\\'coupled\\\'",
  "EnKF: nDeg=2;coefUpdate=\\\'decoupled\\\'",
  "EnKF: nDeg=3;coefUpdate=\\\'decoupled\\\'",
  "EnKF: nDeg=4;coefUpdate=\\\'decoupled\\\'",
  "EnKF: nDeg=5;coefUpdate=\\\'decoupled\\\'",
  "EchoBoost: nDeg=2;adjustResponse=FALSE",
  "EchoBoost: nDeg=3;adjustResponse=FALSE",
  "EchoBoost: nDeg=4;adjustResponse=FALSE",
  "EchoBoost: nDeg=5;adjustResponse=FALSE",
  "EchoBoost: nDeg=2;adjustResponse=TRUE",
  "EchoBoost: nDeg=3;adjustResponse=TRUE",
  "EchoBoost: nDeg=4;adjustResponse=TRUE",
  "EchoBoost: nDeg=5;adjustResponse=TRUE",
  NULL
)


cmdBase <- makeCmd(
  "forecast.R",
  c("randomSeeds=%s",
    "methodStrings='%s'",
    "noiseScales=c(0, 1e-2, 1e-1)",
    "nTrain=2^10",
    "stepRate=2^5",
    NULL
  )
)

cmds <- NULL
for (methodString in methodStrings) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeeds, methodString))
}


runCmds(cmds, runMode, label, qos="short", timeInMin=1440)


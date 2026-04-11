#' @export
writeInfoAndData <- function(info, data, dirPath, name, excludeInfoEntryFromHash = c("^args$", "Path$")) {

  excludedInfoEntryFromHash <- sapply(names(info), \(nm) any(stringr::str_detect(nm, excludeInfoEntryFromHash)))
  infoHashEntries <- setdiff(seq_along(info), which(excludedInfoEntryFromHash))
  hashInfo <- info[infoHashEntries]
  hash <- rlang::hash(list(data, hashInfo))
  infoFileName <- sprintf("%s_%s.json", name, hash)
  infoFilePath <- file.path(dirPath, infoFileName)
  dataFileName <- sprintf("%s_%s.feather", name, hash)
  dataFilePath <- file.path(dirPath, dataFileName)

  additionalInfo <- dplyr::lst(
    hash,
    excludeInfoEntryFromHash,
    includedInfoEntriesForHash = names(hashInfo),
    dirPath,
    name,
    infoFileName,
    infoFilePath,
    dataFileName,
    dataFilePath,
    dataAttributes = attributes(data),
    datetime = Sys.time(),
    sessionInfo = sessioninfo::session_info() |> as.character()
  )
  info <- c(info, additionalInfo)

  dir.create(dirPath, showWarnings=FALSE, recursive=TRUE)

  writeInfo(
    info,
    infoFilePath
  )

  data |>
    as.data.frame() |>
    arrow::write_feather(dataFilePath)

  return( dplyr::lst(hash, dataFilePath, infoFilePath))
}

#' @export
writeInfo <- function(info, infoFilePath) {
  jsonlite::write_json(
    info,
    infoFilePath,
    null = "null",
    na = "string",
    auto_unbox = TRUE,
    digits = NA,
    pretty = TRUE,
    force = TRUE
  )
}

#' @export
printInfo <- function(info) {
  jsonlite::toJSON(
    info,
    null = "null",
    na = "string",
    auto_unbox = TRUE,
    digits = NA,
    pretty = TRUE,
    force = TRUE
  ) |>
    print()
}

#' @export
readInfoAndData <- function(dirPath = NULL, name = NULL, filePath = NULL) {
  info <- readInfo(dirPath, name, filePath)
  data <- readData(dirPath, paste(info$name, info$hash, sep="_"))
  if ("dim" %in% names(info$dataAttributes)) {
    data <- as.matrix(data)
  }
  return(dplyr::lst(info, data))
}

#' @export
readInfo <- function(dirPath = NULL, name = NULL, filePath = NULL) {
  if (is.null(filePath)) {
    stopifnot(is.character(dirPath))
    stopifnot(is.character(name))
    stopifnot(length(dirPath) == 1)
    stopifnot(length(name) == 1)
    filePath <- list.files(
      dirPath,
      pattern = sprintf("^%s(_[0-9a-f]{32})?\\.json$", name),
      full.names = TRUE
    )
    if (length(filePath) == 0) stop("Did not find ", name, " info files at ", dirPath)
    if (length(filePath) > 1) {
      filePath <- filePath[1]
      warning("Found multiple ", name, " info files at ", dirPath, ". Using ", filePath, immediate.=TRUE)
    }
  }
  stopifnot(is.character(filePath))
  stopifnot(length(filePath) == 1)
  jsonlite::read_json(filePath, simplifyVector = TRUE)
}

#' @export
readData <- function(dirPath = NULL, name = NULL, filePath = NULL) {
  if (is.null(filePath)) {
    stopifnot(is.character(dirPath))
    stopifnot(is.character(name))
    stopifnot(length(dirPath) == 1)
    stopifnot(length(name) == 1)
    filePath <- list.files(
      dirPath,
      pattern = sprintf("^%s(_[0-9a-f]{32})?\\.feather$", name),
      full.names = TRUE
    )
    if (length(filePath) == 0) stop("Did not find ", name, " data files at ", dirPath)
    if (length(filePath) > 1) {
      filePath <- filePath[1]
      warning("Found multiple ", name, " data files at ", dirPath, ". Using ", filePath, immediate.=TRUE)
    }
  }
  stopifnot(is.character(filePath))
  stopifnot(length(filePath) == 1)
  arrow::read_feather(filePath)
}

#' @export
saveGgplotAsPdf <- function(plt, outFilePath, width, height, ...) {
  cat("Write", outFilePath, "\n")
  ggsave(
    filename = outFilePath,
    plot = plt,
    width = width,
    height = height,
    device = cairo_pdf,
    family = "Helvetica",
    ...
  )
}

#' @export
saveGgplotAsTikz <- function(plt, outFilePath, width, height, ...) {
  tikzDevice::tikz(
    file = outFilePath,
    standAlone = TRUE,
    width = width,
    height = height
  )
  plot(plt)
  dev.off()
  texLines <- read_lines(outFilePath)
  insertLines <- c(
    r"(\usepackage[scaled]{helvet})",
    r"(\renewcommand\familydefault{\sfdefault})",
    r"(\usepackage[T1]{fontenc})"
  )
  cat("Write", outFilePath, "\n")
  write_lines(
    c(texLines[1:5], insertLines, texLines[6:length(texLines)]),
    outFilePath
  )
  system(sprintf("pdflatex -interaction=nonstopmode -output-directory=\"%s\" \"%s\"", dirname(outFilePath), outFilePath))
}

#' @export
writeTableAsLatex <- function(tbl, outFilePath) {
  latex <- tbl |> gt::as_latex()
  txt <-
    latex |>
    str_replace(fixed("\\begin{table}[!t]"), "\\begin{center}") |>
    str_replace(fixed("\\end{table}"), "\\end{center}") |>
    str_replace(fixed("\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}"), "\\begin{tabular}{") |>
    str_replace(fixed("\\end{tabular*}"), "\\end{tabular}")
  lines <- str_split(txt, "\n")[[1]]
  lines <- lines[!str_detect(lines, "^\\\\fontsize")]
  header <-
    c(r"(\fontsize{8.0pt}{10pt}\selectfont)",
      r"(\fontfamily{phv}\selectfont)",
      r"(\renewcommand{\arraystretch}{1.05})",
      r"(\setlength{\tabcolsep}{0.5em})",
      r"(\rowcolors{2}{gray!20}{white})")
  linesPost <- c(lines[1], header, lines[2:length(lines)])
  cat("Write", outFilePath, "\n")
  write_lines(linesPost, outFilePath)
}

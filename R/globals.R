
# Suppress R CMD check NOTEs for non-standard evaluation variables
utils::globalVariables(c(
  "systemName"
  # add any other NSE column names that trigger NOTEs here
))


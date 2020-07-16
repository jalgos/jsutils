.libPaths("lib")

source("ci/dependencies.R")
df <- as.data.frame(devtools::test())

if (any(df[["failed"]] > 0) || any(df[["error"]] == TRUE)) stop("Some tests failed.")
.libPaths("lib")

if (!identical(devtools::check(document = FALSE, args = "--no-tests", error_on = c("error"))[["errors"]], character(0))) stop("Check with Errors")
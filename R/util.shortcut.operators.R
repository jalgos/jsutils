## Making "A"+"B" = "AB"
#' @export
setGeneric("%p%", function(e1, e2)
    return(paste0(as.character(e1), as.character(e2))))

## shortcut to grep
#' @export
setGeneric("%g%", function(e1, e2) grep(as.character(e1), as.character(e2)))

## shortcut to grepl
#' @export
setGeneric("%gl%", function(e1, e2) grepl(as.character(e1), as.character(e2)))

## shortcut to grep value = TRUE
#' @export
setGeneric("%gv%", function(e1, e2) grep(as.character(e1), as.character(e2), value = TRUE))

## shortcut to grep
#' @export
setGeneric("%gi%", function(e1, e2) grep(as.character(e1), as.character(e2), invert = TRUE))

## shortcut to grepl
#' @export
setGeneric("%gli%", function(e1, e2) !grepl(as.character(e1), as.character(e2)))

## shortcut to grep value = TRUE
#' @export
setGeneric("%gvi%", function(e1, e2) grep(as.character(e1), as.character(e2), value = TRUE, invert = TRUE))

## shortcut to gsub with replacement ""
#' @export
setGeneric("%s%", function(e1, e2) gsub(as.character(e1), "", as.character(e2)))

## shortcut to setdiff
#' @export
setGeneric("%-%", function(e1, e2) setdiff(e1, e2))

## shortcut to `c`
#' @export
setGeneric("%+%", function(e1, e2) c(e1, e2))

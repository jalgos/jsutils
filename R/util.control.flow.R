#' Print Call Stack
#'
#' Prints the entire call stack. Meant to be called upon an error occurring. Especially handy for the pbdR context where 'options(error = browser)' is unavailable.
#' @param ... not used
#' @export
print.call.stack <- function(...)
{
    dump.frames()
    print(last.dump)
}

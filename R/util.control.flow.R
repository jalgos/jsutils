
log.frames <- function(logger,
                       frames)
{
    jlog.error(logger, "Call stack upon error:", attributes(frames)$error.message)
    for(i in 1:length(frames))
    {
        jlog.error(logger, "Frame", i, ": `", names(frames)[i], "`")
    }
    jlog.error(logger, "Call stack upon error:", attributes(frames)$error.message)
}

#' Print Call Stack
#'
#' Prints the entire call stack. Meant to be called upon an error occurring. Especially handy for the pbdR context where 'options(error = browser)' is unavailable.
#' @param ... not used
#' @export
print.call.stack <- function(...,
                             logger = NULL)
{
    dump.frames()
    if(is.null(logger)) print(last.dump)
    else log.frames(logger, last.dump)
}


#' Print Call Stack Upon Error
#'
#' Prints the call stack and quits if the session is not interactive, sets option error to `recover` otherwise.
#' @param logger The logger that will print out the stack
#' @export
trace.upon.error <- function(logger)
{
    if(!interactive())
    { 
        options(error = function(...)
        {
            jsutils::print.call.stack(..., logger = logger)
            jlog.error(logger, "An error occurred. Aborting...")
            barrier()
            q('no')
        })
    }
    else
    {
        options(error = recover)
    }
}

#' @name conditions
#' @title Custom Conditions
#' @param subclass Class of the condition
#' @param parent.class Must be a subclass of "condition". "error", "warning" and "message" are valid
#' @param call Needed by condition
#' @param x condition object
#' @param ... to be added to the structure
#' @examples ## Custom conditions are caught with \code{tryCatch} or \code{withCallingHandlers} as follows:
#' tryCatch(stop(condition("mycondition", "Hello Worms!")),
#'          mycondition = function(cond) print("Yiiiihaaaa!"))
NULL

#' @describeIn conditions Taken as is form http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling. Creates a custom condition that can later on easily caught.
#' @export
condition <- function(subclass,
                      mess,
                      parent.class = character(0),
                      call = sys.call(-1),
                      ...)
{
    structure(class = c(subclass, c(parent.class, "condition")),
              list(message = mess, call = call),
              ...)
}

#' @describeIn conditions checks whether object is a condition
#' @export
is.condition <- function(x) inherits(x, "condition")

#' @describeIn conditions checks whether object is a condition
#' @export
is.error <- function(x) inherits(x, "error")

#' @describeIn conditions checks whether object is a condition
#' @export
is.warning <- function(x) inherits(x, "warning")

#' @describeIn conditions checks whether object is a condition
#' @export
is.message <- function(x) inherits(x, "message")

#' @describeIn conditions Creates a custom error
#' @export
custom.error <- function(...) condition(..., parent.class = "error")

#' @describeIn conditions Creates a custom warning
#' @export
custom.warning <- function(...) condition(..., parent.class = "warning")

#' @describeIn conditions Creates a custom message
#' @export
custom.message <- function(...) condition(..., parent.class = "message")

#' @describeIn conditions Useful error: dimension mismatch
#' @export
dimension.mismatch.error <- function()
{
    custom.error(subclass = "dimension.mismatch",
                 mess = "Dimension mismatch")
}

#' @describeIn conditions Useful error: NA or infinite values
#' @export
non.finite.error <- function()
{
    custom.error(subclass = "non.finite",
                 mess = "non finite error")
}

#' @describeIn conditions Useful error: Maximum number of iterations
#' @export
maxiter.error <- function()
{
    custom.error(subclass = "maxiter",
                 mess = "Maximum number of iterations reached")
}

#' @describeIn conditions Useful error: Model does not converge
#' @export
no.convergence.error <- function()
{
    custom.error(subclass = "no.convergence",
                 mess = "Model training didn't converge")
}

Rprof.memory.summary <- function(filename) 
{
    MNP <- readLines("mnist.prof")
    G <- grep.matches(pattern = ":(\\d*):(\\d*):(\\d*):(\\d*):(.*)", MNP)
    MEMT <- as.data.table(t(sapply(G[[2]], function(X) as.integer(X[1:4]))))
    setnames(MEMT, c("vsize.small", "vsize.large", "nodes", "duplications"))

}

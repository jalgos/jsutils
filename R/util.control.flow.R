
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

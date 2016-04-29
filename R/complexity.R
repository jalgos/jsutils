## Toolbox to measure function complexity regarding to an arbitray number of parameters"

#' @name complexity
#' @title Measuring function runtime
#'
#' @param fun Function for which you want to run the measure
#' @param par Parameters of the function
#' @param nrep Number of time the function will run
#' @param nms Names of the parameters
#' @param prefun Optional function to get the real parameters of the function from the parameters provided
#' @param par.down lower bounds of the parameters
#' @param par.up upper bounds of the parameters
#' @param par.nchunks Number of points in the grid. One entry per parameter.
#' @param par.names Names of the parameters
#' @param filter expression to filter out some prameters combinations
#' @param ... Arguments to be passed to average.time.fun and the function 'fun'
#' @param logger jlogger::JLogger to output info on the state of computations
NULL

#' @describeIn complexity Measure the time a function takes to run by running it several times
#' @export
average.time.fun <- function(fun,
                             par,
                             nrep,
                             nms = names(par),
                             prefun = function(...) list(...),
                             ...,
                             logger = jlogger::JLoggerFactory("Complexity"))
{
    if(!missing(nms))
    {
        jlogger::jlog.debug(logger, "Computing function runtime for:", paste(nms, unlist(par), sep = " = "), "nrep:", nrep)
        names(par) <- nms
    }
    else
        jlogger::jlog.debug(logger, "Computing function runtime for:", par, "nrep:", nrep)
    args <- do.call(prefun, par, ...)
    if(!is.list(args)) args <- list(args)
    T <- tryCatch(system.time(for(i in 1:nrep) do.call(fun, args))[3],
                  error = function(e){
                      jlogger::jlog.error(logger, "Following error occured during timing of function:", e$message)
                      c(elapsed = NaN)
                  })
    T / nrep
}

#' @describeIn complexity Computes runtime as a function of a set of parameters. It will produce a grid and run average.time.fun for each parameter combination.
#' @export
average.time.map <- function(par.down,
                             par.up,
                             par.nchunks,
                             par.names = names(par.down),
                             filter = TRUE,
                             ...,
                             logger = jlogger::JLoggerFactory("Complexity"))
{
    L <- mapply(par.down, par.up, par.nchunks, FUN = function(d, u, n) as.integer(seq(d, u, length.out = n)))
    names(L) <- par.names
    PARS <- create.cartesian.dt(L)[eval(filter)]
    jlogger::jlog.info(logger, "Computing:", nrow(PARS), "function evaluations")
    PARS[, average.time.fun(par = .BY, nms = names(PARS), ..., logger  = logger), by = names(PARS)]
}
                           
### Example ###

example <- function()
{
    fun.complexity(par.down = c(n = 10, N = 25),
                   par.up = c(n = 1000, N = 100000),
                   par.nchunks = c(30, 20),
                   nrep = 10,
                   fun = solve,
                   prefun = random.sparse.matrix,
                   filter = expression(N < n ^ 2))
}

example2 <- function()
{
    prefun <- function(n,
                       N1,
                       m,
                       N2)
    {        
        list(a = random.sparse.matrix(n = n, N = N1),
             b = random.sparse.matrix(nr = n, nc = m, N = N2))
    }
    fun.complexity(par.down = c(n = 10, N1 = 25, m = 1, N2 = 10),
                   par.up = c(n = 1000, N1 = 100000, m = 1000, N2 = 100000),
                   par.nchunks = c(15, 7, 7 ,7),
                   nrep = 10,
                   fun = solve,
                   prefun = prefun,
                   filter = expression(m < n & N2 < m ^ 2 & N1 < n ^ 2))

}

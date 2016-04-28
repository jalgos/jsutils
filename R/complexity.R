## Toolbox to measure function complexity regarding to an arbitray number of parameters"

one_run <- function(fun,
                    par,
                    nms,
                    nrep,
                    prefun = function(...) NULL,
                    ...,
                    logger = JLoggerFactory("Complexity"))
{
    jlog.debug(logger, "Computing function for:", paste(nms, unlist(par), sep = " = "), "nrep:", nrep)
    names(par) = nms
    args = do.call(prefun, par, ...)
    if(!is.list(args)) args = list(args)
    T <- tryCatch(system.time(for(i in 1:nrep) do.call(fun, args))[3],
                  error = function(e){
                      jlog.error(logger, "Following error occured during timing of function:", e$message)
                      c(elapsed = NaN)
                  })
    T / nrep
}

fun_complexity <- function(par_down,
                           par_up,
                           par_nchunks,
                           par_names = names(par_down),
                           filter = TRUE,
                           ...,
                           logger = JLoggerFactory("Complexity"))
{
    L = mapply(par_down, par_up, par_nchunks, FUN = function(d, u, n) as.integer(seq(d, u, length.out = n)))
    names(L) = par_names
    PARS = create_cartesian_dt(L)[eval(filter)]
    jlog.info(logger, "Computing:", nrow(PARS), "function evaluations")
    PARS[, one_run(par = .BY, nms = names(PARS), ..., logger  = logger), by = names(PARS)]
}
                           
### Example ###

example <- function()
{
    fun_complexity(par_down = c(n = 10, N = 25),
                   par_up = c(n = 1000, N = 100000),
                   par_nchunks = c(30, 20),
                   nrep = 10,
                   fun = solve,
                   prefun = random_sparse_matrix,
                   filter = expression(N < n ^ 2))
}

example2 <- function()
{
    prefun <- function(n,
                       N1,
                       m,
                       N2)
    {        
        list(a = random_sparse_matrix(n = n, N = N1),
             b = random_sparse_matrix(nr = n, nc = m, N = N2))
    }
    fun_complexity(par_down = c(n = 10, N1 = 25, m = 1, N2 = 10),
                   par_up = c(n = 1000, N1 = 100000, m = 1000, N2 = 100000),
                   par_nchunks = c(15, 7, 7 ,7),
                   nrep = 10,
                   fun = solve,
                   prefun = prefun,
                   filter = expression(m < n & N2 < m ^ 2 & N1 < n ^ 2))

}

#' Random Words
#'
#' Generate an arbitrary number of random strings made of letters from the 26 characters alphabet.
#' @param n Number of words to be generated
#' @param wsize parameter `size` of the binomial sample to create word lengths
#' @param wprob parameter `prob` of the binomial sample to create word lengths
#' @export
random.word <- function(n,
                        wsize = 100,
                        wprob = .1)
{
    sapply(rbinom(n, size = wsize, prob = wprob),
           function(wl) paste(sample(letters, wl, replace = TRUE), collapse = ''))
}

#' Random Dates
#'
#' Generate random dates around a reference date
#' @param n Sample size
#' @param date.ref Reference date
#' @param rdist distribution function to be used
#' @param ... To be forwarded to `rdist`
#' @export
random.date <- function(n,
                        date.ref = as.POSIXct(Sys.time()),
                        rdist = function(..., sd = 10000000) rnorm(..., sd = sd),
                        ...)
{
    date.ref + rdist(n, ...)
}

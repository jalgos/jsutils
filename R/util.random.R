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

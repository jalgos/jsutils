#' Atomic Type Hashing
#'
#' Hashes a vector of atomic type as determined by `typeof` (character, numeric, integer, logical) to an integer
#' @param x Vector to be hashed
#' @export
hash.atomic <- function(x)
{
    tp <- typeof(x)
    if(tp == 'character')
        hash_string_vector_std(x)
    else if(tp == 'double')
        hash_numeric_vector_std(x)
    else if(tp == 'integer')
        hash_integer_vector_std(x)
    else if(tp == 'logical')
        hash_logical_vector_std(x)
    else
        stop("Type is not atomic")
}

#' Combining Hash Numbers
#'
#' Combines two hash numbers into one
#' @param h1 Vector of hash numbers
#' @param h2 Vector of hash numbers
#' @export
hash.combine <- function(h1,
                         h2)
{
    hash_combine(h1,
                 h2)
}

#' String Hashing
#'
#' Hashes a string into an integer.
#' @param x String vector to be hashed
#' @export
hash.string.vector <- function(x)
{
    hash_string_vector(x)
}

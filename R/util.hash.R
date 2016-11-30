#' Atomic Type Hashing
#'
#' Hashes a vector of atomic type (character, numeric, integer, logical, + factor) to an integer
#' @param x Vector to be hashed
#' @export
hash.atomic <- function(x)
{
    if(is.character(x))
        hash_string_vector_std(x)
    else if(is.numeric(x))
        hash_numeric_vector_std(x)
    else if(is.integer(x))
        hash_integer_vector_std(x)
    else if(is.logical(x))
        hash_logical_vector_std(x)
    else if(is.factor(x))
        hash_integer_vector_std(x)
    else
        stop("Type is not atomic")
}

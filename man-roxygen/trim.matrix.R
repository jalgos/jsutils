#' @param S The Matrix
#' @param sqDl square root of the diagonal values of the matrix that will be mutltiplied to S from the left side 
#' @param sqDr square root of the diagonal values of the matrix that will be mutltiplied to S from the right side 
#' @param ... Not used for now
#' @param mtype Character vector describing the type of matrix. Used to log information about the process
#' @param tol tolerance for the correlation values. Any values under this value will be trimmed out
#' @param tol.inv tolerance for inverting a diagonal value. If below this value the entry will be considered null
#' @param logger JLogger

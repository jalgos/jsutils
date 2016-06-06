#' Removing Least Significant Covariance Coefficients
#'
#' Will reduce the size of a covariance matrix by removing its least significant coefficients. The trimming will happen in the correlation matrix in order not to overweight the entries with the biggest variance.
#' @param S The covariance matrix

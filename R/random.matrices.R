########## Random Cov matrices ##########

#' @name random.mat
#' @title Generating random matrices
#' 
#' @param n Number of rows
#' @param p Number of columns
#' @param tot.vol Scaling that control the total volatiliy of the covariance matrix created
#' @param rgen Distribution to draw from. Defaults to a normal with 0 mean and variance 1
#' @param ... Parameters to be passed to the random number generator and to lower level functions
NULL

#' @describeIn random.mat Generates a random matrix of independently drawn realisations of a random variable
#'
#' @export
random.matrix <- function(n,
                          p = n,
                          rgen = rnorm,
                          ...)
{
    matrix(rgen(n * p, ...), n, p)
}

#' @describeIn random.mat Generates a random symmetric matrix of independently drawn realisations of a random variable
#' @export
random.sym.matrix <- function(n,
                              rgen = rnorm,
                              ...)
{
    X <- rgen(n * (n + 1) / 2, ...)
    Matrix::forceSymmetric(vech.reverse(X))
}


#' @describeIn random.mat Generates a random anti symmetric matrix of independently drawn realisations of a random variable
#' @export
random.anti.sym.matrix <- function(n,
                                   rgen = rnorm,
                                   ...)
{
    X <- rgen(n * (n - 1) / 2, ...)
    vech.reverse(X, keep.diag = FALSE, symmetric = FALSE)
}

#' @describeIn random.mat Generates a random covariance matrix. Uses random.sym.matrix
#' @export
random.cov.matrix <- function(n,
                              tot.vol = n,
                              ...)
{
    if(n == 1) R <- Matrix(rexp(n))
    else
    {        
        M <- random.sym.matrix(n, ...)
        R <- M %*% t(M)
    }
    R / total.vol(R) * tot.vol
}

####### Random sparse matrixes #######
#' @describeIn random.mat Creates a random sparse matrix
#' @export
random.sparse.matrix <- function(n,
                                 p = n,
                                 N,
                                 rgen = rnorm)
{
    Matrix::sparseMatrix(i = sample(1:n, N, replace = TRUE),
                         j = sample(1:p, N, replace = TRUE),
                         x = rgen(N),
                         dims = c(n, p))
}

#' @describeIn random.mat Creates a random sparse covariance matrix. Sparsity is hard to control exactly. The target is approximate. Uses random.sparse.matrix
#' @export
random.sparse.cov.matrix <- function(N,
                                     ...)
{
    
    M <- random.sparse.matrix(N = as.integer(sqrt(N)), ...)
    M %*% t(M)
}


####### Random orthonormal matrices ######

#' @describeIn random.mat Creates a random orthonormal.matrix. Effectively draws an anti symmetric matrix and uses the Cayley parametrization to create the member of SOn(R)
#' @export
random.orthonormal.matrix <- function(n)
{
    A <- random.anti.sym.matrix(n)
    jsmath::reverse.cayley(A)
}
                                      

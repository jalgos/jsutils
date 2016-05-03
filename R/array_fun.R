## TODO sort out what to do with that
## Assume three dimensional array of matrices with last index being the time
array_prod <- function(A, B)
{
    dA <- dim(A)
    dB <- dim(B)
    if(dA[2] != dB[1]) stop("dimension mismatch")
    if(dA[3] != dB[3]) stop("length mismatch")
    dC <- c(dA[1], dB[2], dB[3])
    if(!is.null(dimnames(A)) & ! is.null(dimnames(B))) dmnC = list(dimnames(A)[[1]], dimnames(B)[[2]], dimnames(B)[[3]])
    else dmnC = NULL
    C <- array(0, dim = dC, dimnames = dmnC)
    for(i in 1:dA[1])
    {
        for(j in 1:dB[2])
        {
            for(k in 1:dB[1])
            {
                C[i, j, ] <- C[i, j, ] + A[i, k, ] * B[k, j, ]
            }
        }
    }
    return(C)
}

array_array_prod <- function(x, y, margin1 = 3, margin2 = 3)
{
    array(sapply(1:dim(x)[3], function(i) x[, , i] %*% y[, , i]), dim = c(dim(x)[1], dim(y)[2], dim(x)[3]))
}

array_matrix_prod <- function(x, y)
{
    apply(x, function(M) x %*% y, dim = c(dim(x[1], dim(y[2]), dim(x)[3])))
}

matrix_array_prod <- function(x, y)
{
    apply(y, function(M) x %*% y, dim = c(dim(x[1], dim(y[2]), dim(x)[3])))
}

array_transpose <- function(x)
{
    array(apply(x, 3, t), dim = dim(x))
}

setGeneric("array_prod", function(x, y) x %*% y)
#setMethod("array_prod", c("array", "array"), array_array_prod)
#setMethod("array_prod", c("array", "genMatrix"), array_matrix_prod)
#setMethod("array_prod", c("genMatrix", "array"), matrix_array_prod)
#setMethod("t", "array", array_transpose)

array_rbind <- function(A, B)
{
    dA = dim(A)
    dB = dim(B)
    if(dA[2] != dB[2]) stop("dimension mismatch")
    if(dA[3] != dB[3]) stop("length mismatch")
    dC = c(dA[1] + dB[1], dB[2], dB[3])
    if(!is.null(dimnames(A)) & !is.null(dimnames(B)))
    {
        dmnA = dimnames(A)
        dmnB = dimnames(B)
        dmnC = list(c(dmnA[[1]], dmnB[[1]]),  dmnB[[2]], dmnB[[3]])
    }
    else dmnC = NULL
    C = array(0, dim = dC, dimnames = dmnC)
    C[1:dA[1], ,] = A
    C[dA[1] + 1:dB[1], ,] = B
    C
}

array_cbind <- function(A, B)
{
    dA = dim(A)
    dB = dim(B)
    if(dA[1] != dB[1]) stop("dimension mismatch")
    if(dA[3] != dB[3]) stop("length mismatch")
    dC = c(dA[1], dA[1] + dB[2], dB[3])
    if(!is.null(dimnames(A)) & !is.null(dimnames(B)))
    {
        dmnA = dimnames(A)
        dmnB = dimnames(B)
        dmnC = list(dmnA[[1]], c(dmnA[[2]],  dmnB[[2]]), dmnB[[3]])
    }
    else dmnC = NULL
    C = array(0, dim = dC, dimnames = dmnC)
    C[, 1:dA[2], ] = A
    C[, dA[2] + 1:dB[2], ] = B
    C
}

##Vec operator on an array of matrix
vec_array <- function(A)
{
     matrix(A, dim(A)[1] * dim(A)[2], dim(A)[3])
}

#' 
array.prod <- function(A, B)
{
    dA <- dim(A)
    dB <- dim(B)
    if(dA[2] != dB[1]) stop("dimension mismatch")
    if(dA[3] != dB[3]) stop("length mismatch")
    dC <- c(dA[1], dB[2], dB[3])
    if(!is.null(dimnames(A)) & ! is.null(dimnames(B))) dmnC = list(dimnames(A)[[1]], dimnames(B)[[2]], dimnames(B)[[3]])
    else dmnC <- NULL
    C <- array(0, dim = dC, dimnames = dmnC)
    for(i in 1:dA[1])
    {
        for(j in 1:dB[2])
        {
            for(k in 1:dB[1])
            {
                C[i, j, ] <- C[i, j, ] + A[i, k, ] * B[k, j, ]
            }
        }
    }
    return(C)
}

array.transpose <- function(A)
{
    dA <- dim(A)
    dC <- c(dA[2], dA[1], dA[3])
    if(!is.null(dimnames(A)))
    {
        dmnA <- dimnames(A)
        dmnC <- list(dmnA[[2]], dmnA[[1]], dmnA[[3]])
    }
    else dmnC <- NULL
    C <- array(0, dim = dC, dimnames = dmnC)
    for(i in 1:dA[1])
    {
        for(j in 1:dA[2])
        {
            C[j, i, ] <- A[i, j,]
        }
    }
    C
}

array.rbind <- function(A, B)
{
    dA <- dim(A)
    dB <- dim(B)
    if(dA[2] != dB[2]) stop("dimension mismatch")
    if(dA[3] != dB[3]) stop("length mismatch")
    dC <- c(dA[1] + dB[1], dB[2], dB[3])
    if(!is.null(dimnames(A)) & !is.null(dimnames(B)))
    {
        dmnA <- dimnames(A)
        dmnB <- dimnames(B)
        dmnC <- list(c(dmnA[[1]], dmnB[[1]]),  dmnB[[2]], dmnB[[3]])
    }
    else dmnC <- NULL
    C <- array(0, dim = dC, dimnames = dmnC)
    C[1:dA[1], ,] <- A
    C[dA[1] + 1:dB[1], ,] <- B
    C
}

array.cbind <- function(A, B)
{
    dA <- dim(A)
    dB <- dim(B)
    if(dA[1] != dB[1]) stop("dimension mismatch")
    if(dA[3] != dB[3]) stop("length mismatch")
    dC <- c(dA[1], dA[1] + dB[2], dB[3])
    if(!is.null(dimnames(A)) & !is.null(dimnames(B)))
    {
        dmnA <- dimnames(A)
        dmnB <- dimnames(B)
        dmnC <- list(dmnA[[1]], c(dmnA[[2]],  dmnB[[2]]), dmnB[[3]])
    }
    else dmnC <- NULL
    C <- array(0, dim = dC, dimnames = dmnC)
    C[, 1:dA[2], ] <- A
    C[, dA[2] + 1:dB[2], ] <- B
    C
}

#Vec operator on an array of matrix
vec.array <- function(A)
{
     matrix(A, dim(A)[1] * dim(A)[2], dim(A)[3])
}


setClassUnion("genMatrix", c("matrix", "Matrix"))

## obsolete drop = FALSE does the trick
subvector <- function(V, k = 1:length(V))
{
    if(missing(k)) k <- 1:length(V)
    return(V[k])
}

## DITTO
submatrix <- function(M, i = 1:nrow(M), j = 1:ncol(M))
{
    if(missing(i)) i <- 1:nrow(M)
    if(missing(j)) j <- 1:ncol(M)
    if(is.logical(i)) i <- which(i)
    if(is.logical(j)) j <- which(j)
    if(length(i) == 1 || length(j) == 1)
    {
        rn <- rownames(M)
        cn <- colnames(M)
        return(Matrix(M[i, j], length(i), length(j), dimnames = list(rn[i], cn[j])))
    }
    return(M[i, j])
}

## DITTO
#For 3 dimensional arrays
subarray <- function(A,  i = 1:dim(A)[1], j = 1:dim(A)[2], k = 1:dim(A)[3])
{
    if(missing(i)) i <- 1:dim(A)[1]
    if(missing(j)) j <- 1:dim(A)[2]
    if(missing(k)) k <- 1:dim(A)[3]
    if(is.logical(i)) i <- which(i)
    if(is.logical(j)) j <- which(j)
    if(is.logical(k)) k <- which(k)
    if(length(i) == 1 || length(j) == 1 || length(k) == 1)
    {
        if(!is.null(dimnames(A)))
        {
            inames <- dimnames(A)[[1]][i]
            jnames <- dimnames(A)[[2]][j]
            knames <- dimnames(A)[[3]][k]
            ndimn <- list(inames, jnames, knames)
        }
        else
        {
            ndimn <- NULL
        }
        return(array(A[i, j, k], dim = c(length(i), length(j), length(k)), dimnames = ndimn))
    }
    return(A[i, j, k])
}

## Assume three dimensional array of matrices with last index being the time

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

#' @name matrix.norm
#' @title Norms of a Matrix
#' @param M a matrix
#' @param M1 a matrix
#' @param M2 a matrix
#' @param normalized Should the scalar product be normalized to be expressed as a fraction of the norm of 'M1'.
#' @details Simple wrappers around the norm function.
NULL

#' @describeIn matrix.norm L2 norm (frobenius norm)
#' @export
matrix.norm <- function(M)
{
    norm(M, "f")
}

#' @describeIn matrix.norm Infinite norm (max of the absolute value of the coefficients)
#' @export
matrix.norm.inf <- function(M)
{
    norm(M, "i")
}

#' @describeIn matrix.norm scalar product of matrices. Identitical to the scalar product of the vectorized form of the matrices
#' @export
matrix.scal <- function(M1, M2, normalized = FALSE)
{
    if(normalized) N <- matrix.norm(M1)
    else N <- 1
    return(sum(M1 * M2) / N ^ 2)
}

##Equivalent to Matrix's norm(M, "f")
#' Norm
#' 
#' Generic for vectors
#' @export
setMethod("norm", c("vector", "ANY"), function(x, type) norm(as.matrix(x), type))

#' Trace of a matrix
#' @export
matrix.trace <- function(M) sum(diag(M))

######## Rewriting some utility functions #####
##safe for sparse matrices
gen.vech <- function(M,
                     keep.diag = TRUE)
{    
    if(nrow(M) == 0) return(Matrix(0, 0, 0))
    size <- nn12(nrow(M), keep.diag = keep.diag)
    dms <- c(size, 1)
    if(isDiagonal(M))
    {
        if(size <= .Machine$integer.max)
        {
            if(!keep.diag)
                return(Matrix(0, size, 1))
            return(sparseMatrix(i = index.sym(1:nrow(M), 1:nrow(M), nrow(M)), j = rep(1, nrow(M)), x = diag(M)))
        }
        else
        {
            if(!keep.diag)
                return(STMatrix(DM = data.table(i = numeric(0), j = numeric(0), x = numeric(0)),
                                dims = dms))
            return(STMatrix(DM = data.table(i = index.sym(1:nrow(M), 1:nrow(M), nrow(M)), j = rep(1, nrow(M)), x = diag(M)),
                            dims = dms))
        }
    }
    M <- as(M, "TsparseMatrix")
    if(keep.diag) F <- M@i <= M@j
    else F <- M@i < M@j
    N <- sum(F)
    if(all(dms <= .Machine$integer.max))
        sparseMatrix(i = index.sym(M@i[F] + 1, M@j[F] + 1, nrow(M), keep.diag = keep.diag),
                     j = rep(1, N),
                     x = M@x[F],
                     dims = dms)
    else
        STMatrix(DM = data.table(i = index.sym(M@i[F] + 1, M@j[F] + 1, nrow(M), keep.diag = keep.diag),
                                 j = rep(1, N),
                                 x = M@x[F]),
                 dims = dms)
}


#' @name vectorization
#' @title Vectorization
#' @param M Matrix to vectorize
#' @param nv length of the vectorization
#' @param V The vectorization
#' @param half.vec Is it a vectorization or a half vectorization
#' @param check.integer Should the result be an integer
#' @param n the number of rows of the symmetric matrix
#' @param keep.diag Is the diagonal kept in the half vectorization
#' @param symmetric Is the matrix symmetric
NULL

#' Vectorization of block diagonal matrix
#'
#' Creates the matrix that transforms the concatenation of each blocks half vectorization into the a half vectorization of the entire matrix
#' @param vdim vector of size of each block
#' @param logger JLogger to be used to log messages
#' @seealso diag.to.vech, diag.to.vec, bdiag.to.vec
#' @export
bdiag.to.vech <- function(vdim, #vector of matrix dimensions
                          logger = JLoggerFactory("jalgos filter"))
{
    fmat <- sparseMatrix
    vds <- cumsum(vdim)
    N <- last(vds)
    ND <- sum(nn12(vdim))
    jlog.debug(logger, "Creating a bdiag transition matrix of dimension", nn12(N), ND, "with:", length(vds), "blocks")
    Ivd <- lapply(vdim, function(n) rep((1 - n):0, each = n))
    Jvd <- lapply(vdim, function(n) rep((1 - n):0, n))
    I <- unlist(mapply(Ivd, vds, FUN = "+", SIMPLIFY = FALSE))
    J <- unlist(mapply(vds, Jvd, FUN = "+", SIMPLIFY = FALSE))
    F <- I <= J
    I <- I[F]
    J <- J[F]
    Ibd <- 1:ND
    Jbd <- index.sym(I, J, N)
    dims <- c(nn12(N), ND)
    if(any(is.na(as.integer(dims)))) fmat <- HugeMatrix
    fmat(i = Jbd, j = Ibd, x = 1, dims = dims)
}

#' Vectorization of a diagonal matrix
#'
#' Creates the matrix that transforms the vector of diagonal value into the half vectorization of the whole matrix
#' @param n Size of the diagonal matrix
#' @param logger JLogger to be used to log messages
#' @seealso diag.to.vec, bdiag.to.vec, bdiag.to.vec
#' @export
diag.to.vech <- function(n,
                         logger = JLoggerFactory("jalgos filter"))
{
    bdiag.to.vech(rep(1, n),
                  logger = logger)
}

#' Half vectorization
#'
#' Performs the half vectorization of a symmetric matrix.
#' @details Half vectorization can be done in a pretty straightforward fashion for sparse matrices. The solution here preserves sparsity which makes sure that the program will not run out of memory because of an unwanted conversion to a dense matrix
#' @references https://en.wikipedia.org/wiki/Vectorization_\%28mathematics\%29#Half-vectorization
#' @export
setGeneric("vech", function(M, ...) standardGeneric("vech"))

#' @rdname vech
#' @export
setMethod("vech", "genMatrix", gen.vech)

#' Assigning data by index value pair
#' @export
assign.array.vect <- function(M, I, values, dimn = dim(M))
{
    dimn <- cumprod(c(1, dimn[ -length(dimn)]))
    M[as.vector(1 + (I - 1) %*% dimn)] <- values
    M
}

## For half vectorization
## n (n + 1) = 2X
## (n + 1/2)² - 1 / 4 = 2X
## n = sqrt(2X + 1 / 4) - 1 / 2
## TODO add the keep.diag option

#' @describeIn vectorization Finds the size of the matrix corresponding to the length of its vectorization
#' @export
findN <- function(nv,
                  half.vec = TRUE,
                  check.integer = TRUE)
{
    if(half.vec) n <- sqrt(2 * nv + 1/4) - 1/2
    else n <- sqrt(nv)
    if(check.integer & abs(round(n) - n) > sqrt(.Machine$double.eps))
    {
        stop("The number supplied does not correspond to a vectorization")
    }
    n
}

## Going from a matrix that operates on vec to a matrix that operates on vech

## Function that will transform a vech(n,n) to a vec(n,n)

## We need this one a lot

#' @describeIn vectorization Computes the length of the half vectorization. n (n + 1) / 2 if we keep the diagonal and n (n - 1) / 2
#' @export
nn12 <- function(n, keep.diag = TRUE)  n * (n - 1 + 2 * keep.diag) / 2

#' @name mat.index
#' @title Matrix index to vectorization index
#' @param i row index
#' @param j column index
#' @param n number of rows
#' @param I Half vectorization index
#' @param keep.diag is the diagonal kept in the half vectorization
NULL

#' @describeIn mat.index Computes the vectorization index corresponding to a given row column combination in a matrix
#' @export
mat.index <- function(i, j, n) (j - 1) * n + i

#' @describeIn mat.index Computes the half vectorization index corresponding to a given row column combination in a matrix
#' @export
index.sym <- function(i, j, n, keep.diag = TRUE)
{
    o <- i
    i <- pmin(i, j)
    j <- pmax(j, o)
    adj <- !keep.diag
    (i - 1) * (n - adj) + (j - 1) - i * (i - 1) / 2  - adj + 1
}

#' @describeIn mat.index Computes the row index and column index corresponding to a given vectorization index
#' @export
reverse.index.sym <- function(I, n, keep.diag = TRUE)
{
    if(length(I) == 0) return(list(i = integer(0), k = integer(0)))
    di <- 1:n
    adj <- !keep.diag
    d <- index.sym(di, di + adj, n, keep.diag = keep.diag)
    D <- data.table(i = di, d = d, I = d)
    U <- data.table(or = 1:length(I), I)
    setkey(D, I)
    setkey(U, I)
    U <- D[U, roll = Inf]
    U[order(or), list(i, j = I - d + i + adj)]
}


#' @describeIn vectorization Matrix to go from half vectorization to full vectorization
#' @export
vech.to.vec <- function(n,
                        keep.diag = TRUE)
{
    fmat <- sparseMatrix
    if(n < 0L)
    {
        stop("n must be a positive integer")
    }
    
    if(n == 0L)
    {
        return(Matrix(0, 0, 0))
    }
    
    I <- rep(1:n, n)
    J <- rep(1:n, each = n)
    val <- 1
    if(!keep.diag)
    {
        F <- I != J
        I <- I[F]
        J <- J[F]
        val <- ifelse(I < J, 1, -1)
    }
    ID1 <- (J - 1) * n + (I - 1) + 1
    ID2 <- index.sym(I, J, n, keep.diag = keep.diag)
    dims <- c(n ^ 2, nn12(n, keep.diag = keep.diag))
    if(any(is.na(as.integer(dims)))) fmat = HugeMatrix
    fmat(i = ID1, j = ID2, x = val, dims = dims)
}

## The symmetric
#' @describeIn vectorization Gets the indices of the upper diagonal
#' @export
get.upper.indices <- function(n,
                              keep.diag = TRUE)
{
    I <- unlist(mapply(function(i, j) rep(i, j), 1:n, n:1))
    J <- unlist(sapply(1:n, function(i, n) i:n, n = n))
    if(!keep.diag)
    {
        F <- I < J
        I <- I[F]
        J <- J[F]
    }
    list(I = I, ID = (J - 1) * n + (I - 1) + 1, J = J)
}

#' @describeIn vectorization Matrix to go from full vectorization to half vectorization
#' @export
vec.to.vech <- function(n, keep.diag = TRUE)
{
    fmat <- sparseMatrix
    if ( n < 0L )
    {
        stop("n must be a positive integer")
    }
    
    if( n == 0L)
    {
        return(Matrix(0, 0, 0))
    }

    IUT <- get.upper.indices(n, keep.diag = keep.diag)
    ID2 <-  index.sym(IUT$I, IUT$J, n, keep.diag = keep.diag)
    dims <- c(nn12(n, keep.diag = keep.diag), n ^ 2)
    if(any(is.na(as.integer(dims)))) fmat <- HugeMatrix
    fmat(i = ID2, j = IUT$ID, x = 1, dims = dims)    
}

#' @describeIn vectorization Takes a half vectorization and builds the corresponding matrix of class \code{Matrix}
#' @export
vech.reverse <- function(V,
                         keep.diag = TRUE,
                         symmetric = TRUE) ## if not symmetric then antisymmetric
{
    if(is.vector(V)) V <- Matrix(V)
    DV <- mat.to.data.table(V)
    nd <- findN(nrow(V)) + !keep.diag
    DV[, c("i", "j") := rev.index.sym(i, nd, keep.diag = keep.diag)]
    if(symmetric) return(drop0(dsparseMatrix(DV[, list(i = i, j = j, x =  x)], symmetric = TRUE, dims = c(nd, nd))))
    M <- drop0(dsparseMatrix(DV[, list(i = i, j = j, x = x)], dims = c(nd, nd)))
    M - t(M)
}

## Full Vectorization
gen.vec <- function(M,
                    ...)
{
    if(is(M, "denseMatrix")) M <- as.matrix(M)
    N <- prod(dim(M))
    dim(M) <- c(N, 1)#Keep it sparse
    M    
}

gen.vec.reverse <- function(V,
                            ...)
{
    if(is.vector(V)) V <- Matrix(V, length(V))
    n <- findN(nrow(V), FALSE)
    V <- Matrix(V)
    dim(V) <- c(n, n)
    V
}

vec.reverse.dsy <- function(V)
{
    n <- findN(nrow(V), FALSE)
    V <- as(as.matrix(V), "dgeMatrix")
    dim(V) <- c(n, n)
    forceSymmetric(V)
}

#' Full vectorization
#'
#' Vectorizes a matrix
#' @references https://en.wikipedia.org/wiki/Vectorization_\%28mathematics\%29
#' @export
setGeneric("vec", gen.vec)

#' Full vectorization
#'
#' Builds the matrix corresponding to a vectorization
#' @export
setGeneric("vec.reverse", gen.vec.reverse)

#' Vectorization of block diagonal matrix
#'
#' Creates the matrix that transforms the concatenation of each blocks half vectorization into the a vectorization of the entire matrix
#' @param vdim vector of size of each block
#' @param logger JLogger to be used to log messages
#' @seealso diag.to.vech, diag.to.vec, bdiag.to.vech
#' @export
bdiag.to.vec <- function(vdim, #vector of matrix dimensions
                         logger = JLoggerFactory("jalgos filter"),
                         ...)
{
    fmat <- sparseMatrix
    vds <- cumsum(vdim)
    N <- last(vds)
    ND <- sum(vdim ^ 2)
    jlog.debug(logger, "Creating a bdiag transition matrix of dimension", N ^ 2, ND, "with:", length(vds), "blocks")
    Ivd <- lapply(vdim, function(n) rep((1 - n):0, each = n))
    Jvd <- lapply(vdim, function(n) rep((1 - n):0, n))
    I <- unlist(mapply(Ivd, vds, FUN = "+", SIMPLIFY = FALSE))
    J <- unlist(mapply(vds, Jvd, FUN = "+", SIMPLIFY = FALSE))
    I <- I
    J <- J
    Ibd <- 1:ND
    Jbd <- mat.index(I, J, N)
    dims <- c(N ^ 2, ND)
    if(any(is.na(as.integer(dims)))) fmat <- HugeMatrix
    fmat(i = Jbd, j = Ibd, x = 1, dims = dims)
}

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
#'
#' @export
random.sym.matrix <- function(n,
                              rgen = rnorm,
                              ...)
{
    X <- rgen(n * (n+1) / 2, ...)
    forceSymmetric(vech.reverse(X))
}

#' @describeIn random.mat Generates a random covariance matrix. Uses random.sym.matrix
#'
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
    sparseMatrix(i = sample(1:n, N, replace = TRUE),
                 j = sample(1:p, N, replace = TRUE),
                 x = rgen(N),
                 dims = c(n, p))
}
#' @describeIn random.mat Creates a random sparse covariance matrix. Sparsity is hard to control exactly. The target is approximate. USes random.sparse.matrix
#' @export
random.sparse.cov.matrix <- function(N,
                                     ...)
{
    
    M <- random.sparse.matrix(N = as.integer(sqrt(N)), ...)
    M %*% t(M)
}

## More utility functions
#' @name eigen.func
#' @title Applying functions to the eigen value
#' @param M Matrix on which we want to apply func
#' @param func Function to apply to the eigen values of M
#' @param D eigen values
#' @param ... Parameters to pass on to func
NULL
#' @describeIn eigen.func Applies a function to the eigen decomposition of a matrix in this fashion: let M = Pdiag(V)P^-1 then the result is Q = Pdiag(f(V))P^-1 where f is a function that is applied to the vector of eigen values.
#' @export
funcM <- function(M,
                  func,
                  ...)
{
    n <- nrow(M)
    E <- eigen(M, symmetric = TRUE)
    V <- Matrix(E$vectors)
    D <- E$values
    V %*% Diagonal(n, x = func(D, ...)) %*% t(V)
}

#' @describeIn eigen.func The function applied is the square root. A tolerance can be given to consider negative values close to 0 to be 0
#' @export
sqrtM <- function(M,
                  eps = .Machine$double.eps)
{
    funcM(M, function(D) sqrt(ifelse(D > -eps & D < 0, 0, D)))
}

#' @describeIn eigen.func Absolute value of the eigen values
#' @export
absM <- function(M) funcM(M, func = add.ellipsis(abs))

#' @describeIn eigen.func Positive value
#' @export
posD <- function(D,
                 ...)
{
    ifelse(D > 0, D, 0)
}

#' @describeIn eigen.func Positive value of the eigen values. In case of a symmetric matrix, it will turn it into a semi-definite positive matrix.
#' @export
posM <- function(M, ...) funcM(M, func = posD, ...)

#' @describeIn eigen.func Exponential of the eigen values. Computes the exponential of a matrix. It is however not the most efficient manner.
#' @export
expM <- function(M, ...) funcM(M, func = add.ellipsis(exp))

#' @describeIn eigen.func Computes the logarithm of a matrix. Best defined in the case of definite positive matrices
#' @export
logM <- function(M, ...) funcM(M, func = add.ellipsis(log))

#' @name with.names
#' @title More verbose factorization
#' @param M matrix to factorize
#' @details Base methods to factorize matrices, namely \code{eigen} and \code{svd} discard the names of the matrix in their results. These methods add them back.
NULL

#' @rdname with.names
#' @export
eigen.with.names <- function(M, ...)
{
    E <- eigen(M, ...)
    rownames(E$vectors) <- rownames(M)
    E
}

#' @rdname with.names
#' @export
svd.with.names <- function(M, ...)
{
    S <- svd(M, ...)
    rownames(S$u) <- rownames(M)
    rownames(S$v) <- colnames(M)
    S
}

shift.mat <- function(X, N = 1, ...)
{
    if(N == 0) return(X)
    L <- nrow(X)
    P <- ncol(X)
    padding <- matrix(nrow = abs(N), ncol = P)
    I <- 1:L
    keep <- submatrix(X, I + N > 1 & I + N < L + 1)
    if(N > 0) rBind(padding, keep)
    else rBind(keep, padding)
}

#' Shifting Matrices
#' @export
setMethod("shift", signature = c("matrix"), shift.mat)

#' Shifting Matrices
#' @export
setMethod("shift", signature = c("Matrix"), shift.mat)

util.resize <- function(M,
                        nrows,
                        ncols,
                        padding.val = 0,
                        zero.based = FALSE,
                        ...)
{
    orows <- nrow(M)
    ocols <- ncol(M)
    rdiff <- nrows - orows
    cdiff <- ncols - ocols
    I <- 1:nrows
    J <- 1:ncols
    if(zero.based)
    {
        I <- I - 1
        J <- J - 1
    }
    if(rdiff < 0 ) M <- M[I, ]
    else if(rdiff > 0) M <- row.bind(M, Matrix(padding.val, rdiff, ocols))

    if(cdiff < 0) M <- M[, J]
    else if(cdiff > 0) M <- col.bind(M, Matrix(padding.val, nrows, cdiff))
    M
}

#' @name resize.reserve
#' @title Resizing, reserving
#' @details Some objects are meant to be growing as some algorithms run. These functions provides feature to handle these cases
NULL

#' Resize, reserve, binds
#'
#' Resizes a buffer. If reserve was called earlier no copy should be made
#' @template reserve
#' @export
setGeneric("resize", util.resize)

#' Resize, reserve, binds
#'
#' Reserves additional data, so when more data is inserted no copy is dones
#' @template reserve
#' @export
setGeneric("reserve", util.resize)

#' Resize, reserve, binds
#' 
#' Binds two data structures so that the number of rows of the result is the sum of the number of rows of the two arguments. Columns have to match.
#' @template reserve
#' @export
setGeneric("row.bind", function(M1, M2) rBind(M1, M2))

#' Resize, reserve, binds
#' 
#' Binds two data structures so that the number of columns of the result is the sum of the number of columns of the two arguments. Number of rows has to be te same
#' @template reserve
#' @export
setGeneric("col.bind", function(M1, M2) cBind(M1, M2))

#' @name rows.cols
#' @title Row numbers and column numbers
NULL
#' @describeIn rows.cols returns a vector containing all the row nmbers
#' @export
rows <- function(M)
{
    if(nrow(M) > 0) 1:nrow(M)
    else integer(0)
}

#' @describeIn rows.cols returns a vector containing all the column numbers
#' @export
cols <- function(M)
{
    if(ncol(M) > 0) 1:ncol(M)
    else integer(0)
}

Matrix.ginv <- function(X, tol = sqrt(.Machine$double.eps)) 
{
    Id <- Diagonal(nrow(X))
    solve(X + tol * Id)
}

#' Generalized inverse
#' 
#' Computes the generalized inverse for any type of matrix: rectangular and not full rank.
#' @param X Matrix to invert
#' @param tol espsilon bump to force full rank
#' @export
setGeneric("ginv", MASS::ginv)

#' @rdname ginv
#' @export
setMethod("ginv", "Matrix", Matrix.ginv)

#' Generalized inverse
#' 
#' Computes a solution of the a.v = b for any type of matrix: rectangular and not full rank.
#' @param a Matrix to invert
#' @param b RHS of the equation a.v = b where v is the unknown
#' @export
setGeneric("gsolve", function(a, b, ...) ginv(a) %*% b)

#' @rdname gsolve
#' @export
setMethod("gsolve", c(a = "Matrix", b = "ANY"), function(a, b, eps = sqrt(.Machine$double.eps)) solve(a + eps * Diagonal(n = nrow(a)), b))

#' @describeIn gen.inv Inverts a matrix by truncating the eigen values that contribute to 95% of the whole volatility.
#' @export
psolve <- function(a,
                   b,
                   var.thresh = .95)
{
    S <- svd(a)
    PVE <- cumsum(S$d) / sum(S$d)
    ico <- which(PVE > var.thresh)[1]
    IP <- 1:ico
    M1 <- S$v[, IP] %*% Diagonal(x = 1 / S$d[1:ico]) %*% t(S$u[, IP])
    if(missing(b)) return(M1)
    M1 %*% b
}

mtdt.add.row.col.names <- function(D,
                                   M,
                                   one.based)
{
    if(!is.null(rownames(M)))
    {
        D[, i.names := rownames(M)[i + !one.based]]
    }
    
    if(!is.null(colnames(M)))
    {
        D[, j.names := colnames(M)[j + !one.based]]
    }
    
    D    
}

Matrix.mat.to.data.table <- function(M,
                                     one.based = TRUE,
                                     with.names = FALSE)
{
    TM <- as(M, "TsparseMatrix")
    D <- data.table(i = TM@i + one.based, j = TM@j + one.based, x = TM@x)
    if(with.names)
        mtdt.add.row.col.names(D,
                               M,
                               one.based)
    else
        D
}

matrix.mat.to.data.table <- function(M,
                                     one.based = TRUE,
                                     with.names = FALSE)
{
    D <- data.table(i = as.vector(row(M)) - !one.based, j = as.vector(col(M)) - !one.based, x = as.vector(M))
    if(with.names)
        mtdt.add.row.col.names(D,
                               M,
                               one.based)
    else
        D
}
    
sym.mat.to.data.table <- function(M,
                                  one.based = TRUE,
                                  ...)
{
    D <- callNextMethod(M, one.based, ...)
    if(nrow(D[i > j]) == 0)
    {
        D <- rbind(D, D[i < j, list(i = j, j = i, x)])
    }
    else if(nrow(D[i < j]) == 0)
    {
       D <- rbind(D, D[i > j, list(i = j, j = i, x)])
    }
    D
}

#' Matrix to data table
#'
#' Transforms a matrix into its triplet version in a data.table
#' @param M Matrix to transform to its triplet form
#' @param one.based Indices start at 1 or 0
#' @param with.names Should rownames and column names be added to the data.table
#' @export
setGeneric("mat.to.data.table", function(M, ...) standardGeneric("mat.to.data.table"))

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "Matrix", Matrix.mat.to.data.table)

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "matrix", matrix.mat.to.data.table)

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "diagonalMatrix", function(M, one.based = TRUE)  mtdt.add.row.col.names(data.table(i = 1:nrow(M) - !one.based, j = 1:nrow(M) - !one.based, x = diag(M)), M, one.based))

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "symmetricMatrix", sym.mat.to.data.table)

array.to.data.table <- function(A,
                                one.based = TRUE)
{
    LI <- list()
    nd <- length(dim(A))
    N <- length(A)
    for(i in 1:nd)
    {
        di <- dim(A)[i]
        do <- dim(A)[-(i:nd)]
        ndo <- prod(do)        
        LI <- c(LI,
                list((((1:N) - 1) %/% ndo) %% di + one.based))
    }
    names(LI) <- paste0("i", 1:nd)
    LI <- as.data.table(LI)
    LI[, x := as.vector(A)]
    LI
}

#' Array to data.table
#'
#' Transforms an array into a set of index / value tuples
setGeneric("array.to.data.table", array.to.data.table)

## give the indexes in the order first
#' Array building
#'
#' Builds an array from a list of indices / value tuples
#' @export
make.array <- function(...,
                       x,
                       dim,
                       one.based = TRUE)
{
    A <- array(0, dim = dim)
    nd <- length(dim)
    LI <- list(...)
    inds <- rep(0L, length(x))
    for(i in 1:nd)
    {
        di <- dim(A)[i]
        do <- dim(A)[-(i:nd)]
        ndo <- prod(do)
        cind <- LI[[i]]
        inds <- inds + (cind - one.based) * ndo 
    }
    inds <- inds + 1L
    A[inds] <- x
    A
}

#' Forcing symmetry
#'
#' Builds a symmetric table from all the values of a regular matrix.
#' @export
make.symmetric <- function(M) M + t(M) - Diagonal(x = diag(M))
## Partial Kronecker

## Performs Conj(M1) %x% M2
partial.kronecker <- function(M1,
                              M2,
                              DI,
                              DJ = DI[, list(k = i, l = j, J = I)],
                              dims = c(DI[, max(I)], DJ[, max(J)]),
                              half.vec = TRUE,
                              real = TRUE,
                              logger = JLoggerFactory("Jalgos Algebra"))
{
    DI <- DI[, list(i, j, I)]
    if(nrow(DI[i > j]) == 0)
    {
        DI <- rbind(DI, DI[i < j, list(i = j, j = i, I)])
    }
    DM1 <- mat.to.data.table(M1)
    DM2 <- mat.to.data.table(M2)
    setnames(DM1, c("i", "k", "x1"))
    setnames(DM2, c("j", "l", "x2"))
    if(DM1[, is.complex(x1)] || DM2[, is.complex(x2)])
    {
        DM1[, c("ix1", "rx1") := list(Im(x1), Re(x1))]
        DM2[, c("ix2", "rx2") := list(Im(x2), Re(x2))]
        DM1[, x1 := NULL]
        DM2[, x2 := NULL]
    }
    KR <- merge(DM1, DI, by = "i", allow.cartesian = TRUE)
    if(half.vec) KR <- KR[i <= j]
    KR <-  merge(KR, DJ, by = "k", all.x = TRUE, allow.cartesian = TRUE)
    KR <- merge(KR, DM2, by = c("j", "l"), allow.cartesian = TRUE)
    if("x1" %in% names(KR)) dsparseMatrix(KR[, list(x = sum(x1 * x2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
    else if(real) dsparseMatrix(KR[, list(x = sum(rx1 * rx2 + ix1 * ix2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
    else
    {
        RM <- dsparseMatrix(KR[, list(x = sum(rx1 * rx2 - ix1 * ix2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
        IM <- dsparseMatrix(KR[, list(x = sum(rx1 * ix2 + ix1 * rx2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
        matrix(complex(real = as.matrix(RM), imaginary = as.matrix(IM)), nrow = nrow(IM), ncol = ncol(RM))
    }
}

#is.finite fails in case prod(dims) is passed the integer bound
sum.non.finite <- function(M) nrow(mat.to.data.table(M)[!is.finite(x)])
    
#### Memory safe kronecker product #######

cor.dec <- function(S,
                    tol = 10 * .Machine$double.eps)
{
    sqD <- sqrt(posD(diag(S)))
    D <- Diagonal(x = sqD)
    D1 <- Diagonal(x = ifelse(abs(sqD) > tol, 1 / sqD, 0))
    C <- D1 %*% S %*% D1
    list(D = D,
         C = C)
}

add.new.krchunk <- function(D1,
                            D2,
                            DKR,
                            target.size,
                            n,
                            m,
                            keep.diag,
                            logger = NULL)
{
    jlog.debug(logger, "Computing cartesian product of tables of sizes:", nrow(D1), nrow(D2), "result's size:", nrow(D1) * nrow(D2))
    D <- cartesian.data.table(D1, D2)[, list(i = (i1 - 1) * n + i2, j = (j1 - 1) * m + j2, x = x1 * x2)]
    tot.size <- nrow(D) + nrow(DKR)
    pb <- 1 - target.size / tot.size
    if(pb <= 0) return(rbind(DKR, D))
    jlog.debug(logger, "Keeping:", 100 * (1 - pb), "% of the result")
    pbx <- quantile(c(D[, abs(x)], DKR[, abs(x)]), probs = pb)
    jlog.debug(logger, "Cropping values lesser than:", pbx, "in absolute value")
    rbind(D[abs(x) >= pbx | (keep.diag & i == j)], DKR[abs(x) >= pbx  | (keep.diag & i == j)])
}

## Function to limit memory usage of the kronecker product
## The approximation error should be small if there are a lot fo values<0.01 in the correlation matrix
## The memory taken by a Matrix (in the triplet form) with N non zero elements is roughly 16 * N / 2 ^ 20 MBs (two integer vectors and a double vector: 4 + 4 + 8)
## If you want to limit the memory to 1Gb you need to set the size.limit to 2 ^ 26
## Keep in mind however that the chunking process will use twice the size provided. If you want to limit the mmeory usage to 2Gb give 1Gb as parameter
mem.safe.kronecker <- function(S1,
                               S2,
                               krsize.limit = krmem.limit * 2 ^ 26,
                               krmem.limit = Inf, ## In gbs
                               keep.diag = FALSE,
                               ...,
                               logger = NULL)
{
    jlog.debug(logger, "Computing a memory safe kronecker product by cropping the least significant values")
    nS1 <- as.numeric(nnzero(S1))
    nS2 <- as.numeric(nnzero(S2))
    expm <- nS1 * nS2
    jlog.debug(logger, "Number of non zero elements in S1:", nS1, "in S2:", nS2, "in result:", expm, "limit:", krsize.limit, "ratio:", krsize.limit / expm)
    if(expm < krsize.limit) return(S1 %x% S2)
    CS1 <- cor.dec(S1)
    CS2 <- cor.dec(S2)
    DS1 <- mat.to.data.table(CS1$C)
    DS2 <- mat.to.data.table(CS2$C)
    DD <- CS1$D %x% CS2$D    
    chunksize <- krsize.limit %/% (2 * nrow(DS2)) ## Will  fail if it's zero. Hopefully an unrealistic case
    if(chunksize == 0)
    {
        jlog.error(logger, "Cannot divide S1, S2 too big")
        stop("No viable chunksize")
    }
    jlog.debug(logger, "Dividing DS1 into chunks of size:", chunksize, "number of chunks:", nrow(DS1) %/% chunksize)
    n <- 1
    N <- chunksize
    DKR <- data.table(i = integer(0), j = integer(0), x = numeric(0))
    while(n <= nrow(DS1))
    {
        N <- min(nrow(DS1), N)
        jlog.debug(logger, "Dealing with chunk from:", n, "to", N)
        DKR <- add.new.krchunk(DS1[n:N], DS2, DKR, krsize.limit, nrow(S1), ncol(S2), keep.diag = keep.diag, logger = logger)
        jlog.debug(logger, N / nrow(DS1) * 100, "% done")
        n <- n + chunksize
        N <- N + chunksize
    }
    jlog.debug(logger, "Result has:", nrow(DKR), "nonzero elements over an expected total of:", expm) 
    CC <- dsparseMatrix(DKR[, list(i = i, j = j, x = x)], dims = dim(S1) * dim(S2))
    gc()
    DD %*% CC %*% DD
}

#' Non finite elements
#' @export
setGeneric("non.finite.elements", function(x) sum(!is.finite(x)))

#' @rdname non.finite.elements
#' @export
setMethod("non.finite.elements", c(x = "sparseMatrix"), function(x) non.finite.elements(x@x))

bdiag.with.names <- function(L)
{
    M <- bdiag(L)
    dimnames(M) <- list(unlist(lapply(L, rownames)),
                        unlist(lapply(L, colnames)))
    M
}


### Adjugate ####
## From stackoverflow Vincent Zoonekynd: http://stackoverflow.com/questions/16757100/get-adjoint-matrix-in-r
## Minor and cofactor
minor <- function(A, i, j) det( A[-i,-j] )
cofactor <- function(A, i, j) (-1)^(i+j) * minor(A,i,j)

# With a loop
adjoint1 <- function(A) {
  n <- nrow(A)
  B <- matrix(NA, n, n)
  for( i in 1:n )
    for( j in 1:n )
      B[j,i] <- cofactor(A, i, j)
  B
}

# With `outer`
adjoint2 <- function(A) {
  n <- nrow(A)
  t(outer(1:n, 1:n, Vectorize(
    function(i,j) cofactor(A,i,j)
  )))
}

##### Jacobi inverse

jacobi.inv <- function(a,
                       b,
                       p1 = Diagonal(x = 1 / diag(M)), ## Preconditioner the closer from M the better,
                       p = Diagonal(x = diag(M)),
                       x = p1 %*% b, ## First guess
                       tol = sqrt(.Machine$double.eps),
                       iter.max = 1000,
                       logger = JLoggerFactory("Jalgos Algebra"))
{
    if(nrow(b) != nrow(a))
    {
        jlog.error(logger, "Dimension of b and a don't match:", dim(a), dim(b))
        stop("Dimension mismatch")
    }
    r <- a - p
    inm <- matrix.norm(Diagonal(nrow(M)) -  p1 %*% a)
    if(inm > 1) jlog.warn(logger, "The jacobi method may not convergence as the preconditioner is too far from the real inverse. Norm(I - PA):", inm)
    err <- matrix.norm(a %*% x - b)
    iter <- 1
    while(err > tol)
    {
        if(!is.finite(err))
        {
            jlog.error(logger, "Error is not finite:", err)
            stop("non finite error")
        }
        if(iter == iter.max)
        {
            jlog.error(logger, "Jacobi method did not converge after", iter.max,"iterations, current error:", err, "tolerance:", tol)
            stop("maxiter")
        }
        iter <- iter + 1
        x <- p1 %*% (b - r %*% x)
        err <- matrix.norm(a %*% x - b)
        jlog.debug(logger, "After", iter, "iterations, error is at:", err, "tol:", tol)
    }
    jlog.debug(logger, "Method converged in:", iter, "iterations", err)
    x
}
                       
######## Function used by both ALS and Jalgos filter moved here

#Function to compute the matrix that will transform M(i, j, k, l) into M(i, l, k, j)
#This is commonly known as the commutation matrix
JF.PE4 <- function(n,
                   p = n,
                   half.vec = FALSE)
{
    if(n == 1) return(Diagonal(p))
    if(p == 1) return(Diagonal(n))
    findex <- mat.index
    if(half.vec) findex <- index.sym
    J <- rep(1:p, n)
    L <- rep(1:n, each = p)
    if(half.vec)
    {
        F <- J <= L
        J <- J[F]
        L <- L[F]
    }
    I1 <- findex(J, L, p)
    I2 <- findex(L, J, n)
    #Need to add a switch here in case I1 or I2 exceeds 2^32-1
    sparseMatrix(I1, I2, x = 1)
}

JF.S1 <- function(S,
                  half.vec = TRUE,
                  ...,
                  logger = JLoggerFactory("jalgos filter"))
{
    jlog.debug(logger, "Computing S1 for a matrix of dimension:", nrow(S))
    n <- nrow(S)
    I <- Diagonal(n ^ 2)
    P <- JF.PE4(n)
    S1 <- (I + P) %*% (S %x% S)
    if(half.vec)
    {
        D <- vec.to.vech(n)
        D %*% S1 %*% t(D)
    }
    else S1
}

## Efficiently computes t(D1) %*% (M %x% M) %*% t(D) for a symmetric matrix
## Does not work!!
half.kronecker <- function(M)
{
    nm <- nrow(M)
    DM <- mat.to.data.table(M)
    DM <- DM[i <= j, list(I = index.sym(i, j, nm), x = x)]
    DR <- data.table(i = 1:nm)
    DR <- DR[, list(k = (i:nm)), by = i]
    DR[, I := index.sym(i, k, nm)]
    DC <- DR[, list(j = i, l = k, J = index.sym(i, k, nm))]
    f <- function(I){
        if(I < 2) integer(0)
        else 1:(I-1)
    }
    DR <- DR[, list(J = f(I)), by = list(i, k, I)]
    setkey(DC, J)
    setkey(DR, J)
    DR <- DC[DR]
    DR <- DR[, list(i, j, k, l, I, J, IM = index.sym(i, j, nm), JM = index.sym(k, l, nm))]
    setkey(DM, I)
    setkey(DR, IM)
    DR <- DR[DM]
    setnames(DR, "x", "xr")
    setkey(DR, JM)
    DR <- DR[J(DM[, I])]
    DR[DM, xr := xr * x]
    MR <- dsparseMatrix(DR[, list(i = I, j = J, x = xr)], symmetric = TRUE, dims = c(nn12(nm), nn12(nm)))
    
}

#Total volatility contained in a covariance matrix
total.vol <- function(S)
{
    sum(diag(S))
}

########

non.zero.indices <- function(M)
{
    DT <- as.data.table(which(M != 0, arr.ind = TRUE))
    setnames(DT, c("i", "j"))
    DT
}

is.identity <- function(M)
{
    nrow(M) == ncol(M) && nnzero(M) == nrow(M) && all(diag(M) == 1)
}

########

gen.inverse <- function(a,
                        b,
                        fsolve = solve)
{
    fsolve(crossprod(a), crossprod(a, b))
}

########

mat.exp <- function(M,
                    exp)
{
    if(as.integer(exp) != exp) stop("Exponant must be an integer")
    if(exp == 0L) return(Diagonal(n = nrow(M)))
    else if(exp == 1L) return(M)
    n1 <- exp %/% 2
    n2 <- exp - n1
    (M %^% n1) %*% (M %^% n2)
}

#' Matrix exponent
#' @export
setGeneric("%^%", mat.exp)

## Very sparse big matrix exhibit a weird behaviour when multiplied. STMatrix (for "Stay triplet") class is here to fix that
## Only way to handle huge matrix with dimensions exceeding integer limit


STMatrix <- setClass("STMatrix",
                     slots = c(data = "data.table",
                               dims = "index",
                               dmnames = "list"))

STM.mult <- function(x, y)
{
    Dx <- mat.to.data.table(x)
    Dy <- mat.to.data.table(y)
    setnames(Dx, "j", "k")
    setnames(Dy, "i", "k")
    D <- merge(Dx, Dy, by = "k")
    ## Tries to return a triplet if D is small STMatrix otherwise
    dms <- c(nrow(x), ncol(y))
    dmns <- list(rownames(x), rownames(y))
    if(all(dms <= .Machine$integer.max))
        dsparseMatrix(D[, list(i, j, x = x.x * x.y)],
                      giveCsparse = FALSE,
                      dims = dms,
                      dimnames = dmns) ## Too restricting to return a STMatrix
    else
        STMatrix(DM = D[, list(x = x.x * x.y), by = list(i, j)],
                 dims = dms,
                 dmnames = dmns)
}


    
STMatrix <- function(M,
                     DM = mat.to.data.table(M),
                     dims,
                     dmnames)
{
    if(!missing(M) && is(M, "STMatrix")) return(M)
    
    if(missing(dims) && missing(M)) dims <- DM[, c(max(i), max(j))]
    else if(missing(dims)) dims <- dim(M)
    
    if(missing(dmnames) && missing(M)) dmnames <- list(NULL, NULL)
    else if(missing(dmnames)) dmnames <- dimnames(M)
    
    if(is.null(dmnames)) dmnames <- list(NULL, NULL)
    new("STMatrix",
        data = DM,
        dims = dims,
        dmnames = dmnames)
}

STM.as.Matrix <- function(M)
{
    dsparseMatrix(M@data[, list(i, j, x)],
                  giveCsparse = FALSE,
                  dims = M@dims,
                  dimnames = M@dmnames)
}

STM.show <- function(object)
{
    if(all(object@dims <= .Machine$integer.max))        
        show(as.Matrix(object))
    else
        callNextMethod(object)
}

STM.t <- function(x)
{
    STMatrix(DM = x@data[i = j, j = i, x = x],
             dims = rev(x@dims),
             dmnames = rev(x@dmnames))
}

#' @export
setMethod("%*%", c("STMatrix", "genMatrix"), STM.mult)

#' @export
setMethod("%*%", c("genMatrix", "STMatrix"), STM.mult)

#' @export
setMethod("show", "STMatrix", STM.show)

#' @export
setMethod("t", "STMatrix", STM.t)

#' Converting to Matrix
#' @export
setGeneric("as.Matrix", function(M) standardGeneric("as.Matrix"))

#' @rdname as.Matrix
#' @export
setMethod("as.Matrix", "STMatrix", STM.as.Matrix)

#' @export
setMethod("show", "STMatrix", STM.show)

#' @export
setMethod("dim", "STMatrix", function(x) x@dims)

#' @export
setMethod("[", c("STMatrix", "missing", "missing", "missing"), function(x) as.Matrix(x))

#' @export
setMethod("[", c("STMatrix", "index", "missing", "ANY"), function(x, i, j, drop = FALSE) x[][i, , drop = drop])

#' @export
setMethod("[", c("STMatrix", "missing", "index", "ANY"), function(x, i, j, drop = FALSE) x[][, j, drop = drop])

#' @export
setMethod("[", c("STMatrix", "index", "index", "ANY"), function(x, i, j, drop = FALSE) x[][i, j, drop = drop])

#' @export
setMethod("*", c("STMatrix", "ANY"), function(e1, e2) e1[] * e2)

#' @export
setMethod("*", c("ANY", "STMatrix"), function(e1, e2) e1 * e2[])

#' @export
setMethod("/", c("STMatrix", "ANY"), function(e1, e2) e1[] / e2)

#' @export
setMethod("+", c("STMatrix", "ANY"), function(e1, e2) e1[] + e2)

#' @export
setMethod("+", c("ANY", "STMatrix"), function(e1, e2) e1 + e2[])

#' @export
setMethod("-", c("STMatrix", "missing"), function(e1, e2) - e1[])

#' @export
setMethod("-", c("STMatrix", "ANY"), function(e1, e2) e1[] - e2)

#' @export
setMethod("-", c("ANY", "STMatrix"), function(e1, e2) e1 - e2[])

#' @export
setMethod("%*%", c("STMatrix", "ANY"), function(x, y) x[] %*% y)

#' @export
setMethod("%*%", c("ANY", "STMatrix"), function(x, y) x %*% y[])

#' @export
setMethod("kronecker", c(X = "STMatrix", Y = "ANY"), function(X, Y) X[] %x% Y)

#' @export
setMethod("kronecker", c(X = "ANY", Y = "STMatrix"), function(X, Y) X %x% Y[])

#' @export
setMethod("vech", c(M = "STMatrix"), function(M) vech(M[]))

#' @export
setMethod("vec", c(M = "STMatrix"), function(M, ...) vec(M[], ...))

#' @export
setMethod("col.bind", c(M1 = "STMatrix", M2 = "ANY"), function(M1, M2) col.bind(M1[], M2))

#' @export
setMethod("col.bind", c(M1 = "ANY", M2 = "STMatrix"), function(M1, M2) col.bind(M1, M2[]))

#' @export
setMethod("row.bind", c(M1 = "STMatrix", M2 = "ANY"), function(M1, M2) row.bind(M1[], M2))

#' @export
setMethod("row.bind", c(M1 = "ANY", M2 = "STMatrix"), function(M1, M2) row.bind(M1, M2[]))

#' @export
setMethod("t", c(x = "STMatrix"), function(x) t(x[]))

#' @export
setMethod("is.na", c(x = "STMatrix"), function(x) is.na(x[]))

#' @export
setMethod("non.finite.elements", c(x = "STMatrix"), function(x) non.finite.elements(x[]))

#' Total volatility of a matrix
#' @export
setGeneric("total.vol", total.vol)

#' @rdname total.vol
#' @export
setMethod("total.vol", c(S = "STMatrix"), function(S) sum(diag(S[])))

#' @export
setMethod("diag", c(x = "STMatrix", nrow = "ANY", ncol = "ANY"), function(x) diag(x[]))

#' @rdname mat.to.data.table
#' @export 
setMethod("mat.to.data.table", "STMatrix", function(M) copy(M@data))

#' @rdname matrix.norm
#' @export
setMethod("norm", c("STMatrix", "ANY"), function(x, type, ...) norm(x[], type, ...))


## New version of data.table messes up with sparseMatrices

dsparseMatrix <- function(D,
                          ...)
{
    sparseMatrix(i = D[, i], j = D[, j], x = D[, x], ...)
}

## Rotation matrix 2d

rotation.matrix <- function(theta)
{
    M <- matrix(0, 2, 2)
    M[1, 1] <- cos(theta)
    M[1, 2] <- -sin(theta)
    M[2, 1] <- sin(theta)
    M[2, 2] <- cos(theta)
    M
}

rotate.vector <- function(X,
                          theta)
{
    RM <- rotation.matrix(theta)
    RM %*% X
}

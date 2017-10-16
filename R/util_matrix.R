## If t is not a generic a lot of functions fail
#' @export
setGeneric("t", t)

## If tcrossprod is not a generic a lot of functions fail
#' @export
setGeneric("tcrossprod", tcrossprod)

## DITTO
#' @export
setGeneric("crossprod", crossprod)

## DITTO
#' @export
setGeneric("diag", diag)

## DITTO
#' @export
setGeneric("solve", solve)

## DITTO
#' @export
setGeneric("isSymmetric", isSymmetric)

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

#' Scalar Product Matrix
#'
#' Computes the scalar product between two matrices. Simply the sum of the product Hadamard product
#' @param M1 Matrix
#' @param M2 Matrix
#' @export
setGeneric("matrix.scal.raw", function(M1, M2) sum(M1 * M2))

#' @describeIn matrix.norm scalar product of matrices. Identitical to the scalar product of the vectorized form of the matrices
#' @export
matrix.scal <- function(M1, M2, normalized = TRUE)
{
    if(normalized) N <- matrix.scal.raw(M1, M1)
    else N <- 1
    return(matrix.scal.raw(M1, M2) / N)
}

##Equivalent to Matrix's norm(M, "f")
#' Norm
#' 
#' Generic for vectors
#' @export
setMethod("norm", c("vector", "ANY"), function(x, type, ...) Matrix::norm(Matrix(x), type))


#' Trace of a matrix
#' @export
matrix.trace <- function(M) sum(diag(M))

## More utility functions
#' @name eigen.func
#' @title Applying functions to the eigen value
#' @param M Matrix on which we want to apply func
#' @param func Function to apply to the eigen values of M
#' @param D eigen values
#' @param ... Parameters to pass on to func
NULL

do.funcM <- function(M,
                     func,
                     ...)
{
    n <- nrow(M)
    E <- eigen(M, symmetric = TRUE)
    V <- Matrix(E$vectors)
    D <- E$values
    tcrossprod(V %*% Diagonal(n, x = func(D, ...)), V)
}

#' Factors Handling
#'
#' Applies a function to the eigen decomposition of a matrix in this fashion: let M = Pdiag(V)P^-1 then the result is Q = Pdiag(f(V))P^-1 where f is a function that is applied to the vector of eigen values.
#' @param M Matrix to be decomposed
#' @param func Function to be applied to the eigen values
#' @export
setGeneric("funcM", function(M, ...) standardGeneric("funcM"))

#' @export
setMethod("funcM", "Matrix", do.funcM)

#' @export
setMethod("funcM", "matrix", do.funcM)

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
#' @include util.R
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

#' Inserting Data Into a Buffer
#'
#' @param EG The buffer
#' @param new.data data to be pushed to the buffer. When some columns in new.data are missing in the buffer, they will be added to the new buffer producing warnings.
#' @param i Row indices into which making the insertion
#' @param j Column indices into which making the insertion
#' @export
setGeneric("insert", function(EG, new.data, i, j, ...) standardGeneric("insert"))

#' Replacing data in the buffer
#'
#' Replaces parts or totality of the buffer.
#' @param EG The buffer
#' @param new.data Data to use as replacement
#' @export
setGeneric("update.buffer", function(EG, new.data, ...) standardGeneric("update.buffer"))

#' Resize, reserve, binds
#' 
#' Binds two data structures so that the number of rows of the result is the sum of the number of rows of the two arguments. Columns have to match.
#' @template reserve
#' @export
setGeneric("row.bind", function(M1, M2) standardGeneric("row.bind"))

#' @export
setMethod("row.bind", c("ANY", "ANY"), function(M1, M2) rBind(M1, M2))

#' Resize, reserve, binds
#' 
#' Binds two data structures so that the number of columns of the result is the sum of the number of columns of the two arguments. Number of rows has to be te same
#' @template reserve
#' @export
setGeneric("col.bind", function(M1, M2) standardGeneric("col.bind"))

#' @export
setMethod("col.bind", c("ANY", "ANY"), function(M1, M2) cBind(M1, M2))

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

#' Partial Inversion Using Singular Values
#'
#' Inverts a matrix by truncating the eigen values that contribute to 95% of the whole volatility.
#' @param a Matrix to invert
#' @param b RHS of the equation a.v = b where v is the unknown
#' @param var.thresh quantity of volatility to be taken into account for inverstion
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

#' Matrix to data.table
#'
#' Adds two columns i.names and j.names to the triplet represantation of a Matrix
#' @param D triplet representation
#' @param M Matrix
#' @param one.based indices strart at 1 or 0?
#' @export
mtdt.add.row.col.names <- function(D,
                                   M,
                                   one.based = TRUE)
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
                                     with.names = FALSE,
                                     ...)
{
    TM <- as(M, "TsparseMatrix")
    D <- data.table::data.table(i = TM@i + one.based, j = TM@j + one.based, x = TM@x)
    if(with.names)
        mtdt.add.row.col.names(D,
                               M,
                               one.based)
    else
        D
}

matrix.mat.to.data.table <- function(M,
                                     one.based = TRUE,
                                     with.names = FALSE,
                                     ...)
{
    D <- data.table::data.table(i = as.vector(row(M)) - !one.based, j = as.vector(col(M)) - !one.based, x = as.vector(M))
    D <- D[x != 0] ## It should make later computations faster    
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


vec.mat.to.data.table <- function(M,
                                  one.based = TRUE,
                                  with.names = FALSE,
                                  ...)
{
    D <- data.table(i = 1:length(M), x = M, j = 1)
    if(with.names)
        D[, i.names := names(M)]
    D
}

#' Matrix to data table
#'
#' Transforms a matrix into its triplet version in a data.table
#' @param M Matrix to transform to its triplet form
#' @param one.based Indices start at 1 or 0
#' @param with.names Should rownames and column names be added to the data.table
#' @aliases mat.to.triplet
#' @export
setGeneric("mat.to.data.table", function(M, ...) standardGeneric("mat.to.data.table"))

#' @export mat.to.triplet
mat.to.triplet <- mat.to.data.table

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "Matrix", Matrix.mat.to.data.table)

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "matrix", matrix.mat.to.data.table)

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "diagonalMatrix", function(M, one.based = TRUE, ...)  mtdt.add.row.col.names(data.table::data.table(i = 1:nrow(M) - !one.based, j = 1:nrow(M) - !one.based, x = diag(M)), M, one.based))

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "symmetricMatrix", sym.mat.to.data.table)

#' @rdname mat.to.data.table
#' @export
setMethod("mat.to.data.table", "vector", vec.mat.to.data.table)

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
    LI <- data.table::as.data.table(LI)
    LI[, x := as.vector(A)]
    LI
}

#' Array to data.table
#'
#' Transforms an array into a set of index / value tuples
#' @param A Array to transform to a data.table
#' @param one.based Should the indices start at one
#' @export
setGeneric("array.to.data.table", array.to.data.table)

#' Array Index
#'
#' Computes the index in the vectorization of a multidimensional array corresponding to the individual coordinates
#' @param ... integer vectors describing the coordinates
#' @param LI a list of the individual coordinates
#' @param dim Dimension of the array
#' @param one.based should the indices start at one
#' @export
array.index <- function(...,
                        LI = list(...),
                        dim,
                        one.based = TRUE)
{
    nd <- length(dim)
    if(nd == 0) return(integer(0L))
    inds <- rep(0L, length(LI[[1]]))
    for(i in 1:nd)
    {
        di <- dim[i]
        do <- dim[-(i:nd)]
        ndo <- prod(do)
        cind <- LI[[i]]
        inds <- inds + (cind - one.based) * ndo 
    }
    inds + one.based
}

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
    inds <- array.index(..., dim = dim)
    A[inds] <- xs
    A
}

#' Forcing symmetry
#'
#' Builds a symmetric table from all the values of a regular matrix.
#' @export
make.symmetric <- function(M) M + t(M) - Diagonal(x = diag(M))
## Partial Kronecker

## Performs Conj(M1) %x% M2

#' @export
dt.partial.kronecker <- function(M1,
                                 M2,
                                 DI,
                                 DJ = DI[, list(k = i, l = j, J = I)],
                                 dims = c(DI[, max(I)], DJ[, max(J)]),
                                 half.vec = TRUE,
                                 real = TRUE,
                                 logger = jlogger::JLoggerFactory("Jalgos Algebra"))
{
    DI <- DI[, list(i, j, I)]
    if(nrow(DI[i > j]) == 0)
    {
        DI <- rbind(DI, DI[i < j, list(i = j, j = i, I)])
    }
    DM1 <- mat.to.triplet(M1)
    DM2 <- mat.to.triplet(M2)
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
    if(half.vec) KR <- KR[i <= j] ## Don't think that is right
    KR <- merge(KR, DJ, by = "k", all.x = TRUE, allow.cartesian = TRUE)
    KR <- merge(KR, DM2, by = c("j", "l"), allow.cartesian = TRUE)
    if("x1" %in% names(KR))
        dsparseMatrix(KR[, list(x = sum(x1 * x2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
    else if(real)
        dsparseMatrix(KR[, list(x = sum(rx1 * rx2 + ix1 * ix2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
    else
    {
        RM <- dsparseMatrix(KR[, list(x = sum(rx1 * rx2 - ix1 * ix2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
        IM <- dsparseMatrix(KR[, list(x = sum(rx1 * ix2 + ix1 * rx2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
        matrix(complex(real = as.matrix(RM), imaginary = as.matrix(IM)), nrow = nrow(IM), ncol = ncol(RM))
    }
}

#### Memory safe kronecker product #######

#' Covariance matrix to correlation matrix
#'
#' Computes the correlation matrix associated with a given covariance matrix
#' @seealso cov2cor
#' @export
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
    jlogger::jlog.debug(logger, "Computing cartesian product of tables of sizes:", nrow(D1), nrow(D2), "result's size:", nrow(D1) * nrow(D2))
    D <- cartesian.data.table(D1, D2)[, list(i = (i1 - 1) * n + i2, j = (j1 - 1) * m + j2, x = x1 * x2)]
    tot.size <- nrow(D) + nrow(DKR)
    pb <- 1 - target.size / tot.size
    if(pb <= 0) return(rbind(DKR, D))
    jlogger::jlog.debug(logger, "Keeping:", 100 * (1 - pb), "% of the result")
    pbx <- quantile(c(D[, abs(x)], DKR[, abs(x)]), probs = pb)
    jlogger::jlog.debug(logger, "Cropping values lesser than:", pbx, "in absolute value")
    rbind(D[abs(x) >= pbx | (keep.diag & i == j)], DKR[abs(x) >= pbx  | (keep.diag & i == j)])
}

## Function to limit memory usage of the kronecker product
## The approximation error should be small if there are a lot fo values < 0.01 in the correlation matrix
## The memory taken by a Matrix (in the triplet form) with N non zero elements is roughly 16 * N / 2 ^ 20 MBs (two integer vectors and a double vector: 4 + 4 + 8)
## If you want to limit the memory to 1Gb you need to set the size.limit to 2 ^ 26
## Keep in mind however that the chunking process will use twice the size provided. If you want to limit the mmeory usage to 2Gb give 1Gb as parameter

#' Memory Safe Kronecker Product
#'
#' Performs a kronecker product of two so the memory used stays under a scpecified bound.
#' @details The matrices are divided into smaller chunks and the kronecker products of these chunks are computed. Only the most significant correlation are kept to meet the target size.
#' The approximation error should be small if there are a lot fo values < 0.01 in the correlation matrix. \cr
#' The memory taken by a Matrix (in the triplet form) with N non zero elements is roughly 16 * N / 2 ^ 20 MBs (two integer vectors and a double vector: 4 + 4 + 8) \cr
#' If you want to limit the memory to 1Gb you need to set the size.limit to 2 ^ 26. \cr
#' Keep in mind however that the chunking process will use twice the size provided. If you want to limit the mmeory usage to 2Gb give 1Gb as parameter.
#' @param S1 First matrix
#' @param S2 Second matrix
#' @param krsize.limit The limit in number of non zero value of the result
#' @param krmem.limit Used to express the limit in GBs
#' @param keep.diag Should the diagonal terms be kept
#' @param logger JLogger to log additional messages
#' @param ... No use now
#' @export
mem.safe.kronecker <- function(S1,
                               S2,
                               krsize.limit = krmem.limit * 2 ^ 26,
                               krmem.limit = Inf, ## In gbs
                               keep.diag = FALSE,
                               ...,
                               logger = NULL)
{
    jlogger::jlog.debug(logger, "Computing a memory safe kronecker product by cropping the least significant values")
    nS1 <- as.numeric(nnzero(S1))
    nS2 <- as.numeric(nnzero(S2))
    expm <- nS1 * nS2
    jlogger::jlog.debug(logger, "Number of non zero elements in S1:", nS1, "in S2:", nS2, "in result:", expm, "limit:", krsize.limit, "ratio:", krsize.limit / expm)
    if(expm < krsize.limit) return(S1 %x% S2)
    CS1 <- cor.dec(S1)
    CS2 <- cor.dec(S2)
    DS1 <- mat.to.triplet(CS1$C)
    DS2 <- mat.to.triplet(CS2$C)
    DD <- CS1$D %x% CS2$D    
    chunksize <- krsize.limit %/% (2 * nrow(DS2)) ## Will  fail if it's zero. Hopefully an unrealistic case
    if(chunksize == 0)
    {
        jlogger::jlog.error(logger, "Cannot divide S1, S2 too big")
        stop("No viable chunksize")
    }
    jlogger::jlog.debug(logger, "Dividing DS1 into chunks of size:", chunksize, "number of chunks:", nrow(DS1) %/% chunksize)
    n <- 1
    N <- chunksize
    DKR <- data.table::data.table(i = integer(0), j = integer(0), x = numeric(0))
    while(n <= nrow(DS1))
    {
        N <- min(nrow(DS1), N)
        jlogger::jlog.debug(logger, "Dealing with chunk from:", n, "to", N)
        DKR <- add.new.krchunk(DS1[n:N], DS2, DKR, krsize.limit, nrow(S1), ncol(S2), keep.diag = keep.diag, logger = logger)
        jlogger::jlog.debug(logger, N / nrow(DS1) * 100, "% done")
        n <- n + chunksize
        N <- N + chunksize
    }
    jlogger::jlog.debug(logger, "Result has:", nrow(DKR), "nonzero elements over an expected total of:", expm) 
    CC <- dsparseMatrix(DKR[, list(i = i, j = j, x = x)], dims = dim(S1) * dim(S2))
    gc()
    DD %*% CC %*% DD
}

#' Alternative To NULL Dimensions
#'
#' Returns a vector of empty strings instead of NULL
#' @param x The object on which name.fun and length.fun are to be applied
#' @param name.fun function returning the desired name of x
#' @param length.fun function returning the length of the desired dimension
#' @export
not.null.names <- function(x,
                           name.fun = names,
                           length.fun = length,
                           default = "")
{
    nm <- name.fun(x)
    if(is.null(nm)) rep(default, length.fun(x))
    else nm
}

#' Matrix rows and column names
#'
#' Build a block diagonal matrix and computes the row names and column names from the block names
#' @export
bdiag.with.names <- function(L)
{
    M <- bdiag(L)
    dimnames(M) <- list(unlist(lapply(L, not.null.names, name.fun = rownames, length.fun = nrow)),
                        unlist(lapply(L, not.null.names, name.fun = colnames, length.fun = ncol)))
    M
}


### Adjugate ####
#' @name adjugate
#' @title Minor, Cofactor, Adjugates
#' @description Tools to compute the minor, cofactor or adjugate matrix of a given matrix
#' @param A Matrix
#' @param i reference line
#' @param j reference column

#' @details From stackoverflow Vincent Zoonekynd: http://stackoverflow.com/questions/16757100/get-adjoint-matrix-in-r
#' @references https://en.wikipedia.org/wiki/Adjugate_matrix
NULL
#' @describeIn adjugate Minor, determinant where row i and j are taken out
#' @export
minor <- function(A, i, j) det(A[-i, -j])

#' @describeIn adjugate Cofactor, minor multiplied by (-1) ^ (i + j)
#' @export
cofactor <- function(A, i, j) (-1) ^ (i + j) * minor(A, i, j)

#' @describeIn adjugate Adjugate Matrix. Adjugate(M)[i, j] = Cofactor(M, j, i)
#'@export
adjugate1 <- function(A)
{
    n <- nrow(A)
    B <- matrix(NA, n, n)
    for(i in 1:n)
        for(j in 1:n)
            B[j, i] <- cofactor(A, i, j)
    B
}

#' @describeIn adjugate Adjugate Matrix computed without the use of for loops but with outer and Vectorize
#' @export
adjugate2 <- function(A)
{
    n <- nrow(A)
    t(outer(1:n, 1:n, Vectorize(
                          function(i, j) cofactor(A,i,j)
                      )))
}


## Efficiently computes t(D1) %*% (M %x% M) %*% t(D) for a symmetric matrix
## Does not work!!
half.kronecker <- function(M)
{
    nm <- nrow(M)
    DM <- mat.to.triplet(M)
    DM <- DM[i <= j, list(I = index.sym(i, j, nm), x = x)]
    DR <- data.table::data.table(i = 1:nm)
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

#' Total volatility
#'
#' Total volatility contained in a covariance matrix. It's simply its trace
#' @export
setGeneric("total.vol", function(S) sum(diag(S)))

########
#' Non zero indices
#' Gets non zero indices
#' @export
non.zero.indices <- function(M)
{
    DT <- data.table::as.data.table(which(M != 0, arr.ind = TRUE))
    setnames(DT, c("i", "j"))
    DT
}

#' Identity
#'
#' Checks whether a matrix is identical to the identity matrix
#' @export
is.identity <- function(M)
{
    nrow(M) == ncol(M) && nnzero(M) == nrow(M) && all(diag(M) == 1)
}

########

#' Generalized Inverse
#'
#' Computes the generalized solution to a linear system for rectangular matrices.
#' @param a Matrix
#' @param b Right hand side of the equation a.x = b
#' @param fsolve The function to use to solve the generalized linear system
#' @export
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
#'
#' Computes the exponentiation of a matrix: M %^% 4 = M %*% M %*% M%*% M %*%
#' @export
setGeneric("%^%", mat.exp)

## New version of data.table messes up with sparseMatrices

#' Building Matrices
#'
#' Builds a sparseMatrix from a data.table containing a triplet representation of a matrix. The function will only use the i, j, x columns
#' @param data triplet representation, needs to have i, j, x columns
#' @param ... Parameters to be forwarded to sparseMatrix
#' @return a Matrix of class Matrix
#' @export
dsparseMatrix <- function(data,
                          ...)
{
    data <- data.table::as.data.table(data)
    Matrix::sparseMatrix(i = data[, i], j = data[, j], x = data[, x], ...)
}

## Rotation matrix 2d
#' @name rotation
#' @title Rotation
#' @param theta angle of the rotation
NULL

#' @describeIn rotation Computes a two dimensional rotation matrix
#' @return A 2 x 2 matrix that when multiplied to a vector produce the rotated vector
#' @export
rotation.matrix <- function(theta)
{
    M <- matrix(0, 2, 2)
    M[1, 1] <- cos(theta)
    M[1, 2] <- -sin(theta)
    M[2, 1] <- sin(theta)
    M[2, 2] <- cos(theta)
    M
}

#' @describeIn rotation Rotates a two dimensional vector of the specified angle
#' @export
rotate.vector <- function(X,
                          theta)
{
    RM <- rotation.matrix(theta)
    RM %*% X
}


#' Testing Matrix Positivity
#'
#' Tests whether a matrix is positive and optionally definite.
#' @param S Symmetric Matrix
#' @param tol Tolerance to decide whether an eigen value is null. Everything in the range [-tol, tol] is considered to be a null eigen value. Everything in ]-Inf, -tol[ is negative and everything in ]tol, Inf[ is positive.
#' @param definite Should we check the matrix be definite positive
#' @export
is.positive <- function(S,
                        tol = .Machine$double.eps,
                        definite = FALSE)
{
    E <- eigen(S)
    if(!definite) all(E$values >= -tol)
    else all(E@values > tol)
}

#' Comparing Matrices
#'
#' Compares two matrices by computing their norms and computing there scalar products normalized bby M1's norm.
#' @param M1 A matrix
#' @param M2 A matrix
#' @return A list with tree elements:
#' \itemize{
#' \item{n1} L2 Norm of M1
#' \item{n2} L2 Norm of M2
#' \item{n12} Scalar product of M1 and M2 normalized by M1's norm
#' }
#' @usage compare.matrices(M1, M2)
#' @export compare.matrices
compare.matrices <- function(M1, M2)
{
    n1 <- matrix.norm(M1)
    n2 <- matrix.norm(M2)
    n12 <- matrix.scal(M1, M2) / n1 ^ 2
    list(n1 = n1, n2 = n2, n12 = n12)
}

#' Orthonormalizing a Matrix
#'
#' Returns the closest orthonormal matrix using the function \code{qr}
#' @param B base to orthonormalize
#' @seealso \link{qr}, \link{qr.Q}
#' @export
orthonorm <- function(B)
{
    QB <- qr.Q(qr(B))
    D <- Diagonal(x = sign(diag(crossprod(QB, B))))
    QB %*% D
}


#' @template trim.matrix.gen
#' @export
setGeneric("trim.cov.matrix", function(S, ...) standardGeneric("trim.cov.matrix"))

## drop0 Needs to be defined as a generic in here for trim.cov.matrix to work
#' @export
setGeneric("drop0", Matrix::drop0)

#' @export
inv.values <- function(values,
                        tol.inv)
{
    ifelse(abs(values) > tol.inv, 1 / values, 0)
}

gen.trim.cov.matrix <- function(S,
                                sqDl = sqrt(posD(diag(S))),
                                sqDr = sqDl,
                                Dr = Matrix::Diagonal(x = sqDr),
                                D1r = Matrix::Diagonal(x = inv.values(diag(Dr), tol.inv)),
                                Dl = Matrix::Diagonal(x = sqDl),
                                D1l = Matrix::Diagonal(x = inv.values(diag(Dl), tol.inv)),
                                ...,
                                mtype = "",                               
                                tol = 0,
                                tol.inv = 10 * .Machine$double.eps,
                                logger = NULL)
{
    jlogger::jlog.debug(logger, "Dropping near 0 correlations from matrix:", mtype, "tolerance equals:", tol)
    C <- D1l %*% S %*% D1r
    jlogger::jlog.debug(logger, "Current mb size:", format(object.size(S), "Mb"), "for", mtype, "sparsity ratio:", jsutils::sparsity.ratio(S))
    S <- Dl %*% drop0(C, tol) %*% Dr ## Reusing S saves memory for huge matrices
    jlogger::jlog.debug(logger, "After trimming mb size:", format(object.size(S), "Mb"), "for", mtype, "sparsity ratio:", jsutils::sparsity.ratio(S))
    S
}

#' @template trim.matrix.gen
#' @template trim.matrix
#' @export
setMethod("trim.cov.matrix", "ANY", gen.trim.cov.matrix)

dhs.trim.cov.matrix <- function(S,
                                sqDl = mat.to.triplet(S, shallow = TRUE)[i == j, sqrt(posD(x))],
                                sqDr = sqDl,
                                Dr = distributedhugesparse::DHugeMatrix(data = mat.to.triplet(S, shallow = TRUE)[i == j, .(i, j, x = sqDr)]),
                                D1r = distributedhugesparse::DHugeMatrix(data = mat.to.triplet(Dr, shallow = TRUE)[, .(i, j, x = inv.values(x, tol.inv))]),
                                Dl = distributedhugesparse::DHugeMatrix(data = mat.to.triplet(S, shallow = TRUE)[i == j, .(i, j, x = sqDl)]),
                                D1l = distributedhugesparse::DHugeMatrix(data = mat.to.triplet(Dl, shallow = TRUE)[i == j, .(i, j, x = inv.values(x, tol.inv))]),
                                ...,
                                tol.inv = 10 * .Machine$double.eps)
{
    callNextMethod(S,
                   sqDl = sqDl,
                   sqDr = sqDr,
                   Dr = Dr,
                   D1r = D1r,
                   Dl = Dl,
                   D1l = D1l,
                   ...)
}


    #' @export
setMethod("trim.cov.matrix", "DHugeMatrix", dhs.trim.cov.matrix)


## Matrix operations reimplemented in cpp

#' Partial Kronecker Product
#'
#' Computes a kronecker product for a limited subset of the rows and columns. This version returns the triplet representation as a \code{data.frame}
#' @details Kronecker products produce are very costly in terms of memory usage.
#' @param DM1 a triplet representation of a matrix
#' @param DM2 DITTO
#' @param Il The list of row indices in the result that we are interested in keeping. If NULL assumes no projection. Il must be replicated locally to each process.
#' @param Ir The list of column indices in the result that we are interested in keeping. If NULL assumes no projection. Ir must be replicated locally to each process.
#' @param dm2 Dimension of the RHS matrix
#' @include RcppExports.R
#' @export
df.partial.kronecker <- function(DM1,
                                 DM2,
                                 Il = NULL,
                                 Ir = NULL,
                                 dm2)
{
    partial_kronecker(DM1,
                      DM2,
                      Il,
                      Ir,
                      dm2)
}


partial.kronecker <- function(M1,
                              M2,
                              Il = NULL,
                              Ir = NULL)
{
    
    D <- df.partial.kronecker(mat.to.triplet(M1),
                              mat.to.triplet(M2),
                              Il,
                              Ir,
                              dim(M2))
    Matrix::sparseMatrix(i = D$i, j = D$j, x = D$x, dims = dim(M1) * dim(M2))
}

#' Partial Kronecker Product
#'
#' Computes a kronecker product for a limited subset of the rows and columns.
#' @details Kronecker products produce are very costly in terms of memory usage.
#' @param M1 a generic matrix that can be converted to a triplet using \code{mat.to.triplet}
#' @param M2 DITTO
#' @param Il The list of row indices in the result that we are interested in keeping. If NULL assumes no projection.
#' @param Ir The list of column indices in the result that we are interested in keeping. If NULL assumes no projection.
#' @include RcppExports.R
#' @export
setGeneric('partial.kronecker', partial.kronecker)

#' Kronecker projection
#' 
#' Efficiently projects the result of a kronecker into two (or one) smaller spaces
#' @param M1 a generic matrix that can be converted to a triplet using \code{mat.to.triplet}
#' @param M2 DITTO
#' @param Pl A matrix that will project the rows of M1 \%x\% M2 into a space of smaller dimension. If not provided will assume identity
#' @param Pr A matrix that will project the columns of M1 \%x\% M2 into a space of smaller dimension. If not provided will assume identity
#' @export
kronecker.proj <- function(M1,
                           M2,
                           Pl,
                           Pr)
{
    if(missing(Pl) && missing(Pr))
    {
        stop("no projector provided")
    }
    
    if(missing(Pr))
        Pl %*% partial.kronecker(M1,
                                 M2,
                                 mat.to.triplet(Pl)[, j])
    else if(missing(Pl))
        partial.kronecker(M1,
                          M2,
                          Ir = mat.to.triplet(Pr)[, i]) %*% Pr
    
    else
        Pl %*% partial.kronecker(M1,
                                 M2,
                                 mat.to.triplet(Pl)[, j],
                                 mat.to.triplet(Pr)[, i]) %*% Pr
    
}

#' @name triplet.product
#' @title Triplet Product
#' @details Two implementation of matrix products that uses maps (red / black tree and hashmap) to perform a matrix multiplication. This is a more efficient implementation for very sparse huge matrices. \cr
#' The hashmap version is almost always preferrable.
#' @name triplet.product
#' @param i1 row indices in LHS Matrix
#' @param j1 column indices in LHS Matrix
#' @param x1 values in LHS Matrix
#' @param i2 row indices in RHS Matrix
#' @param j2 column indices in RHS Matrix
#' @param x2 values in RHS Matrix
NULL

#' @describeIn triplet.product Uses a read / black tree map to represent matrix data.
#' @export
triplet.prod.rb <- function(i1,
                            j1,
                            x1,
                            i2,
                            j2,
                            x2)
{
    triplet_prod(i1, j1, x1, i2, j2, x2)
}

#' @describeIn triplet.product Uses a hashmap to represent the data
#' @export
triplet.prod.hash <- function(i1,
                              j1,
                              x1,
                              i2,
                              j2,
                              x2)
{
    triplet_prod_un(i1, j1, x1, i2, j2, x2)
}


###

#' Get The Dimension Of an Algebraic Vector
#'
#' Returns the dimension of an algebraic vector. Will work with objects of type 'vector' and one row matrices
#' @param x vector
#' @export
vecdim <- function(x)
{
    if(is.vector(x)) length(x)
    else if(ncol(x) == 1) nrow(x)
    else stop('matrix is not a vector')
}


do.safe.cov2cor <- function(M,
                            sqDl = sqrt(posD(diag(M))),
                            sqDr = sqDl,
                            tol.inv = 10 * .Machine$double.eps,
                            diag.fun = Matrix::Diagonal)
{
    Dr <- diag.fun(x = sqDr)
    D1r <- diag.fun(x = inv.values(sqDr, tol.inv))
    Dl <- diag.fun(x = sqDl)
    D1l <- diag.fun(x = inv.values(sqDl, tol.inv))
    D1l %*% M %*% D1r
}

#' Covariance Matrix To Correlation Matrix
#'
#' Transforms a covariance matrix into a correlation matrix with parameters to handle 
#' @param M Matrix to transform
#' @param sqDl Diagonal to multiply on the left side
#' @param sqDr Diagonal to multiply on the right side
#' @param diag.fun Function to generate the diagonal matrix
#' @export
setGeneric("safe.cov2cor", function(M, ...) standardGeneric("safe.cov2cor"))

#' @export
setMethod("safe.cov2cor", "genMatrix", do.safe.cov2cor)

#' Sparsity Ratio
#'
#' Computes the ratio between the non empty elements and the total elements of a matrix
#' @param M a matrix
#' @export
sparsity.ratio <- function(M)
    nnzero(M) / prod(dim(M))

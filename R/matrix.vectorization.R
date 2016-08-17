######## Rewriting some utility functions #####
## safe for sparse matrices

#' Triplet to half vectorization
#'
#' Transforms a triplet representation of a matrix into its half vectorization
#' @param DM Triplet representation i, j, x in a data.table
#' @param nr Number of rows of the matrix
#' @param M the matrix of which DM is the triplet representation
#' @param keep.diag should the diagonal be kept in the half vectorization
#' @export
vech.triplet <- function(DM = mat.to.triplet(DM),                         
                         nr = nrow(M),
                         M,
                         keep.diag = TRUE)
{
    if(keep.diag) F <- DM[, i <= j]
    else F <- DM[, i < j]
    N <- sum(F)
    DM[F, list(i = index.sym(i, j, nr, keep.diag = keep.diag),
               j = 1,
               x)]
}

safe.mat.constr <- function(data,
                            dims,
                            ...)
{
    if(all(dims <= .Machine$integer.max))
        data[, sparseMatrix(i = i,
                            j = j,
                            x = x,
                            dims = dims,
                          ...)]
    else if(require(hugesparse))
        HugeMatrix(data = data,
                   dims = dims,
                   ...)
    else
        stop('Half vectorization failed: size exceeds integer limit')
}

gen.vech <- function(M,
                     keep.diag = TRUE,
                     mat.constr = safe.mat.constr)
{
    if(nrow(M) == 0)
        return(mat.constr(data = data.table::data.table(i = integer(0),
                                                        j = integer(0),
                                                        x = integer(0)),
                          dims = c(0, 0)))
    size <- nn12(nrow(M), keep.diag = keep.diag)
    dms <- c(size, 1)
    
    DM <- mat.to.triplet(M)
    DM <- vech.triplet(DM, nr = nrow(M), keep.diag = keep.diag)

    mat.constr(data = DM,
               dims = dms)
}

#' Half vectorization triplet to matrix triplet
#'
#' @param DV triplet representation of the half vectorization
#' @param nr number of elements in the half vectorization
#' @param V Half vectorization
#' @export
vech.reverse.triplet <- function(DV = mat.to.triplet(V),
                                 nr = findN(nrow(V)) + !keep.diag, ## seems bogus
                                 V,
                                 keep.diag = TRUE)
{
    DV[, c("i", "j") := reverse.index.sym(i, nr, keep.diag = keep.diag)]
}

gen.vech.reverse <- function(V,
                             keep.diag = TRUE,
                             symmetric = TRUE,
                             mat.constr = safe.mat.constr) ## if not symmetric then antisymmetric
{
    if(is.vector(V)) V <- Matrix(V)
    nr <- findN(nrow(V)) + !keep.diag
    DV <- vech.reverse.triplet(mat.to.data.table(V),
                               nr = nr,
                               keep.diag = keep.diag)
                               
    if(symmetric)
        drop0(mat.constr(data = DV,
                         symmetric = TRUE,
                         dims = c(nr, nr)))
    else
    {
        M <- drop0(mat.constr(data = DV,
                              dims = c(nr, nr)))
        M - t(M)
    }
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
                          logger = jlogger::JLoggerFactory("jalgos filter"))
{
    fmat <- sparseMatrix
    vds <- cumsum(vdim)
    N <- last(vds)
    ND <- sum(nn12(vdim))
    jlogger::jlog.debug(logger, "Creating a bdiag transition matrix of dimension", nn12(N), ND, "with:", length(vds), "blocks")
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
                         logger = jlogger::JLoggerFactory("jalgos filter"))
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

#' Reverse half vectorization
#'
#' Take an half vectorization and returns the corresponding matrix.
#' @param V Half vectorization of the matrix
#' @param keep.diag Was the diagonal kept when performing the half vectorization
#' @param symmetric Is the matrix symmetric. If not then the matrix is assumed antisymmetric
#' @export
setGeneric("vech.reverse", function(V, ...) standardGeneric("vech.reverse"))


#' @title Abstract Matrix
#' @description A generic class for matrices. Useful to create object that have slots that are matrices
#' @name genMatrix
#' @import Matrix
#' @importClassesFrom Matrix Matrix
#' @exportClass genMatrix
setClassUnion("genMatrix", c("matrix", "Matrix"))

#' @rdname vech
#' @export
setMethod("vech", "ANY", gen.vech)

#' @rdname vech.reverse
#' @export
setMethod("vech.reverse", "ANY", gen.vech.reverse)


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
#' @param M matrix
#' @param values numeric vector
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
    D <- data.table::data.table(i = di, d = d, I = d)
    U <- data.table::data.table(or = 1:length(I), I)
    data.table::setkey(D, I)
    data.table::setkey(U, I)
    U <- D[U, roll = Inf]
    U[order(or), list(i, j = I - d + i + adj)]
}

#' @describeIn mat.index Assigns value to a matrix by a triplet specification
#' @export
assign.matrix.vect <- function(M, i, j, values)
{
    n <- nrow(M)
    M[mat.index(i, j, n)] <- values
    M
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
                         logger = jlogger::JLoggerFactory("jalgos filter"),
                         ...)
{
    fmat <- sparseMatrix
    vds <- cumsum(vdim)
    N <- last(vds)
    ND <- sum(vdim ^ 2)
    jlogger::jlog.debug(logger, "Creating a bdiag transition matrix of dimension", N ^ 2, ND, "with:", length(vds), "blocks")
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

#' @describeIn vectorization Computes the commuation matrix, i.e matrix K such that K %*%  vec(A) = vec(t(A))
#' @details Function to compute the matrix that will transform M(i, j, k, l) into M(i, l, k, j)
#' This is commonly known as the commutation matrix
#' https://en.wikipedia.org/wiki/Commutation_matrix
#' @export
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

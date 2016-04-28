library(MASS)
library(Matrix)
setClassUnion("genMatrix", c("matrix", "Matrix"))

subvector <- function(V, k = 1:length(V))
{
    if(missing(k)) k = 1:length(V)
    return(V[k])
}

submatrix <- function(M, i = 1:nrow(M), j = 1:ncol(M))
{
    if(missing(i)) i = 1:nrow(M)
    if(missing(j)) j = 1:ncol(M)
    if(is.logical(i)) i = which(i)
    if(is.logical(j)) j = which(j)
    if(length(i) == 1 || length(j) == 1)
    {
        rn = rownames(M)
        cn = colnames(M)
        return(Matrix(M[i, j], length(i), length(j), dimnames = list(rn[i], cn[j])))
    }
    return(M[i, j])
}

#For 3 dimensional arrays
subarray <- function(A,  i = 1:dim(A)[1], j = 1:dim(A)[2], k = 1:dim(A)[3])
{
    if(missing(i)) i = 1:dim(A)[1]
    if(missing(j)) j = 1:dim(A)[2]
    if(missing(k)) k = 1:dim(A)[3]
    if(is.logical(i)) i = which(i)
    if(is.logical(j)) j = which(j)
    if(is.logical(k)) k = which(k)
    if(length(i) == 1 || length(j) == 1 || length(k) == 1)
    {
        if(!is.null(dimnames(A)))
        {
            inames = dimnames(A)[[1]][i]
            jnames = dimnames(A)[[2]][j]
            knames = dimnames(A)[[3]][k]
            ndimn = list(inames, jnames, knames)
        }
        else
        {
            ndimn = NULL
        }
        return(array(A[i, j, k], dim = c(length(i), length(j), length(k)), dimnames = ndimn))
    }
    return(A[i, j, k])
}

#Assume three dimensional array of matrices with last index being the time
array_prod <- function(A, B)
{
    dA = dim(A)
    dB = dim(B)
    if(dA[2] != dB[1]) stop("dimension mismatch")
    if(dA[3] != dB[3]) stop("length mismatch")
    dC = c(dA[1], dB[2], dB[3])
    if(!is.null(dimnames(A)) & ! is.null(dimnames(B))) dmnC = list(dimnames(A)[[1]], dimnames(B)[[2]], dimnames(B)[[3]])
    else dmnC = NULL
    C = array(0, dim = dC, dimnames = dmnC)
    for(i in 1:dA[1])
    {
        for(j in 1:dB[2])
        {
            for(k in 1:dB[1])
            {
                C[i, j, ] = C[i, j, ] + A[i, k, ] * B[k, j, ]
            }
        }
    }
    return(C)
}

array_transpose <- function(A)
{
    dA = dim(A)
    dC = c(dA[2], dA[1], dA[3])
    if(!is.null(dimnames(A)))
    {
        dmnA = dimnames(A)
        dmnC = list(dmnA[[2]], dmnA[[1]], dmnA[[3]])
    }
    else dmnC = NULL
    C = array(0, dim = dC, dimnames = dmnC)
    for(i in 1:dA[1])
    {
        for(j in 1:dA[2])
        {
            C[j, i, ] = A[i, j,]
        }
    }
    C
}

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
#Vec operator on an array of matrix
vec_array <- function(A)
{
     matrix(A, dim(A)[1] * dim(A)[2], dim(A)[3])
}

#Equivalent to Matrix's norm(M, "f")
setMethod("norm", c("vector", "ANY"), function(x, type) norm(as.matrix(x), type))
matrix.norm <- function(M)
{
    norm(M, "f")
}

matrix.norm_inf <- function(M)
{
    norm(M, "i")
}

matrix.scal <- function(M1, M2, normalized = FALSE)
{
    if(normalized) N = matrix.norm(M1)
    else N = 1
    return(sum(M1 * M2) / N ^ 2)
}

matrix.trace <- function(M) sum(diag(M))

######## Rewriting some utility functions #####
#safe for sparse matrices
gen.JF.vech <- function(M,
                        keep_diag = TRUE)
{    
    if(nrow(M) == 0) return(Matrix(0, 0, 0))
    size <- JF.nn12(nrow(M), keep_diag = keep_diag)
    dms <- c(size, 1)
    if(isDiagonal(M))
    {
        if(size < 2 ^ 31)
        {
            if(!keep_diag)
                return(Matrix(0, size, 1))
            return(sparseMatrix(i = JF.index_sym(1:nrow(M), 1:nrow(M), nrow(M)), j = rep(1, nrow(M)), x = diag(M)))
        }
        else
        {
            if(!keep_diag)
                return(STMatrix(DM = data.table(i = numeric(0), j = numeric(0), x = numeric(0)),
                                dims = dms))
            return(STMatrix(DM = data.table(i = JF.index_sym(1:nrow(M), 1:nrow(M), nrow(M)), j = rep(1, nrow(M)), x = diag(M)),
                            dims = dms))
        }
    }
    M <- as(M, "TsparseMatrix")
    if(keep_diag) F <- M@i <= M@j
    else F <- M@i < M@j
    N <- sum(F)
    if(all(dms < 2 ^ 31))
        sparseMatrix(i = JF.index_sym(M@i[F] + 1, M@j[F] + 1, nrow(M), keep_diag = keep_diag),
                     j = rep(1, N),
                     x = M@x[F],
                     dims = dms)
    else
        STMatrix(DM = data.table(i = JF.index_sym(M@i[F] + 1, M@j[F] + 1, nrow(M), keep_diag = keep_diag),
                                 j = rep(1, N),
                                 x = M@x[F]),
                 dims = dms)
}

#bdiag to vec
JF.bdiag_to_vech <- function(vdim, #vector of matrix dimensions
                             logger = JLoggerFactory("jalgos filter"))
{
    fmat = sparseMatrix
    vds = cumsum(vdim)
    N = last(vds)
    ND = sum(JF.nn12(vdim))
    jlog.debug(logger, "Creating a bdiag transition matrix of dimension", JF.nn12(N), ND, "with:", length(vds), "blocks")
    Ivd = lapply(vdim, function(n) rep((1 - n):0, each = n))
    Jvd = lapply(vdim, function(n) rep((1 - n):0, n))
    I = unlist(mapply(Ivd, vds, FUN = "+", SIMPLIFY = FALSE))
    J = unlist(mapply(vds, Jvd, FUN = "+", SIMPLIFY = FALSE))
    F = I <= J
    I = I[F]
    J = J[F]
    Ibd = 1:ND
    Jbd = JF.index_sym(I, J, N)
    dims = c(JF.nn12(N), ND)
    if(any(is.na(as.integer(dims)))) fmat = HugeMatrix
    fmat(i = Jbd, j = Ibd, x = 1, dims = dims)
}

JF.diag_to_vech <- function(n,
                            logger = JLoggerFactory("jalgos filter"))
{
    JF.bdiag_to_vech(rep(1, n),
                     logger = logger)
}

safeGeneric("JF.vech", function(M, ...) standardGeneric("JF.vech"))
setMethod("JF.vech", "genMatrix", gen.JF.vech)



assign_array_vect <- function(M, I, values, dimn = dim(M))
{
    dimn = cumprod(c(1, dimn[ -length(dimn)]))
    M[as.vector(1 + (I-1) %*% dimn)] = values
    M
}

#For half vectorization
#n ( n+1) = 2X
#(n+1/2)² - 1/4 = 2X
#n = sqrt(2X + 1/4) - 1/2
findN <- function(X,
                  half_vec = TRUE,
                  check_integer = TRUE)
{
    if(half_vec) n = sqrt(2 * X + 1/4) - 1/2
    else n = sqrt(X)
    if(check_integer & abs(round(n) - n) > sqrt(.Machine$double.eps))
    {
        stop("The number supplied does not correspond to a vectorization")
    }
    n
}

#Going from a matrix that operates on vec to a matrix that operates on vech

#Function that will transform a vech(n,n) to a vec(n,n)

#We need this one a lot
JF.nn12 <- function(n, keep_diag = TRUE)  n * (n - 1 + 2 * keep_diag) / 2

JF.mat_index <- function(i, j, n) (j - 1) * n + i

JF.index_sym <- function(i, k, n, keep_diag = TRUE)
{
    o = i
    i = pmin(i, k)
    k = pmax(k, o)
    adj = !keep_diag
    (i - 1) * (n - adj) + (k - 1) - i * (i - 1) / 2  - adj + 1
}

JF.rev_index_sym <- function(I, n, keep_diag = TRUE)
{
    if(length(I) == 0) return(list(i = integer(0), k = integer(0)))
    di = 1:n
    adj = !keep_diag
    d = JF.index_sym(di, di + adj, n, keep_diag = keep_diag)
    D = data.table(i = di, d = d, I = d)
    U = data.table(or = 1:length(I), I)
    setkey(D, I)
    setkey(U, I)
    U = D[U, roll = Inf]
    U[order(or), list(i, j = I - d + i + adj)]
}

JF.vech_to_vec <- function(n,
                           skew_sym = FALSE)
{
    fmat = sparseMatrix
    if(n < 0L)
    {
        stop("n must be a positive integer")
    }
    
    if(n == 0L)
    {
        return(Matrix(0, 0, 0))
    }
    
    I = rep(1:n, n)
    J = rep(1:n, each = n)
    val = 1
    if(skew_sym)
    {
        F = I != J
        I = I[F]
        J = J[F]
        val = ifelse(I < J, 1, -1)
    }
    ID1 = (J - 1) * n + (I - 1) + 1
    ID2 = JF.index_sym(I, J, n, keep_diag = !skew_sym)
    dims = c(n ^ 2, JF.nn12(n, keep_diag = !skew_sym))
    if(any(is.na(as.integer(dims)))) fmat = HugeMatrix
    fmat(i = ID1, j = ID2, x = val, dims = dims)
}

## The symmetric

get_upper_tri_indices <- function(n, keep_diag = TRUE)
{
    I = unlist(mapply(function(i, j) rep(i, j), 1:n, n:1))
    J = unlist(sapply(1:n, function(i, n) i:n, n = n))
    if(!keep_diag)
    {
        F = I < J
        I = I[F]
        J = J[F]
    }
    list(I = I, ID = (J - 1) * n + (I - 1) + 1, J = J)
}

JF.vec_to_vech <- function(n, skew_sym = FALSE)
{
    fmat = sparseMatrix
    if ( n < 0L )
    {
        stop("n must be a positive integer")
    }
    
    if( n == 0L)
    {
        return(Matrix(0, 0, 0))
    }

    IUT = get_upper_tri_indices(n, keep_diag = !skew_sym)
    ID2 =  JF.index_sym(IUT$I, IUT$J, n, keep_diag = !skew_sym)
    dims = c(JF.nn12(n, keep_diag = !skew_sym), n ^ 2)
    if(any(is.na(as.integer(dims)))) fmat = HugeMatrix
    fmat(i = ID2, j = IUT$ID, x = 1, dims = dims)    
}


JF.vech.reverse <- function(V,
                            keep_diag = TRUE,
                            symmetric = TRUE) ## if not symmetric then antisymmetric
{
    if(is.vector(V)) V = Matrix(V)
    DV = mat_to_data.table(V)
    nd = findN(nrow(V)) + !keep_diag
    DV[, c("i", "j") := JF.rev_index_sym(i, nd, keep_diag = keep_diag)]
    if(symmetric) return(drop0(dsparseMatrix(DV[, list(i = i, j = j, x =  x)], symmetric = TRUE, dims = c(nd, nd))))
    M = drop0(dsparseMatrix(DV[, list(i = i, j = j, x = x)], dims = c(nd, nd)))
    M - t(M)
}

#Full Vectorization
gen.JF.vec <- function(M,
                       ...)
{
    if(is(M, "denseMatrix")) M = as.matrix(M)
    N = prod(dim(M))
    dim(M) = c(N, 1)#Keep it sparse
    M    
}

gen.JF.vec.reverse <- function(V,
                               ...)
{
    if(is.vector(V)) V = Matrix(V, length(V))
    n = findN(nrow(V), FALSE)
    V = Matrix(V)
    dim(V) = c(n, n)
    V
}

JF.vec.reverse.dsy <- function(V)
{
    n = findN(nrow(V), FALSE)
    V = as(as.matrix(V), "dgeMatrix")
    dim(V) = c(n, n)
    forceSymmetric(V)
}

safeGeneric("JF.vec", gen.JF.vec)
safeGeneric("JF.vec.reverse", gen.JF.vec.reverse)

#bdiag to vec
JF.bdiag_to_vec <- function(vdim, #vector of matrix dimensions
                            logger = JLoggerFactory("jalgos filter"),
                            ...)
{
    fmat = sparseMatrix
    vds = cumsum(vdim)
    N = last(vds)
    ND = sum(vdim ^ 2)
    jlog.debug(logger, "Creating a bdiag transition matrix of dimension", N ^ 2, ND, "with:", length(vds), "blocks")
    Ivd = lapply(vdim, function(n) rep((1 - n):0, each = n))
    Jvd = lapply(vdim, function(n) rep((1 - n):0, n))
    I = unlist(mapply(Ivd, vds, FUN = "+", SIMPLIFY = FALSE))
    J = unlist(mapply(vds, Jvd, FUN = "+", SIMPLIFY = FALSE))
    I = I
    J = J
    Ibd = 1:ND
    Jbd = JF.mat_index(I, J, N)
    dims = c(N ^ 2, ND)
    if(any(is.na(as.integer(dims)))) fmat = HugeMatrix
    fmat(i = Jbd, j = Ibd, x = 1, dims = dims)
}

##########Random Cov matrices##########

random_matrix <- function(n,
                          p = n)
{
    matrix(rnorm(n * p), n, p)
}

random_sym_matrix <- function(n,
                              scaling = 1)
{
    X = rnorm(n * (n+1) / 2) * scaling
    forceSymmetric(JF.vech.reverse(X))
}

random_cov_matrix <- function(n, tot_vol = n)
{
    if(n == 1) R = Matrix(rexp(n))
    else
    {
        W = rnorm(n * (n +1) / 2)
        M = JF.vech.reverse(W)
        R = M %*% t(M)
    }
    R / total_vol(R) * tot_vol
}

####### Random sparse matrixes #######

random_sparse_matrix <- function(nr = n,
                                 nc = n,
                                 n,
                                 N,
                                 rgen = rnorm)
{
    sparseMatrix(i = sample(1:nr, N, replace = TRUE),
                 j = sample(1:nc, N, replace = TRUE),
                 x = rgen(N),
                 dims = c(nr, nc))
}

random_sparse_cov_matrix <- function(nr = n,
                                     nc = n,
                                     n,
                                     N)
{
    M = random_sparse_matrix(nr, nc, N = N)
    M %*% t(M)
}

#More utility functions
add_ellipsis <- function(f) function(x, ...) f(x)
funcM <- function(M, func, ...)
{
    n = nrow(M)
    E = eigen(M, symmetric = TRUE)
    V = Matrix(E$vectors)
    D = E$values
    V %*% Diagonal(n, x = func(D, ...)) %*% t(V)
}

sqrtM <- function(M,
                  eps = .Machine$double.eps)
{
    funcM(M, function(D) sqrt(ifelse(D > -eps & D < 0, 0, D)))
}

absM <- function(M) funcM(M, func = add_ellipsis(abs))
posD <- function(D,
                 ...)
{
    ifelse(D > 0, D, 0)
}
posM <- function(M, ...) funcM(M, func = posD, ...)
expM <- function(M, ...) funcM(M, func = add_ellipsis(exp))
logM <- function(M, ...) funcM(M, func = add_ellipsis(log))

eigen_with_names <- function(M, ...)
{
    E = eigen(M, ...)
    rownames(E$vectors) = rownames(M)
    E
}

svd_with_names <- function(M, ...)
{
    S = svd(M, ...)
    rownames(S$u) = rownames(M)
    rownames(S$v) = colnames(M)
    S
}

#shifting
shift_mat <- function(X, N = 1, ...)
{
    if(N == 0) return(X)
    L = nrow(X)
    P = ncol(X)
    padding = matrix(nrow = abs(N), ncol = P)
    I = 1:L
    keep = submatrix(X, I + N > 1 & I + N < L + 1)
    if(N > 0) rBind(padding, keep)
    else rBind(keep, padding)
}

setMethod("shift", signature = c("matrix"), shift_mat)

setMethod("shift", signature = c("Matrix"), shift_mat)

#Utility to resize matrix and keep data in place
util.resize <- function(M,
                        nrows,
                        ncols,
                        padding.val = 0,
                        zero.based = FALSE,
                        ...)
{
    orows = nrow(M)
    ocols = ncol(M)
    rdiff = nrows - orows
    cdiff = ncols - ocols
    I = 1:nrows
    J = 1:ncols
    if(zero.based)
    {
        I = I - 1
        J = J - 1
    }
    if(rdiff < 0 ) M = M[I, ]
    else if(rdiff > 0) M = row_bind(M, Matrix(padding.val, rdiff, ocols))

    if(cdiff < 0) M = M[, J]
    else if(cdiff > 0) M = col_bind(M, Matrix(padding.val, nrows, cdiff))
    M
}

safeGeneric("resize", util.resize)

safeGeneric("reserve", util.resize)

safeGeneric("row_bind", function(M1, M2) rBind(M1, M2))
safeGeneric("col_bind", function(M1, M2) cBind(M1, M2))

rows <- function(M)
{
    if(nrow(M) > 0) 1:nrow(M)
    else integer(0)
}

cols <- function(M)
{
    if(ncol(M) > 0) 1:ncol(M)
    else integer(0)
}

###derivation of the square root of a matrix

dsqrtM <- function(M0,
                   M)
{
    sM0 = sqrtM(M0)
    Id = Diagonal(nrow(M0))
    KS1 = solve(sM0 %x% Id + Id %x% sM0)
    dsM = JF.vec.reverse(KS1 %*% JF.vec(M))
    dsM
}

d2sqrtM <- function(M0,
                    M)
{
    sM0 = sqrtM(M0)
    Id = Diagonal(nrow(M0))
    KS1 = solve(sM0 %x% Id + Id %x% sM0)
    dsM = JF.vec.reverse(KS1 %*% JF.vec(M))
    d2SM = JF.vec.reverse(-2 * KS1 %*% JF.vec(dsM %*% dsM))
    d2SM / 2
}

Matrix.ginv <- function(X, tol = sqrt(.Machine$double.eps)) 
{
    Id = Diagonal(nrow(X))
    solve(X + tol * Id)
}

safeGeneric("ginv", ginv)
setMethod("ginv", "Matrix", Matrix.ginv)

safeGeneric("gsolve", function(a, b, ...) ginv(a) %*% b)
setMethod("gsolve", c(a = "Matrix", b = "ANY"), function(a, b, eps = sqrt(.Machine$double.eps)) solve(a + eps * Diagonal(n = nrow(a)), b))

psolve <- function(M,
                   b,
                   var_thresh = .95)
{
    S = svd(M)
    PVE = cumsum(S$d) / sum(S$d)
    ico = which(PVE > var_thresh)[1]
    IP = 1:ico
    M1 = S$v[, IP] %*% Diagonal(x = 1 / S$d[1:ico]) %*% t(S$u[, IP])
    if(missing(b)) return(M1)
    M1 %*% b
}

mtdt.add.row.col.names <- function(D,
                                   M,
                                   one_based)
{
    if(!is.null(rownames(M)))
    {
        D[, i.names := rownames(M)[i + !one_based]]
    }
    
    if(!is.null(colnames(M)))
    {
        D[, j.names := colnames(M)[j + !one_based]]
    }
    
    D    
}

Matrix.mat_to_data.table <- function(M,
                                     one_based = TRUE,
                                     with.names = FALSE)
{
    TM <- as(M, "TsparseMatrix")
    D <- data.table(i = TM@i + one_based, j = TM@j + one_based, x = TM@x)
    if(with.names)
        mtdt.add.row.col.names(D,
                               M,
                               one_based)
    else
        D
}

matrix.mat_to_data.table <- function(M,
                                     one_based = TRUE,
                                     with.names = FALSE)
{
    D <- data.table(i = as.vector(row(M)) - !one_based, j = as.vector(col(M)) - !one_based, x = as.vector(M))
    if(with.names)
        mtdt.add.row.col.names(D,
                               M,
                               one_based)
    else
        D
}
    
sym.mat_to_data.table <- function(M,
                                  one_based = TRUE,
                                  ...)
{
    D <- callNextMethod(M, one_based, ...)
    if(nrow(D[i > j]) == 0)
    {
        D = rbind(D, D[i < j, list(i = j, j = i, x)])
    }
    else if(nrow(D[i < j]) == 0)
    {
       D = rbind(D, D[i > j, list(i = j, j = i, x)])
    }
    D
}

safeGeneric("mat_to_data.table", function(M, ...) standardGeneric("mat_to_data.table"))
setMethod("mat_to_data.table", "Matrix", Matrix.mat_to_data.table)
setMethod("mat_to_data.table", "matrix", matrix.mat_to_data.table)
setMethod("mat_to_data.table", "diagonalMatrix", function(M, one_based = TRUE)  mtdt.add.row.col.names(data.table(i = 1:nrow(M) - !one_based, j = 1:nrow(M) - !one_based, x = diag(M)), M, one_based))
setMethod("mat_to_data.table", "symmetricMatrix", sym.mat_to_data.table)


array_to_data.table <- function(A,
                                one_based = TRUE)
{
    LI = list()
    nd = length(dim(A))
    N = length(A)
    for(i in 1:nd)
    {
        di = dim(A)[i]
        do = dim(A)[-(i:nd)]
        ndo = prod(do)        
        LI <- c(LI,
                list((((1:N) - 1) %/% ndo) %% di + one_based))
    }
    names(LI) = paste0("i", 1:nd)
    LI = as.data.table(LI)
    LI[, x := as.vector(A)]
    LI
}

## give the indexes in the order first
make_array <- function(...,
                       x,
                       dim,
                       one_based = TRUE)
{
    A = array(0, dim = dim)
    nd = length(dim)
    LI = list(...)
    inds = rep(0L, length(x))
    for(i in 1:nd)
    {
        di = dim(A)[i]
        do = dim(A)[-(i:nd)]
        ndo = prod(do)
        cind = LI[[i]]
        inds = inds + (cind - one_based) * ndo 
    }
    inds = inds + 1L
    A[inds] = x
    A
}


make_symmetric <- function(M) M + t(M) - Diagonal(x = diag(M))
## Partial Kronecker

## Performs Conj(M1) %x% M2
partial_kronecker <- function(M1,
                              M2,
                              DI,
                              DJ = DI[, list(k = i, l = j, J = I)],
                              dims = c(DI[, max(I)], DJ[, max(J)]),
                              half_vec = TRUE,
                              real = TRUE,
                              logger = JLoggerFactory("Jalgos Algebra"))
{
    DI = DI[, list(i, j, I)]
    if(nrow(DI[i > j]) == 0)
    {
        DI = rbind(DI, DI[i < j, list(i = j, j = i, I)])
    }
    DM1 = mat_to_data.table(M1)
    DM2 = mat_to_data.table(M2)
    setnames(DM1, c("i", "k", "x1"))
    setnames(DM2, c("j", "l", "x2"))
    if(DM1[, is.complex(x1)] || DM2[, is.complex(x2)])
    {
        DM1[, c("ix1", "rx1") := list(Im(x1), Re(x1))]
        DM2[, c("ix2", "rx2") := list(Im(x2), Re(x2))]
        DM1[, x1 := NULL]
        DM2[, x2 := NULL]
    }
    KR = merge(DM1, DI, by = "i", allow.cartesian = TRUE)
    if(half_vec) KR = KR[i <= j]
    KR =  merge(KR, DJ, by = "k", all.x = TRUE, allow.cartesian = TRUE)
    KR = merge(KR, DM2, by = c("j", "l"), allow.cartesian = TRUE)
    if("x1" %in% names(KR)) dsparseMatrix(KR[, list(x = sum(x1 * x2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
    else if(real) dsparseMatrix(KR[, list(x = sum(rx1 * rx2 + ix1 * ix2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
    else
    {
        RM = dsparseMatrix(KR[, list(x = sum(rx1 * rx2 - ix1 * ix2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
        IM = dsparseMatrix(KR[, list(x = sum(rx1 * ix2 + ix1 * rx2)), by = list(I, J)][, list(i = I, j = J, x = x)], dims = dims)
        matrix(complex(real = as.matrix(RM), imaginary = as.matrix(IM)), nrow = nrow(IM), ncol = ncol(RM))
    }
}

#is.finite fails in case prod(dims) is passed the integer bound
sum_non_finite <- function(M) nrow(mat_to_data.table(M)[!is.finite(x)])
    
#### Memory safe kronecker product #######

cor_dec <- function(S,
                    tol = 10 * .Machine$double.eps)
{
    sqD = sqrt(posD(diag(S)))
    D = Diagonal(x = sqD)
    D1 = Diagonal(x = ifelse(abs(sqD) > tol, 1 / sqD, 0))
    C = D1 %*% S %*% D1
    list(D = D,
         C = C)
}

add_new_krchunk <- function(D1,
                            D2,
                            DKR,
                            target_size,
                            n,
                            m,
                            keep_diag,
                            logger = NULL)
{
    jlog.debug(logger, "Computing cartesian product of tables of sizes:", nrow(D1), nrow(D2), "result's size:", nrow(D1) * nrow(D2))
    D = cartesian_data_table(D1, D2)[, list(i = (i1 - 1) * n + i2, j = (j1 - 1) * m + j2, x = x1 * x2)]
    tot_size = nrow(D) + nrow(DKR)
    pb = 1 - target_size / tot_size
    if(pb <= 0) return(rbind(DKR, D))
    jlog.debug(logger, "Keeping:", 100 * (1 - pb), "% of the result")
    pbx = quantile(c(D[, abs(x)], DKR[, abs(x)]), probs = pb)
    jlog.debug(logger, "Cropping values lesser than:", pbx, "in absolute value")
    rbind(D[abs(x) >= pbx | (keep_diag & i == j)], DKR[abs(x) >= pbx  | (keep_diag & i == j)])
}

## Function to limit memory usage of the kronecker product
## The approximation error should be small if there are a lot fo values<0.01 in the correlation matrix
## The memory taken by a Matrix (in the triplet form) with N non zero elements is roughly 16 * N / 2 ^ 20 MBs (two integer vectors and a double vector: 4 + 4 + 8)
## If you want to limit the memory to 1Gb you need to set the size_limit to 2 ^ 26
## Keep in mind however that the chunking process will use twice the size provided. If you want to limit the mmeory usage to 2Gb give 1Gb as parameter
mem_safe_kronecker <- function(S1,
                               S2,
                               krsize_limit = krmem_limit * 2 ^ 26,
                               krmem_limit = Inf, ## In gbs
                               keep_diag = FALSE,
                               ...,
                               logger = NULL)
{
    jlog.debug(logger, "Computing a memory safe kronecker product by cropping the least significant values")
    nS1 = as.numeric(nnzero(S1))
    nS2 = as.numeric(nnzero(S2))
    expm = nS1 * nS2
    jlog.debug(logger, "Number of non zero elements in S1:", nS1, "in S2:", nS2, "in result:", expm, "limit:", krsize_limit, "ratio:", krsize_limit / expm)
    if(expm < krsize_limit) return(S1 %x% S2)
    CS1 = cor_dec(S1)
    CS2 = cor_dec(S2)
    DS1 = mat_to_data.table(CS1$C)
    DS2 = mat_to_data.table(CS2$C)
    DD = CS1$D %x% CS2$D    
    chunksize = krsize_limit %/% (2 * nrow(DS2)) ## Will  fail if it's zero. Hopefully an unrealistic case
    if(chunksize == 0)
    {
        jlog.error(logger, "Cannot divide S1, S2 too big")
        stop("No viable chunksize")
    }
    jlog.debug(logger, "Dividing DS1 into chunks of size:", chunksize, "number of chunks:", nrow(DS1) %/% chunksize)
    n = 1
    N = chunksize
    DKR = data.table(i = integer(0), j = integer(0), x = numeric(0))
    while(n <= nrow(DS1))
    {
        N = min(nrow(DS1), N)
        jlog.debug(logger, "Dealing with chunk from:", n, "to", N)
        DKR = add_new_krchunk(DS1[n:N], DS2, DKR, krsize_limit, nrow(S1), ncol(S2), keep_diag = keep_diag, logger = logger)
        jlog.debug(logger, N / nrow(DS1) * 100, "% done")
        n = n + chunksize
        N = N + chunksize
    }
    jlog.debug(logger, "Result has:", nrow(DKR), "nonzero elements over an expected total of:", expm) 
    CC = dsparseMatrix(DKR[, list(i = i, j = j, x = x)], dims = dim(S1) * dim(S2))
    gc()
    DD %*% CC %*% DD
}


safeGeneric("non_finite_elements", function(x) sum(!is.finite(x)))
setMethod("non_finite_elements", c(x = "sparseMatrix"), function(x) non_finite_elements(x@x))

bdiag_with_names <- function(L)
{
    M = bdiag(L)
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

jacobi_inv <- function(a,
                       b,
                       p1 = Diagonal(x = 1 / diag(M)), ## Preconditioner the closer from M the better,
                       p = Diagonal(x = diag(M)),
                       x = p1 %*% b, ## First guess
                       tol = sqrt(.Machine$double.eps),
                       iter_max = 1000,
                       logger = JLoggerFactory("Jalgos Algebra"))
{
    if(nrow(b) != nrow(a))
    {
        jlog.error(logger, "Dimension of b and a don't match:", dim(a), dim(b))
        stop("Dimension mismatch")
    }
    r = a - p
    inm = matrix.norm(Diagonal(nrow(M)) -  p1 %*% a)
    if(inm > 1) jlog.warn(logger, "The jacobi method may not convergence as the preconditioner is too far from the real inverse. Norm(I - PA):", inm)
    err = matrix.norm(a %*% x - b)
    iter = 1
    while(err > tol)
    {
        if(!is.finite(err))
        {
            jlog.error(logger, "Error is not finite:", err)
            stop("non finite error")
        }
        if(iter == iter_max)
        {
            jlog.error(logger, "Jacobi method did not converge after", iter_max,"iterations, current error:", err, "tolerance:", tol)
            stop("maxiter")
        }
        iter = iter + 1
        x = p1 %*% (b - r %*% x)
        err = matrix.norm(a %*% x - b)
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
                   half_vec = FALSE)
{
    if(n == 1) return(Diagonal(p))
    if(p == 1) return(Diagonal(n))
    findex = JF.mat_index
    if(half_vec) findex = JF.index_sym
    J = rep(1:p, n)
    L = rep(1:n, each = p)
    if(half_vec)
    {
        F = J <= L
        J = J[F]
        L = L[F]
    }
    I1 = findex(J, L, p)
    I2 = findex(L, J, n)
    #Need to add a switch here in case I1 or I2 exceeds 2^32-1
    sparseMatrix(I1, I2, x = 1)
}

JF.S1 <- function(S,
                  half_vec = TRUE,
                  ...,
                  logger = JLoggerFactory("jalgos filter"))
{
    jlog.debug(logger, "Computing S1 for a matrix of dimension:", nrow(S))
    n = nrow(S)
    I = Diagonal(n ^ 2)
    P = JF.PE4(n)
    S1 = (I + P) %*% (S %x% S)
    if(half_vec)
    {
        D = JF.vec_to_vech(n)
        D %*% S1 %*% t(D)
    }
    else S1
}

## Efficiently computes t(D1) %*% (M %x% M) %*% t(D) for a symmetric matrix
## Does not work!!
half_kronecker <- function(M)
{
    nm = nrow(M)
    DM = mat_to_data.table(M)
    DM = DM[i <= j, list(I = JF.index_sym(i, j, nm), x = x)]
    DR = data.table(i = 1:nm)
    DR = DR[, list(k = (i:nm)), by = i]
    DR[, I := JF.index_sym(i, k, nm)]
    DC = DR[, list(j = i, l = k, J = JF.index_sym(i, k, nm))]
    f = function(I){
        if(I < 2) integer(0)
        else 1:(I-1)
    }
    DR = DR[, list(J = f(I)), by = list(i, k, I)]
    setkey(DC, J)
    setkey(DR, J)
    DR = DC[DR]
    DR = DR[, list(i, j, k, l, I, J, IM = JF.index_sym(i, j, nm), JM = JF.index_sym(k, l, nm))]
    setkey(DM, I)
    setkey(DR, IM)
    DR = DR[DM]
    setnames(DR, "x", "xr")
    setkey(DR, JM)
    DR = DR[J(DM[, I])]
    DR[DM, xr := xr * x]
    MR = dsparseMatrix(DR[, list(i = I, j = J, x = xr)], symmetric = TRUE, dims = c(JF.nn12(nm), JF.nn12(nm)))
    
}

########

non_zero_indices <- function(M)
{
    DT = as.data.table(which(M != 0, arr.ind = TRUE))
    setnames(DT, c("i", "j"))
    DT
}

is.identity <- function(M)
{
    nrow(M) == ncol(M) && nnzero(M) == nrow(M) && all(diag(M) == 1)
}

########

gen_inverse <- function(a,
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
    n1 = exp %/% 2
    n2 = exp - n1
    mat.exp(M, n1) %*% mat.exp(M, n2)    
}

setGeneric("%^%", mat.exp)

## Very sparse big matrix exhibit a weird behaviour when multiplied. STMatrix (for "Stay triplet") class is here to fix that
## Only way to handle huge matrix with dimensions exceeding integer limit


STMatrix <- setClass("STMatrix",
                     slots = c(data = "data.table",
                               dims = "index",
                               dmnames = "list"))

STM.mult <- function(x, y)
{
    Dx <- mat_to_data.table(x)
    Dy <- mat_to_data.table(y)
    setnames(Dx, "j", "k")
    setnames(Dy, "i", "k")
    D <- merge(Dx, Dy, by = "k")
    ## Tries to return a triplet if D is small STMatrix otherwise
    dms <- c(nrow(x), ncol(y))
    dmns <- list(rownames(x), rownames(y))
    if(all(dms < 2 ^ 31))
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
                     DM = mat_to_data.table(M),
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
    if(all(object@dims < 2 ^ 31))        
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

setMethod("%*%", c("STMatrix", "genMatrix"), STM.mult)
setMethod("%*%", c("genMatrix", "STMatrix"), STM.mult)

setMethod("show", "STMatrix", STM.show)
setMethod("t", "STMatrix", STM.t)

safeGeneric("as.Matrix", function(M) standardGeneric("as.Matrix"))
setMethod("as.Matrix", "STMatrix", STM.as.Matrix)

setMethod("show", "STMatrix", STM.show)
setMethod("dim", "STMatrix", function(x) x@dims)

setMethod("[", c("STMatrix", "missing", "missing", "missing"), function(x) as.Matrix(x))
setMethod("[", c("STMatrix", "index", "missing", "ANY"), function(x, i, j, drop = FALSE) x[][i, , drop = drop])
setMethod("[", c("STMatrix", "missing", "index", "ANY"), function(x, i, j, drop = FALSE) x[][, j, drop = drop])
setMethod("[", c("STMatrix", "index", "index", "ANY"), function(x, i, j, drop = FALSE) x[][i, j, drop = drop])
setMethod("*", c("STMatrix", "ANY"), function(e1, e2) e1[] * e2)
setMethod("*", c("ANY", "STMatrix"), function(e1, e2) e1 * e2[])
setMethod("/", c("STMatrix", "ANY"), function(e1, e2) e1[] / e2)
setMethod("+", c("STMatrix", "ANY"), function(e1, e2) e1[] + e2)
setMethod("+", c("ANY", "STMatrix"), function(e1, e2) e1 + e2[])
setMethod("-", c("STMatrix", "missing"), function(e1, e2) - e1[])
setMethod("-", c("STMatrix", "ANY"), function(e1, e2) e1[] - e2)
setMethod("-", c("ANY", "STMatrix"), function(e1, e2) e1 - e2[])
setMethod("%*%", c("STMatrix", "ANY"), function(x, y) x[] %*% y)
setMethod("%*%", c("ANY", "STMatrix"), function(x, y) x %*% y[])
setMethod(kronecker, c(X = "STMatrix", Y = "ANY"), function(X, Y) X[] %x% Y)
setMethod(kronecker, c(X = "ANY", Y = "STMatrix"), function(X, Y) X %x% Y[])
setMethod(JF.vech, c(M = "STMatrix"), function(M) JF.vech(M[]))
setMethod(JF.vec, c(M = "STMatrix"), function(M, ...) JF.vec(M[], ...))

setMethod(col_bind, c(M1 = "STMatrix", M2 = "ANY"), function(M1, M2) col_bind(M1[], M2))
setMethod(col_bind, c(M1 = "ANY", M2 = "STMatrix"), function(M1, M2) col_bind(M1, M2[]))
setMethod(row_bind, c(M1 = "STMatrix", M2 = "ANY"), function(M1, M2) row_bind(M1[], M2))
setMethod(row_bind, c(M1 = "ANY", M2 = "STMatrix"), function(M1, M2) row_bind(M1, M2[]))
setMethod("t", c(x = "STMatrix"), function(x) t(x[]))
setMethod(is.na, c(x = "STMatrix"), function(x) is.na(x[]))
setMethod("non_finite_elements", c(x = "STMatrix"), function(x) non_finite_elements(x[]))
safeGeneric("total_vol", total_vol)
setMethod("total_vol", c(S = "STMatrix"), function(S) sum(diag(S[])))
setMethod("diag", c(x = "STMatrix", nrow = "ANY", ncol = "ANY"), function(x) diag(x[]))
setMethod("mat_to_data.table", "STMatrix", function(M) copy(M@data))
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

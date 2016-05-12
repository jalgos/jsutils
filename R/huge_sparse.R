#' HugeMatrix
#'
#' Implementation of triple matrix that can have indices that exceeds R's integer limit (2 ^ 31).
#' @details Set to overcome int32 limitation on indexation in R. \cr
#' The implementation is just triplet representation with a data.table. \cr
#' We use doubles to represent the indices. This limits the size of our new HugeMatrix to 2^53 - 1 \cr
#' @importClassesFrom data.table data.table
#' @exportClass HugeMatrix
setClass("HugeMatrix",
         slots = c(data = "data.table",
                    dims = "index",
                    dimnames = "list"))
#' HugeMatrix
#'
#' @param M Matrix to convert
#' @param DM Triplet form i, j, x needed
#' @param dims Dimension of the Matrix. Defaults to the dimension of M or the maximum of i and j
#' @param dimnames list with two entries, the first is the row names and the second the column names. Defaults to the dimension names of M
#' @export
HugeMatrix <- function(M,
                       DM = mat.to.data.table(M),
                       dims,
                       dimnames)
{
    if(!missing(M) && is(M, "HugeMatrix")) return(M)
    
    if(missing(dims) && missing(M)) dims <- DM[, c(max(i), max(j))]
    else if(missing(dims)) dims <- dim(M)
    
    if(missing(dimnames) && missing(M)) dimnames <- list(NULL, NULL)
    else if(missing(dimnames)) dimnames <- base::dimnames(M)
    
    new("HugeMatrix",
        data = data.table::as.data.table(DM),
        dims = dims,
        dimnames = dimnames)
}

HM.dim <- function(x) x@dims
HM.dimnames <- function(x) x@dimnames

HM.lscal.op <- function(HM,
                        sc,
                        op)
{
    update.obj(HM,
               data = data.table::copy(HM@data)[, x := op(x, sc)])
}

HM.rscal.op <- function(HM,
                        sc,
                        op)
{
    update.obj(HM,
               data = data.table::copy(HM@data)[, x := op(sc, x)])
}

HM.unary.minus <- function(e1,
                           e2)
{
    update.obj(e1,
               data = data.table::copy(e1@data)[, x := -x])
}

HM.mat.sum <- function(e1,
                       e2)
{
  if(ncol(e1) != ncol(e2) || nrow(e1) != nrow(e2)) stop("dimension mismatch")
  if(!is(e1, "HugeMatrix")) e1 <- HugeMatrix(e1)
  if(!is(e2, "HugeMatrix")) e2 <- HugeMatrix(e2)
  D <- rbind(e1@data, e2@data)
  update.obj(e1,
             data = D[, list(x = sum(x)), by = list(i, j)])
}

HM.mat.mult <- function(x,
                        y)
{
    Dx <- mat.to.data.table(x)
    Dy <- mat.to.data.table(y)
    data.table::setnames(Dx, "j", "k")
    data.table::setnames(Dy, "i", "k")
    D <- data.table::merge(Dx, Dy, by = "k")
    ## Tries to return a triplet if D is small HugeMatrix otherwise
    dms <- c(nrow(x), ncol(y))
    dmns <- list(rownames(x), rownames(y))
    if(all(dms <= .Machine$integer.max))
        dsparseMatrix(D[, list(i, j, x = x.x * x.y)],
                      giveCsparse = FALSE,
                      dims = dms,
                      dimnames = dmns) ## Too restricting to return a HugeMatrix
    else
        HugeMatrix(DM = D[, list(x = x.x * x.y), by = list(i, j)],
                 dims = dms,
                 dimnames = dmns)
}

HM.kronecker <- function(X,
                         Y)
{
    Dx <- mat.to.data.table(x)
    Dy <- mat.to.data.table(y)
    D <- cartesian.data.table(Dx, Dy)
    new("HugeMatrix", data = D[, list(i = (i1 - 1) * nrow(X) + i2, j = (j1 - 1) * ncol(X) + j2, x = x1 * x2)],
        dim = dim(X) * dim(Y), dimnames = list(NULL, NULL))
}

HM.as.Matrix <- function(M)
{
    dsparseMatrix(M@data[, list(i, j, x)],
                  giveCsparse = FALSE,
                  dims = M@dims,
                  dimnames = M@dimnames)
}

HM.show <- function(object)
{
    if(all(object@dims <= .Machine$integer.max))        
        show(as.Matrix(object))
    else
        callNextMethod(object)
}

HM.t <- function(x)
{
    HugeMatrix(DM = x@data[i = j, j = i, x = x],
             dims = rev(x@dims),
             dimnames = rev(x@dimnames))
}

HM.col.bind <- function(M1,
                        M2)
{
    if(nrow(M1) != nrow(M2)) stop("dimension mismatch")
    if(!is(M1, "HugeMatrix")) M1 <- HugeMatrix(M1)
    if(!is(M2, "HugeMatrix")) M2 <- HugeMatrix(M2)    
    d2 <- data.table::copy(M2@data)
    d2[, j := j + ncol(M1)]
    new("HugeMatrix",
        data = rbind(M1@data, d2),
        dim = c(ncol(M1) + ncol(M2), nrow(M1)),
        dimnames = list(rownames(M1), c(colnames(M1), colnames(M2))))
}

HM.row.bind <- function(M1,
                        M2)
{
    if(ncol(M1) != ncol(M2)) stop("dimension mismatch")
    if(!is(M1, "HugeMatrix")) M1 = HugeMatrix(M1)
    if(!is(M2, "HugeMatrix")) M2 = HugeMatrix(M2)
    d2 <- data.table::copy(M2@data)
    d2[, i := i + nrow(M1)]
    new("HugeMatrix", data = rbind(M1@data, d2), dim = c(ncol(M1), nrow(M1) + nrow(M2)), dimnames = list(rownames(M1), c(colnames(M1), colnames(M2))))
}


#' @export
setMethod("show", "HugeMatrix", HM.show)

#' @export
setMethod("dim", "HugeMatrix", HM.dim)

#' @export
setMethod("[", c("HugeMatrix", "missing", "missing", "missing"), function(x) as.Matrix(x))

#' @export
setMethod("[", c("HugeMatrix", "index", "missing", "ANY"), function(x, i, j, drop = FALSE) x[][i, , drop = drop])

#' @export
setMethod("[", c("HugeMatrix", "missing", "index", "ANY"), function(x, i, j, drop = FALSE) x[][, j, drop = drop])

#' @export
setMethod("[", c("HugeMatrix", "index", "index", "ANY"), function(x, i, j, drop = FALSE) x[][i, j, drop = drop])

#' @export
setMethod("t", "HugeMatrix", HM.t)

#' @export
setMethod("*", c("HugeMatrix", "ANY"), function(e1, e2) HM.lscal.op(e1, e2, getFunction("*")))

#' @export
setMethod("*", c("ANY", "HugeMatrix"), function(e1, e2) HM.lscal.op(e2, e1, getFunction("*")))

#' @export
setMethod("/", c("HugeMatrix", "ANY"), function(e1, e2) HM.lscal.op(e2, e1, getFunction("/")))

#' @export
setMethod("+", c("HugeMatrix", "HugeMatrix"), HM.mat.sum)

#' @export
setMethod("+", c("HugeMatrix", "ANY"), function(e1, e2) e1[] + e2)

#' @export
setMethod("+", c("ANY", "HugeMatrix"), function(e1, e2) e1 + e2[])

#' @export
setMethod("-", c("HugeMatrix", "missing"), HM.unary.minus)

#' @export
setMethod("-", c("HugeMatrix", "HugeMatrix"), function(e1, e2) e1 + (-e2))

#' @export
setMethod("-", c("ANY", "HugeMatrix"), function(e1, e2) e1 - e2[])

#' @export
setMethod("-", c("HugeMatrix", "ANY"), function(e1, e2) e1[] - e2)

#' @export
setMethod("%*%", c("HugeMatrix", "HugeMatrix"), HM.mat.mult)

#' @export
setMethod("%*%", c("ANY", "HugeMatrix"), HM.mat.mult)

#' @export
setMethod("kronecker", c(X = "HugeMatrix", Y = "ANY"), HM.kronecker)

#' @export
setMethod("kronecker", c(X = "ANY", Y = "HugeMatrix"), HM.kronecker)

#' @include util_matrix.R
#' @export
setMethod("row.bind", c("ANY", "HugeMatrix"), HM.row.bind)

#' @export
setMethod("col.bind", c("HugeMatrix", "ANY"), HM.col.bind)

#' Converting to a Matrix
#' @export
setGeneric("as.Matrix", function(M) standardGeneric("as.Matrix"))

#' @export
setMethod("as.Matrix", "HugeMatrix", HM.as.Matrix)

#' @export
setMethod("total.vol", c(S = "HugeMatrix"), function(S) sum(diag(S[])))

#' @export
setMethod("diag", c(x = "HugeMatrix", nrow = "ANY", ncol = "ANY"), function(x) diag(x[]))

#' @export 
setMethod("mat.to.data.table", "HugeMatrix", function(M) data.table::copy(M@data))

#' @export
setMethod("norm", c("HugeMatrix", "ANY"), function(x, type, ...) norm(x[], type, ...))

gen.kronecker <- function(X, Y)
{
    if(any(is.na(dim(X) * dim(Y))))
    {
        X <- HugeMatrix(X)
        Y <- HugeMatrix(Y)
    }
    kronecker(X, Y)
}

#' Kronecker product
#' @export
setGeneric("%x%", gen.kronecker)
                          
#' @title Abstract Matrix
#' @description A generic class for matrices. Useful to create object that have slots that are matrices
#' @name genMatrix
#' @import Matrix
#' @exportClass genMatrix
setClassUnion("genMatrix", c("matrix", "HugeMatrix", "Matrix"))

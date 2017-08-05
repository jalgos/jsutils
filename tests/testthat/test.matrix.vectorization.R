library(hugesparse)
library(data.table)
library(Matrix)
context("Matrix vectorization")

M1 <- random.matrix(20, 50)
M2 <- random.sym.matrix(20)
vdim <- c(4, 10, 3)
bmsl <- lapply(vdim, random.sym.matrix)
bml <- lapply(vdim, random.matrix)
BMS <- bdiag(bmsl)
BM <-  bdiag(bml)

test_that("bdiag.to.vech works",
{
    BMSv <- unlist(lapply(bmsl, function(x) as.vector(vech(x))))
    BDVH <- bdiag.to.vech(vdim)
    expect_equal_matrices(vech.reverse(BDVH %*% BMSv), BMS)
})

test_that("bdiag.to.vec works",
{
    BMv <- unlist(lapply(bml, as.vector))
    BDV <- bdiag.to.vec(vdim)
    expect_equal_matrices(vec.reverse(BDV %*% BMv), BM)
})

test_that("vec.to.vech works",
{
    expect_equal_matrices(M2,
                          vech.reverse(vec.to.vech(20L) %*% as.vector(M2)))
})

test_that("vech.to.vec works",
{
    expect_equal_matrices(M2,
                          vec.reverse(vech.to.vec(20L) %*% vech(M2)))
})

test_that("commutation.matrix works",
{
    expect_equal_matrices(t(M1),
                          vec.reverse(commutation.matrix(20, 50) %*% as.vector(M1), 50, p = 20),
                          compare.dimnames = FALSE)
})

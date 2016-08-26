library(Matrix)
context("Matrix operations")

N <- 200
n <- 20
p <- 10
q <- 30

npl <- 50L
npr <- 20L
nrpl <- 100L
ncpr <- 30L

M1 <- random.sparse.matrix(n, p, N)
M2 <- random.sparse.matrix(p, q, N)

Pl <- Matrix::sparseMatrix(i = sample(1:nrpl, npl), j = sample(1:(n * p), npl), x = rnorm(npl), dims = c(nrpl, n * p))
Pr <- Matrix::sparseMatrix(j = sample(1:ncpr, npr), i = sample(1:(p * q), npr), x = rnorm(npr), dims = c(p * q, ncpr))

triplet.prod <- function(M1,
                         M2,
                         trfun)
{
    DM1 <- mat.to.data.table(M1)
    DM2 <- mat.to.data.table(M2)
    
    dsparseMatrix(trfun(DM1[, i],
                        DM1[, j],
                        DM1[, x],
                        DM2[, i],
                        DM2[, j],
                        DM2[, x]),
                  dims = c(nrow(M1), ncol(M2)))
}

test_that("triplet Red / Black works",
{
    expect_equal_matrices(M1 %*% M2,
                          triplet.prod(M1, M2, triplet.prod.rb))
})

test_that("triplet hashmap works",
{
    expect_equal_matrices(M1 %*% M2,
                          triplet.prod(M1, M2, triplet.prod.hash))
})

test_that("Partial kronecker works",
{
    expect_equal_matrices(Pl %*% (M1 %x% M2) %*% Pr,
                          kronecker.proj(M1, M2, Pl, Pr))
})

test_that("Partial kronecker with only right projection",
{
    expect_equal_matrices((M1 %x% M2) %*% Pr,
                          kronecker.proj(M1, M2, Pr = Pr))
})

test_that("Partial kronecker with only left projection",
{
    expect_equal_matrices(Pl %*% (M1 %x% M2),
                          kronecker.proj(M1,
                                         M2,
                                         Pl = Pl))
})

test_that("Partial kronecker with no projection",
{
    expect_equal_matrices(M1 %x% M2,
                          partial.kronecker(M1,
                                            M2))
})

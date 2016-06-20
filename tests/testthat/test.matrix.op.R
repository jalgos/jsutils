context("Matrix operations")

N <- 800
n <- 200
p <- 100

npl <- 50L
npr <- 20L
nrpl <- 100L
ncpr <- 30L

M1 <- random.sparse.matrix(n, p, N)
M2 <- random.sparse.matrix(n, p, N)

Pl <- Matrix::sparseMatrix(i = sample(1:nrpl, npl), j = sample(1:(n ^ 2), npl), x = rnorm(npl), dims = c(nrpl, n ^ 2))
Pr <- Matrix::sparseMatrix(j = sample(1:ncpr, npr), i = sample(1:(p ^ 2), npr), x = rnorm(npr), dims = c(p ^ 2, ncpr))

Pl <- Matrix::Diagonal(n ^ 2)
Pr <- Matrix::Diagonal(p ^ 2)
test_that("Partial kronecker works",
{
    expect_equal(Pl %*% (M1 %x% M2) %*% Pr,
                 partial_kronecker(mat.to.data.table(M1),
                                   mat.to.data.table(M2),
                                   mat.to.data.table(Pl),
                                   mat.to.data.table(Pr),
                                   dim(M2)),
                 check.attributes = FALSE)
})

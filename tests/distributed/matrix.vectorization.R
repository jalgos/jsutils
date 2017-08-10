
gen.test <- function(...,
                     ftest,
                     type.test = substitute(ftest))
{
    jlog.info(ltester, "Testing distributed", type.test)
    mat.vectorization.distributed(TRUE)
    BV <- ftest(...,
                logger = ltester)
    mat.vectorization.distributed(FALSE)
    lBV <- ftest(...,
                 logger = ltester)
    jlog.debug(ltester, "Class of BV:", class(BV), "class of lBV:", class(lBV))
    expect_true(is(BV, "DHugeMatrix"))
    expect_true(!is(lBV, "DHugeMatrix"))
    jlwrite.debug(ltester, as.data.frame(as.data.table(mat.to.triplet(BV))))
    jlwrite.debug(ltester, mat.to.triplet(lBV))
    
    expect_equal_matrices(BV[], lBV)
    jlog.info(ltester, "Testing distributed", type.test, "passed")
}

bdiag.to.vech.test <- function()
{
    jlog.debug(ltester, "Testing distributed bdiag.to.vech")
    BV <- bdiag.to.vech(vdim)
    expect_equal(class(BV), "DHugeMatrix")
    expect_equal_matrices(BV[], bdiag.to.vech.local(vdim))
    
}

bdiag.to.vech.test <- function()
{
    jlog.debug(ltester, "Testing distributed bdiag.to.vech")
    BV <- bdiag.to.vech(vdim)
    expect_equal_matrices(BV[], bdiag.to.vech.local(vdim))
    
}

gen.test(ftest = bdiag.to.vech,
         vdim)

gen.test(ftest = bdiag.to.vec,
         vdim)
gen.test(ftest = function(..., logger) vech.to.vec(...),
         type.test = "vech.to.vec",
         n = 100L)

gen.test(ftest = function(..., logger) vec.to.vech(...),
         type.test = "vec.to.vech",
         n = 100L)

gen.test(ftest = function(..., logger) commutation.matrix(...),
         type.test = "commutation.matrix",
         n = 100L,
         p = 33L)

## Edge cases:
gen.test(ftest = function(..., logger) vech.to.vec(...),
         type.test = "vech.to.vec n = 1L",
         n = 1L)

gen.test(ftest = function(..., logger) vec.to.vech(...),
         type.test = "vec.to.vech n = 1L",
         n = 1L)

gen.test(ftest = function(..., logger) commutation.matrix(...),
         type.test = "commutation.matrix n = p = 1L",
         n = 1L,
         p = 1L)

gen.test(ftest = function(..., logger) vech.to.vec(...),
         type.test = "vech.to.vec n = 3L",
         n = 3L)

gen.test(ftest = function(..., logger) vec.to.vech(...),
         type.test = "vec.to.vech n = 2L",
         n = 2L)

gen.test(ftest = function(..., logger) commutation.matrix(...),
         type.test = "commutation.matrix n = 2L p = 4L",
         n = 2L,
         p = 4L)


jlog.info(ltester, "******************************************")
jlog.info(ltester, "Distributed matrix vectorization tests passed!")

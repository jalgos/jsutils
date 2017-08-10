.libPaths("lib")
library(pbdDMAT, quiet = TRUE)
init.grid()
library(testthat, quiet = TRUE)
library(data.table, quiet = TRUE)
library(jsutils, quiet = TRUE)
library(jlogger, quiet = TRUE)
library(jsparallel, quiet = TRUE)
library(hugesparse, quiet = TRUE)
library(ddata.table, quiet = TRUE)
library(distributedhugesparse, quiet = TRUE)

ltester <- JPLoggerFactory("distributed matrix vectorization tests")
add.logfiles(ltester, sprintf("logs/dmat.vec.tests.%i.%i.log", comm.rank(), comm.size()))
add.logfiles(dhs.logger(), sprintf("logs/dmat.vec.tests.%i.%i.log", comm.rank(), comm.size()))
add.logfiles(get.ddt.logger(), sprintf("logs/dmat.vec.tests.%i.%i.log", comm.rank(), comm.size()))
jlflush(ltester)
if(!interactive()) { 
    options(error = function(...)
    {
        jsutils::print.call.stack(..., logger = ltester)
        jlog.error(ltester, "An error occurred. Aborting...")
        barrier()
        q('no')
    })
} else {
    options(error = recover)
}

mat.vectorization.distributed(TRUE)
vdim <- c(10L, 2L, 3L, 5L, 20L)
options(warn = 2)
source('tests/distributed/matrix.vectorization.R')
source('tests/distributed/trim.cov.matrix.R')

if(!interactive())
    finalize()

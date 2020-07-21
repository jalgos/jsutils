.libPaths("lib")

options(repos = structure(c(CRAN = "https://cloud.r-project.org")))

`.` <- list
jspackages <- .("utils" = .(c('jconfig', version = "1.0.5"),
                            c('jlogger', version = "1.0.6")))

jsroot::dependencies(jspackages = jspackages,
                     cran.packages = c('RJSONIO', 'data.table', 'Matrix', 'track'),
                     quiet = FALSE)

install.packages(".", repos = NULL)
jspackages <- .("utils" = .(c('hugesparse'),
                            c('jsmath', version = "1.0.2")))
jsroot::dependencies(jspackages = jspackages,
                      quiet = FALSE)

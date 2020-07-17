.libPaths("lib")

options(repos = structure(c(CRAN = "https://cloud.r-project.org")))

`.` <- list
jspackages <- .("utils" = .(c('jconfig', version = "1.0.5"),
                            c('jlogger', version = "1.0.6")))

jsroot::dependencies(jspackages = jspackages,
                     cran.packages = c('RJSONIO', 'data.table', 'Matrix', 'MASS', 'track'),
                     quiet = FALSE)
.libPaths("lib")

`.` <- list
jspackages <- .("utils" = .(c('jconfig', version = "1.0.5")))

jsroot::dependencies(jspackages = jspackages,
                     cran.packages = .('RJSONIO'),
                     quiet = FALSE)


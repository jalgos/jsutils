.libPaths("lib")

devtools::document()
devtools::build(binary = TRUE)

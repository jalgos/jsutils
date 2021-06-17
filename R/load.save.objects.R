#' @name save_load
#' @title Saving Loading Objects
#' @param OBJ object to save
#' @param file full path of the saving destination
#' @param ... To be passed to the saving / reading method
#' @param logger JLogger
NULL

#' @describeIn save_load Uses `saveRDS` or `qs::qsave` based on the extension of file
#' @export save.object
save.object <- function(OBJ,
                        file,
                        ...,
                        logger = NULL)
{
    if(grepl("\\.rds", file, ignore.case = TRUE))
    {
        jlog.debug(logger, "Saving object as an rds file in:", file)
        saveRDS(OBJ,
                file,
                ...)
    }
    else if(grepl("\\.qs", file, ignore.case = TRUE))
    {
        jlog.debug(logger, "Saving object as a qs file in:", file)
        qs::qsave(OBJ,
                  file,
                  ...)
                
    }
    else
    {
        jlog.error(logger, "Extension not supported for file:", file)
        stop("Extension not supported")
    }
}

#' @describeIn save_load Uses `readRDS` or `qs::qread` based on the extension of file
#' @export read.object
read.object <- function(file,
                        ...,
                        logger = NULL)
{
    if(grepl("\\.rds", file, ignore.case = TRUE))
    {
        jlog.debug(logger, "Reading file as an rds file from:", file)
        readRDS(file,
                ...)
    }
    else if(grepl("\\.qs", file, ignore.case = TRUE))
    {
        jlog.debug(logger, "Reading file as a qs file from:", file)
        qs::qread(file,
                  ...)
                
    }
    else
    {
        jlog.error(logger, "Extension not supported for file:", file)
        stop("Extension not supported")
    }
}

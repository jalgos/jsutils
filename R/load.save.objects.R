#' @name save_load
#' @title Saving Loading Objects
#' @param object object to save
#' @param file full path of the saving destination
#' @param ... To be passed to the saving / reading method
#' @param logger JLogger
NULL

#' @describeIn save_load Uses `saveRDS`, `qs::qsave`, `arrow::write_parquet`, ... based on the extension of file
#' @export save.object
save.object <- function(object,
                        file,
                        ...,
                        logger = NULL)
{
    if(grepl("\\.rds", file, ignore.case = TRUE))
    {
        jlog.debug(logger, "Saving object as an rds file in:", file)
        saveRDS(object,
                file,
                ...)
    }
    else if(grepl("\\.qs", file, ignore.case = TRUE))
    {
        jlog.debug(logger, "Saving object as a qs file in:", file)
        qs::qsave(object,
                  file,
                  ...)
                
    }
    else if(grepl("\\.parquet", file, ignore.case = TRUE))
    {
        jlog.debug(logger, "Saving object as a parquet file in:", file)
        arrow::write_parquet(object,
                             file,
                             ...)
    }
    else
    {
        jlog.error(logger, "Extension not supported for file:", file)
        stop("Extension not supported")
    }
}

#' @describeIn save_load Uses `readRDS`, `qs::qread`, `arrow::read_parquet`, ... based on the extension of file
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
    else if(grepl("\\.parquet", file, ignore.case = TRUE))
    {
        jlog.debug(logger, "Reading object as a parquet file in:", file)
        arrow::read_parquet(file,
                            ...)
    }
    else
    {
        jlog.error(logger, "Extension not supported for file:", file)
        stop("Extension not supported")
    }
}

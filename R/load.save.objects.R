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

    ext <- tools::file_ext(file)
    jlog.debug(logger, "Saving file as an", ext, "file")

    save.fun <- switch(ext,
                       'rds' = saveRDS,
                       'qs' = qs::qsave,
                       'parquet' = arrow::write_parquet,
                       'csv' = data.table::fwrite,
                       'json' = RJSONIO::toJSON)
    if(is.null(save.fun))
        stop("Extension", ext, "not supported")
    
    is.s3.path <-  grepl("^s3\\/", file)
    if(is.s3.path)
    {
        jlog.debug(logger, "Saving file to s3")
        s3.file <- file
        file <- tempfile(fileext = ext)
    }

    save.fun(object,
             file)

    if(is.s3.path)
    {
        copy.mc.cmd(src = file, dest = s3.file, ...)
        unlink(file)
    }
}

#' @describeIn save_load Uses `readRDS`, `qs::qread`, `arrow::read_parquet`, ... based on the extension of file
#' @export read.object
read.object <- function(file,
                        ...,
                        logger = NULL)
{
    ext <- tools::file_ext(file)
    jlog.debug(logger, "Reading file as an", ext, "file")

    read.fun <- switch(ext,
                       'rds' = readRDS,
                       'qs' = qs::read,
                       'parquet' = arrow::read_parquet,
                       'csv' = data.table::fread,
                       'json' = RJSONIO::fromJSON)
    if(is.null(read.fun))
        stop("Extension", ext, "not supported")
    
    is.s3.path <-  grepl("^s3\\/", file)
    if(is.s3.path)
    {
        jlog.debug(logger, "Loading file from s3")
        tmp.file <- tempfile(fileext = ext)
        copy.mc.cmd(src = file, dest = tmp.file, ...)
        file <- tmp.file
    }
   
    obj <- read.fun(file, ...)

    if(is.s3.path)
        unlink(file)

    obj
}


#' @name copy.mc.cmd 
#' @title Copy file from/to s3 storage
#' @param src full path of the source file
#' @param dest full path of the saving destination
#' @param retrieved.output if mc output should be retrieved to R
#' @param recursive should be TRUE of src is a directory
#' @param quiet set to TRUE for removing logs
#' @param ... 
#' @param logger JLogger
NULL

#' @describeIn copy.mc.cmd copy file or directory from s3 storage to local storage or the opposite
#' @export copy.mc.cmd
copy.mc.cmd <- function(src,
                        dest,
                        ...,
                        retrieved.output = FALSE,
                        recursive = FALSE,
                        quiet = FALSE,
                        logger = ukpik.logger())
{
    cp.cmd <- paste(c('mc cp',
                      if(recursive) '--recursive',
                      src,
                      dest),
                    collapse = ' ')
    jlog.debug(logger, "Running", cp.cmd %c% BW)
    system(cp.cmd,
           ignore.stdout = quiet,
           ignore.stderr = quiet,
           intern = retrieved.output)
}

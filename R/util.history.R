track.env <- new.env()

create.rhistory.directory <- function(folder = "Rhistory")
{
    V <- scan(pipe("ls"), what = "character")
    if(!folder %in% V) system(paste("mkdir", folder))
    date <- gsub("-", "", Sys.Date())
    subfolder <- paste(folder, date, sep = "/")
    V <- scan(pipe(paste("ls", folder)), what = "character")
    if(!date %in% V) system(paste("mkdir", subfolder))
    return(subfolder)
}

create.rhistory.file <- function(info = "", subfolder = "Rhistory")
{
    filename <- paste(c(info, "Rhistory", gsub(" |-|:", "", Sys.time())), collapse=".")
    filename <- paste(c(subfolder, filename), collapse = "/")
    return(filename)
}

#' @name track.rhistory
#' @title Saving Rhistory
#' @details Keeping track of your work is very important. These tools allow you to save your entire RHistory so you can comme back to earlier work easily.\cr
#' The files is saved into the specified directory under the subdirectory current date. The current timestamp is added to the info file so saving your work several time does not overwrite earlier saves. It works like save.rhistory, but here we use the track library.\cr
#' @import track
#' @param info A name to identify the work you did in the session
#' @param folder Folder into which saving the Rhistory files
NULL

#' @export
track.rhistory <- function(info = "", folder = "Rhistory")
{
    if(!is.null(track.env$trackfile))
    {
        return()
    }
    
    subfolder <- create.rhistory.directory(folder)
    filename <- create.rhistory.file(info, subfolder)

    track::track.history.start(filename)
    track.env$trackfile <- filename
}

#' @name rhistory
#' @title Saving Rhistory
#' @details Keeping track of your work is very important. These tools allow you to save your entire RHistory so you can comme back to earlier work easily.\cr
#' The files is saved into the specified directory under the subdirectory current date. The current timestamp is added to the info file so saving your work several time does not overwrite earlier saves. \cr
#' @param info A name to identify the work you did in the session
#' @param folder Folder into which saving the Rhistory files
NULL

#' @export
save.rhistory <- function(info = "", folder = "Rhistory")
{
    subfolder <- create.rhistory.directory(folder)
    filename <- create.rhistory.file(info, subfolder)
    savehistory(file = filename)
    print(c("saved rhistory: ", filename))
}


#' @describeIn rhistory Saves the RHistory before exiting
#' @export
quit.save <- function(...)
{
    save.rhistory(...)
    q()
}

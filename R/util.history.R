track.env <- new.env()

create.rhistory.directory <- function(folder = "Rhistory")
{
    ls1 <- pipe("ls")
    V <- scan(ls1, what = "character")
    close(ls1)
    if(!folder %in% V) system(paste("mkdir", folder))
    date <- gsub("-", "", Sys.Date())
    subfolder <- paste(folder, date, sep = "/")
    ls2 <- pipe(paste("ls", folder))
    V <- scan(ls2, what = "character")
    close(ls2)
    if(!date %in% V) system(paste("mkdir", subfolder))
    return(subfolder)
}

create.rhistory.file <- function(info, subfolder = "Rhistory")
{    
    if(missing(info))
    {
        cat("Prefix of the file to track (default empty)?\n")
        info <- get.keyboard.input()
    }
    filename <- paste(c(info, "Rhistory", gsub(" |-|:", "", Sys.time())), collapse = ".")
    filename <- paste(c(subfolder, filename), collapse = "/")
    return(filename)
}

#' rhistory
#' 
#' Keeping track of your work is very important. These tools allow you to save your entire RHistory so you can comme back to earlier work easily.\cr
#' The files is saved into the specified directory under the subdirectory current date. The current timestamp is added to the info file so saving your work several time does not overwrite earlier saves.\cr
#' @name rhistory
#' @param info A name to identify the work you did in the session
#' @param folder Folder into which saving the Rhistory files
#' @import track
NULL

#' @describeIn rhistory Use savehistory() to store the history in a file.
#' @export
save.rhistory <- function(info, folder = "Rhistory")
{
    subfolder <- create.rhistory.directory(folder)
    filename <- create.rhistory.file(info, subfolder)
    savehistory(file = filename)
    cat("saved rhistory: ", filename, '\n')
}


#' @describeIn rhistory Saves the RHistory before exiting
#' @export
quit.save <- function(...)
{
    save.rhistory(...)
    q()
}

#' @describeIn rhistory The function track.rhistory works like save.rhistory, but here we use the track library with track.history.start() to take care of the recording of the history. If it was already used in the current session, track.history.start() won't be called again.
#' @export
track.rhistory <- function(info,
                           folder = "Rhistory")
{
    if(!interactive())
        return()
    
    if(!is.null(track.env$trackfile))
    {
        return()
    }

    cat("Do you want to track this session?\n")
    if(!y.to.continue('(^yes$)|(y)', ignore.case = TRUE))
    {
        track.env$trackfile <- FALSE
        return()
    }
    subfolder <- create.rhistory.directory(folder)
    filename <- create.rhistory.file(info, subfolder)
    track::track.history.start(filename)
    cat('tracking history in file:', filename, '\n')
    track.env$trackfile <- filename
}


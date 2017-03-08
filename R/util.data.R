
get.util.data.logger <- function()
{
    JLoggerFactory('Data loading')
}

#' Split Write csv
#'
#' Allows to export a big data.table as several csv files
#' @param TAB Data.table, usually big, to be splitted and exported as csv files
#' @param split.size parameter controlling the max size of splits, in number of lines
#' @param export.folder parameter for target path to repo to save the csv files
#' @export
split.write.csv <- function(TAB,
                            split.size,
                            export.folder = "output",
                            logger = get.util.data.logger())
{
    size <- dim(TAB)[1]
    nb.files <- ceiling(size / split.size)
    splits <- (1:nb.files) * split.size
    lapply(1:nb.files,
           FUN = function(x)
    {
        jlog.info(logger, "Writing the", x, "split to", export.folder)
        y <- splits[x]
        write.csv(TAB[(y - split.size + 1):y], file = paste0(export.folder, "/", x, ".csv"))
    })
}

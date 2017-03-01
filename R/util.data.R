#' Split Write csv
#'
#' Generate an arbitrary number of random strings made of letters from the 26 characters alphabet.
#' @param TAB Data.table, usually big, to be splitted and exported as csv files
#' @param split.size parameter controlling the max size of splits, in number of lines
#' @export
split.write.csv <- function(TAB,
                            split.size,
                            logger = get.grt.logger())
{
    size <- dim(TAB)[1]
    nb.files <- ceiling(size / split.size)
    splits <- (1:nb.files) * split.size
    lapply(1:nb.files,
           FUN = function(x)
    {
        jlog.info(logger, "Writing the", x, "split")
        y <- splits[x]
        write.csv(TAB[(y - split.size + 1):y], file = paste0("reports/split", x, ".csv"))
    })
}

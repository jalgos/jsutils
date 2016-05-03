sweave.logger <- function() JLoggerFactory("Sweave")

sweave.and.tex <- function(filename,
                           logger)
{
    jlog.info(logger, "Removing *.{toc,tex,pdf,log,aux}")
    system("rm *.{toc,tex,pdf,log,aux}")
    jlog.info(logger, "Sweaving...")
    Sweave(paste(filename, "Rnw", sep = "."))
    jlog.info(logger, "texifying...")
    safe.system(paste("texi2pdf", paste(filename,"tex", sep = ".")))
}

execute.or.jump.back <- function(expr,
                                 ...)
{
    tryCatch(expr, ..., error = function(e){setwd(".."); stop(e)})
}

clean.sweave <- function(filename,
                         report.directory = "report",
                         theme.fam = theme.gray,
                         base.size = 100,
                         point.size = 10,
                         logger = sweave.logger())
{
    TH <- theme.get()
    theme.set(theme.fam(base.size = base.size))
    oldcfg <- CFG$graph.conf[["point.size"]]
    CFG$graph.conf[["point.size"]] <<- point.size
    jlog.info(logger, "Switching to directory:", report.directory)
    setwd(report.directory)
    execute.or.jump.back(sweave.and.tex(filename, logger = logger))
    pdf <- paste(filename, "pdf", sep = ".")
    system(paste("evince", pdf), wait = FALSE)
    setwd("..")
    if(!is.null(oldcfg)) CFG$graph.conf[["point.size"]] <<- oldcfg
    else CFG$graph.conf[["point.size"]] <<- 1.
    theme.set(TH)
    jlog.info(logger, "Done creating the report", pdf)
}

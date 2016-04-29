sweave.logger <- function() JLoggerFactory("Sweave")

sweave_and_tex <- function(filename,
                           logger)
{
    jlog.info(logger, "Removing *.{toc,tex,pdf,log,aux}")
    system("rm *.{toc,tex,pdf,log,aux}")
    jlog.info(logger, "Sweaving...")
    Sweave(paste(filename, "Rnw", sep="."))
    jlog.info(logger, "texifying...")
    safe_system(paste("texi2pdf", paste(filename,"tex",sep=".")))    
}

execute_or_jump_back <- function(expr,
                                 ...)
{
    tryCatch(expr, ..., error = function(e){setwd(".."); stop(e)})
}

clean_sweave <- function(filename,
                         report_directory = "report",
                         theme_fam = theme_gray,
                         base_size = 100,
                         point_size = 10,
                         logger = sweave.logger())
{
    TH = theme_get()
    theme_set(theme_fam(base_size = base_size))
    oldcfg = CFG$graph_conf[["point_size"]]
    CFG$graph_conf[["point_size"]] <<- point_size
    jlog.info(logger, "Switching to directory:", report_directory)
    setwd(report_directory)
    execute_or_jump_back(sweave_and_tex(filename, logger = logger))
    pdf = paste(filename, "pdf", sep = ".")
    system(paste("evince", pdf), wait = FALSE)
    setwd("..")
    if(!is.null(oldcfg)) CFG$graph_conf[["point_size"]] <<- oldcfg
    else CFG$graph_conf[["point_size"]] = 1.
    theme_set(TH)
    jlog.info(logger, "Done creating the report", pdf)
}

guess.user <- function()
{
    paste(Sys.info()[c("effective_user", "nodename")], collapse = "@")
}

critical.mail <- function(from = guess.user(),
                          to,
                          subject,
                          msg,
                          ...,
                          logger = NULL)
{
    if(!require(sendmailR)) jlog.warn(logger, "sendmailR was not install on this machine, no mail will be sent")
    status <- sendmail(from = from, to = to, subject = subject, msg = msg)
    jlog.debug(logger, "User:", to, "was warned of the issue")
    status
}

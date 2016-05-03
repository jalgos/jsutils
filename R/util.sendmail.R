#' Guess user
#'
#' Guess a username@domain string for the current user
#' @export
guess.user <- function()
{
    paste(Sys.info()[c("effective_user", "nodename")], collapse = "@")
}


#' Critical mail
#'
#' Tool to send email from R
#' @param from Email from
#' @param to Recipient
#' @param subject Subject of the email
#' @param msg Message
#' @param logger JLogger to be used to log messages
#' @export
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

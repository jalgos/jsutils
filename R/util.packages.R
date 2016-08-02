#' Install Git
#'
#' Installing Jalgos packages with git without using git2r package.
#' @param name Name of the package
#' @param group Gitlab group to which the package belongs
#' @param url Remote url of the package
#' @param install.fun Function to use to install the packages
#' @export
install.git <- function(name,
                        group,
                        url = sprintf('git@www.datasaiyan.com:%s/%s.git', group, name),
                        install.fun = function(ploc, ...) install.packages(ploc, repos = NULL, ...),
                        ...)
{
    puid <- uuid::UUIDgenerate()
    package.loc <- sprintf("/tmp/RPackage.%s", puid)
    system(sprintf("git clone %s %s",  url, package.loc))
    tryCatch(install.fun(package.loc, ...),
             error = function(cond) {
        system(sprintf("rm -Rf %s", package.loc))            
        stop(sprintf("An error occured while installing package %s, message: %s. Cleaning up before leaving.", name, cond$message))
    })
    system(sprintf("rm -Rf %s", package.loc))
}

#' Install From Jalgos Repos
#'
#' Wrapper around install_git to install packages from Jalgos repos (uses git2r)
#' @export
install.jalgos <- function(url,
                           lib.loc = "lib",
                           ...)
{
    withr::with_libpaths(new = lib.loc, devtools::install_git(url))
}

#' @title Install Missing Package
#' @name require.or.install
NULL

#' @describeIn require.or.install Install specified package in case call to \code{require} fails.
#' @param name Name of the package
#' @param group Group to which the package belongs
#' @param url Url of the package
#' @param url.pattern Url pattern
#' @param install.fun Package installation function to use
#' @param vars Variables that appear in the url.pattern
#' @param ... to be forwarded to install.fun
#' @param force force install
#' @export
require.or.install <- function(name,
                               group,
                               url = replace.variable(url.pattern, subs = vars),
                               url.pattern = "git@www.datasaiyan.com:$group/$name.git",
                               install.fun = install.git,
                               vars = c("$group" = group, "$name" = name),
                               ...,
                               force = FALSE)
{
    if(force || !require(name, character = TRUE)) install.fun(url, name = name, ..., repos = NULL)
    else return()
    
    require(name, character = TRUE)
}

file.package.pattern <- "/home/sebastien/Dev/util/jalgos-packages/%s"

#' @describeIn require.or.install For development use. Installs package directly from directory not from remote repo
#' @export
require.or.install.dev <- function(name,
                                   url.pattern = file.package.pattern,
                                   ...)
{
    require.or.install(name,
                       url = sprintf(file.package.pattern, name),
                       ...,
                       install.fun = devtools::install)
}

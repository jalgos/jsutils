#' @name text.to.data
#' @title Text to data
#' @description Various function to extract relevant data from text using regex.
#' @details There are a few native function to manipulate text with regexed \code{grep}, \code{gsub} etc... But no function to extract the string matched from the text.\cr
#' We provide here some function to do so
#' @param pattern Pattern to match as in \code{grep}
#' @param text Text on which match will be done as in \code{grep}
#' @param replacement Replacement pattern as in \code{gsub}
#' @param ... Parameters forwarded to native functions
#' @param npats Number of patterns to capture (patterns surrounded by '()'). Automatically detected
#' @param sep Matches are concatenated together before being split. The separator used can be set. It needs to be a pattern that does not naturally appear in the strings matched. Defaults to '#'
#' @param strip.newlines Should multilines string be considered as one line strings? This is important beause the way multiline strings are matched is different than single line ones. By default newlines are stripped from the text.
#' @param match.names Each match can be named in which case a data.table which columns are the matches and column names this parameter will be returned. A list is returned otherwise
#' @param grep.fun Which regex function should be used in mgrep
NULL

#' describeIn text.to.data Replaces the entirety of a string with the replacement pattern. Useful when we are more concerned with discarding everything that doesn't match than the contrary.
#' @export
replace.whole.string <- function(pattern,
                                 replacement,
                                 text,
                                 ...)
{
    pattern <- paste(c(".*?", pattern, ".*"), collapse = "")
    gsub(pattern = pattern , replacement = replacement, x = text, ...)
}

#' @describeIn text.to.data Returns either a 2 elements list describing the matches. The first element is a vector giving the indices that match. The second element is an array with as many columns as they are patterns to capture (ie '(...)') and which elements are the strings captured.
#' @export
grep.matches <- function(pattern,
                         text,
                         npats = stringr::str_count(pattern, "\\(") - stringr::str_count(pattern, "\\\\\\("),
                         sep = "#",
                         ...)
{
    rpat <- paste(paste0("\\", 1:npats), collapse = sep)
    F <- grep(pattern = pattern, x = text,...)
    
    if(length(F) == 0)
    {
        if(npats > 1) return(list(match.indices = F, match.values = matrix("", 0, npats)))
        else return(list(match.indices = F, match.values = character(0)))
    }
    
    if(npats > 0)
    {
        S <- replace.whole.string(pattern = pattern , replacement = rpat, text = text[F],...)
        M <- simplify2array((strsplit(x = paste0(S, sep), split = sep)))
    }
    else M <- character(0)

    if(npats > 1) M <- t(M)
    
    list(match.indices = F, match.values = M)
}

#' @describeIn text.to.data Returns the patterns captured or an empty string for strings that didn't match. There is an option for setting the names of each pattern captured.
#' @export
to.matches.or.null <- function(text,
                               ...,
                               strip.newlines = TRUE,
                               match.names)
{
    if(strip.newlines) text <- gsub("\\n", " ", text)
    G <- grep.matches(text = text, ...)
    
    if(is.matrix(G$match.values))
    {
        res <- matrix(character(length(text) * ncol(G$match.values)), length(text))
        res[G$match.indices, ] <- G$match.values
    }
    else
    {
        res <- character(length(text))
        res[G$match.indices] <- G$match.values
    }
    
    if(!missing(match.names))
    {
        res <- as.data.table(res)
        setnames(res, match.names)
    }
    
    res
}


#' @describeIn text.to.data Matches a set of pattern
#' @export
mgrep <- function(patterns, ..., grep.fun = grep)
{
    pattern <- paste(patterns, collapse = "|")
    grep.fun(pattern, ...)
}

#' @describeIn text.to.data Matches the entire string. Will return FALSE if only part of the string matches.
#' @export
grep.whole <- function(pattern, ..., grep.fun = grep)
{
    pattern <- paste(c("^", pattern, "$"), collapse = "")
    grep.fun(pattern, ...)
}

## names are regex and function returns the first occurence which name matches

#' Retrieving an element of a list by matching the names
#' @param L Named list. The names are regexes that attempt to match \code{name}.
#' @param name string to be matched
#' @return The first occurence of L for which its name matches the parameter \code{name}
match.grep <- function(L,
                       name)
{
    for(nm in names(L))
    {
        if(grepl(nm, name)) return(L[[nm]])
    }
    stop("no match found")
}

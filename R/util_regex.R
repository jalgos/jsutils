library(stringr)

replace_whole_string <- function(pattern, replacement, text, ...)
{
    pattern = paste(c(".*?", pattern, ".*"), collapse = "")
    gsub(pattern = pattern , replacement = replacement, x = text, ...)
}

grep_matches <- function(pattern,
                         text,
                         npats = str_count(pattern, "\\(") - str_count(pattern, "\\\\\\("),
                         sep = "#",
                         ...)
{
    rpat = paste(paste0("\\", 1:npats), collapse = sep)
    F = grep(pattern = pattern, x = text,...)
    
    if(length(F) == 0)
    {
        if(npats > 1) return(list(match_indices = F, match_values = matrix("", 0, npats)))
        else return(list(match_indices = F, match_values = character(0)))
    }
    
    if(npats > 0)
    {
        S = replace_whole_string(pattern = pattern , replacement = rpat, text = text[F],...)
        M = simplify2array((strsplit(x = paste0(S,sep), split = sep)))
    }
    else M = character(0)

    if(npats > 1) M = t(M)
    
    list(match_indices = F, match_values = M)
}

to_matches_or_null <- function(text,
                               ...,
                               strip.newlines = TRUE,
                               match_names)
{
    if(strip.newlines) text <- gsub("\\n", "", text)
    G = grep_matches(text = text, ...)
    
    if(is.matrix(G$match_values))
    {
        res = matrix(character(length(text) * ncol(G$match_values)), length(text))
        res[G$match_indices, ] = G$match_values
    }
    else
    {
        res = character(length(text))
        res[G$match_indices] = G$match_values
    }
    
    if(!missing(match_names))
    {
        res = as.data.table(res)
        setnames(res, match_names)
    }
    
    res
}

mgrep <- function(patterns, ..., grep_fun = grep)
{
    pattern = paste(patterns, collapse = "|")
    grep_fun(pattern, ...)
}

## greps string that entirely match the pattern
grep_whole <- function(pattern, ..., grep_fun = grep)
{
    pattern = paste(c("^", pattern, "$"), collapse = "")
    grep_fun(pattern, ...)
}

## names are regex and function returns the first occurence which name matches
match_grep <- function(L,
                       name)
{
    for(nm in names(L))
    {
        if(grepl(nm, name)) return(L[[nm]])
    }
    stop("no match found")
}

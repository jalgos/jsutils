safeGeneric("init", function(obj, ...) standardGeneric("init"))            

constructor_init <- function(.Class)
{
    function(...) init(new(.Class), ...)
}

setClassJ <- function(class,
                      ...)
{
    setClass(class, ...)
    constructor_init(class)
}

update_obj <- function(obj,
                       ...)
{
    new(class(obj),
        ...,
        obj)
}
                       

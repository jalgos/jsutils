## safe standard generic to avoid creating generics at each file sourcing
## With this function generics can be defined anywhere

#' Not overwriting generics
#'
#' Creates a generic only if f is not a generic yet. Useful when generics are created in different files and the user doesn't have control over the order in which it's loaded
#' @param f Function to be made generic
#' @param ... Extra parameters to be passed on to setGeneric
#' @export
safeGeneric <- function(f,
                        ...)
{
    if(isGeneric(f)) return()
    setGeneric(f, ...)
}

#' Init
#'
#' An elegant way to build objects that require a lot of logic in their construction
#' @export
setGeneric("init", function(obj, ...) standardGeneric("init"))

constructor.init <- function(.Class)
{
    function(...) init(new(.Class), ...)
}

#' Classes with init
#' 
#' Creates a class that relies on init
#' @export
setClassJ <- function(class,
                      ...)
{
    setClass(class, ...)
    constructor.init(class)
}

#' Updating object
#'
#' Creates a copy of the object which slots and parents can be updated via new.
#' @details S4 classes slots cannot be modified. Solution such as \code{obj@@slot <- new.value} are discouraged as the operation creates a copy of obj everytime.\cr
#' The proper way to update an S4 object is to use the \code{new} operator. The problem with new is that it requires the class name. Hardcoding it prevents sub classes to use a parent function properly.\cr
#' The solution is to give the class of the object to be updated as a parameter of new. To be DRY we provide this function.
#' @param obj Object to update
#' @param ... new fields of the object. Named arguments update slots and unnamed arguments update parent object.
#' @usage update.obj(obj, ...)
#' @export update.obj
update.obj <- function(obj,
                       ...)
{
    new(class(obj),
        ...,
        obj)
}
                       

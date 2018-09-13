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
#' @param class The class you want to set
#' @param where The environment you want to set it to
#' @param ... to be passed on to setClass
#' @export
setClassJ <- function(class,
                      where,
                      ...)
{
    if(missing(where))
        where <- sys.frame()
    setClass(class,
             where = where,
             ...)
    
    constructor.init(class)
}

#' Updating object
#'
#' Creates a copy of the object which slots and parents can be updated via new.
#' @details S4 classes slots cannot be modified. Solution such as \code{obj@slot <- new.value} are discouraged as the operation creates a copy of obj everytime.\cr
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

##################
### Traits
##################

#' @title Applying Sub Methods For Traits
#' @name traits.dispatch
NULL

#' @describeIn traits.dispatch Applies the sub methods for each sub traits of a given class
#' @param OBJ To be used in case each call to the submethods is supposed to transform the input. In this case OBJ must be set to the object on which the dispatch occurs
#' @param method The method that we want to dispatch
#' @param traits.traj The traits that constitute the given object
#' @param ... Parameters of the submethods
#' @param update.var Should we update the object after each sub call. Defaults to whether OBJ was provided
#' @param until.no.null Should the function return as soon as a result is not null?
#' @param prev.objs To complement the signature. Objects that appear before the concerned class
#' @param post.objs To complement the signature. Objects that appear after the concerned class 
#' @param tra.logger Logger that helps trace the dispatching
#' @usage traits.apply(OBJ = NULL,
#'                       method,
#'                       traits.traj,
#'                       ...,
#'                       update.var = !is.null(OBJ),
#'                       until.no.null = FALSE,
#'                       prev.objs = list(),
#'                       post.objs = list(),
#'                       tra.logger = JF.logger())
#' @export traits.apply
traits.apply <- function(OBJ = NULL,
                         method,
                         traits.traj,
                         ...,
                         update.var = !is.null(OBJ),
                         until.no.null = FALSE,
                         prev.objs = list(),
                         post.objs = list(),
                         tra.logger = NULL)
{
    prev.classes <- as.character(sapply(prev.objs, class))
    post.classes <- as.character(sapply(post.objs, class))
    
    for(class in traits.traj)
    {
        jlog.trace(tra.logger, "Applying method:", method, "to object of class:", class, "args:", nma)
        child.method <- selectMethod(method,
                                     signature = c(prev.classes,
                                                   class,
                                                   post.classes))
        ANS <- child.method(OBJ,
                            ...)
        
        if(until.no.null && !is.null(ANS))
            return(ANS)
        
        if(update.var)
            OBJ <- ANS
    }
    ANS
}

#' @describeIn traits.dispatch Dispatches a method when both objects are a composition of several traits and the sub methods need to be called on each combination of traits
#' @param traits.traj1 Traits of obj1
#' @param traits.traj2 Traits of obj2
#' @param obj1 The first argument to dispatch on
#' @param obj2 The second argumet to dispatch on
#' @export double.traits.apply
double.traits.apply <- function(method,
                                ...,
                                traits.traj1 = get.direct.superclasses(largs[[obj1]]),
                                traits.traj2 = get.direct.superclasses(largs[[obj2]]),
                                obj1,
                                obj2)
{
    largs <- list(...)
    for(class1 in traits.traj1)
    {
        for(class2 in traits.traj2)
        {
            child.method <- selectMethod(method,
                                         signature = c(class1,
                                                       class2))
            child.method(...)
        }
    }
}

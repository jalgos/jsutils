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

get.direct.superclasses <- function(obj)
{
    ct <- getClassDef(class(obj))@contains
    dst <- sapply(ct, function(x) x@distance)
    names(dst[dst == 1])
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

######################
## Traits composition
######################

## Traits don't work if they are VIRTUAL

#' @name mixing-traits
#' @title Adding And Removing Traits
NULL

downtrait <- function(Class,
                      object)
{
    nob <- as(object, Class)
    if(class(nob) != Class) ## Means Class is virtual
        NULL
    else
        nob
}

construct.composite <- function(Class,
                                object,
                                contained.classes,
                                superclasses,
                                new.part = NULL,
                                ...)
{
    lparts <- lapply(superclasses,
                     downtrait,
                     object)
    
    names(lparts) <- superclasses
    if(!is.null(new.part))
        lparts[[class(new.part)]] <- new.part
    lparts <- lparts[contained.classes]
    lparts <- lparts[!sapply(lparts, is.null)]
    names(lparts) <- NULL
    do.call(new,
            c(list(Class = Class),
              lparts))
}

remove.s4 <- function(superclasses,
                      trait,
                      ...)
{
    sort(setdiff(superclasses, trait))
}

add.s4 <- function(superclasses,
                   trait,
                   ...)
{
    sort(union(superclasses, trait))
}

mix.trait <- function(object,
                      trait,
                      fmodify.class,
                      fcreate = function(...) NULL,
                      ...,
                      trait.separator = ".")
{
    superclasses <- get.direct.superclasses(object)
    if(is.null(superclasses))
        superclasses <- class(object)
    contained.classes <- fmodify.class(superclasses,
                                       trait,
                                       ...)
    
    if(identical(as.character(superclasses),
                 as.character(contained.classes)))
        return(object)
        
    new.class <- paste(contained.classes,
                       collapse = trait.separator)
    
    tryCatch(getClass(new.class),
             error = function(cond)
    {
         ## Adding classes to global environment may be a problem, `methods` maintainers advise against messing up to much with internal stuff but this is the point of writing this piece of code
        setClass(new.class,
                 contains = contained.classes,
                 where = .GlobalEnv)
        
        setMethod("initialize",
                  new.class,
                  function(.Object,
                           ...)
        {
            selectMethod("initialize",
                         signature = "ANY")(.Object,
                             ...)
        },
        where = .GlobalEnv)

    })
    
    new.part <- fcreate(trait,
                        ...)
    if(length(superclasses) > length(contained.classes))
        superclasses <- contained.classes
    
    construct.composite(Class = new.class,
                        object = object,
                        contained.classes = contained.classes,
                        superclasses = superclasses,
                        new.part = new.part,
                        ...)
}

create.obj <- function(...)
{
    tryCatch(new(...),
             error = function(cond) NULL)
}

#' Mixing Traits
#'
#' @describeIn mixing-traits An interface to add a new traits to an object. The function will get all the parent traits of the current object and create a new class adding `new.trait` to the list of direct superclasses. It will create a composite object making sure the constructor of the trait is called with `...`
#' @param object The object that needs to be improved
#' @param trait Name of the trait (an S4 class) that needs to be added or remove from the class definition of the object
#' @param ... Additional parameters that will be passed on to the initializer of the new.trait
#' @return An object that inherits from its old parent traits as well as the new trait provided
#' @export
add.trait <- function(object,
                      trait,
                      ...)
{
    mix.trait(object,
              trait = trait,
              fmodify.class = add.s4,
              fcreate = create.obj,
              ...)
}

#' @describeIn mixing-traits Removes a trait from an object.
#' @export
remove.trait <- function(object,
                         trait,
                         ...)
{
    mix.trait(object,
              trait = trait,
              fmodify.class = remove.s4,
              ...)
}

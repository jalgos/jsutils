plus_join <- function(D1,
                      D2,
                      by = intersect(key(D1), key(D2)),
                      col_plus = setdiff(intersect(names(D1), names(D2)), by))
{
    DM = merge(D1, D2, all = TRUE, by = by)
    lsa = sapply(col_plus, function(x) fun_expr("safe_sum", c(paste0(x, ".x"), paste0(x, ".y"))))
    ple = assign_expr(col_plus, fun_expr("list", lsa))    
    DM[, eval(parse(text = ple))]
    delr = assign_expr(mpaste0(col_plus,  c(".x", ".y")), "NULL")
    DM[, eval(parse(text = delr))]
    DM
}

plus_join_test <- function()
{
    dt1 = data.table(u = sample(letters, 15), x = 1:15, b = rexp(15))
    dt2 = data.table(u = sample(letters, 15), x = 1:15, b = rexp(15))
    setkey(dt1, u)
    setkey(dt2, u)
    list(dt1 = dt1, dt2 = dt2, res = plus_join(dt1, dt2))
}

dtextract1 <- function(DT,
                       var,
                       var_name,
                       var_value)
{
    filt = sprintf("%s == '%s'", var_name, var)
    DF = DT[eval(parse(text = filt))]
    DF[, eval(var_name) := NULL]
    setnames(DF, var_value, var)
}

cols_from_var <- function(DT,
                          var_name,
                          var_value)
{
    vars = unique(DT[, var_name, with = FALSE][[1]])
    L = lapply(vars, dtextract1, DT = DT, var_name = var_name, var_value = var_value)
    over(L, cbind)
}

convert_columns <- function(D, lconv)
{
    N = length(lconv)
    nmconv = intersect(names(lconv), names(D))
    if(N < 1) return(D)
    for(nm in nmconv)
    {
        t = lconv[[nm]]
        convert_cols(D, nm, t)
    }
    D
}

convert_cols <- function(D, col, type___)
{
    cl = class(D[[col]])
    if(type___ %in% cl) return()
    FUN = util.type_conv_map[[type___]]
    if(!is.null(FUN)) suppressWarnings(D[, eval(col) := FUN(D[[col]])])
    else D[, eval(col) := suppressWarnings(as(D[[col]], type___))]
}

col_classes <- function(D)
{
    D[, sapply(.SD, function(x) class(x)[1])]
}

cartesian_data_table <- function(D1,
                                 D2 = D1,
                                 size_limit = Inf,
                                 ...)
{
    if(is.null(D1) || is.null(D2)) return(NULL)
    N1 = dim(D1)[1]
    N2 = dim(D2)[1]
    PS = as.numeric(N1) * as.numeric(N2)
    if(PS > size_limit)
    {
        rsample = sqrt(size_limit / PS)
        N1 = sample(1:N1, as.integer(rsample * N1))
        N2 = sample(1:N2, as.integer(rsample * N2))
        D1 = copy(D1[N1])
        D2 = copy(D2[N2])
    }
    else
    {
        D1 = copy(D1)
        D2 = copy(D2)
    }

    common_names = intersect(names(D1), names(D2))
    if(length(common_names) > 0)
    {
        setnames(D1, common_names, paste0(common_names, "1"))
        setnames(D2, common_names, paste0(common_names, "2"))
    }
    add_token_key(D1)
    add_token_key(D2)
    
    setkey(D1, key)
    setkey(D2, key)
    D = D1[D2, allow.cartesian = TRUE]
    D[, key := NULL]
    D
}

cartesian_list <- function(L, ...)
{
    ## yet another split and run
    N = length(L)
    if(N == 1) return(as.data.table(L))
    n = N %/% 2
    cartesian_data_table(cartesian_list(L[1:n]), cartesian_list(L[(n + 1):N]), ...)
}

create_cartesian_dt <- function(...)
{
    as.data.table(expand.grid(...))
}

dt_cart_apply <- function(funapp, Largs, ...)
{
    DT <- as.data.table(expand.grid(Largs))
    cc <- col_classes(DT)
    ncvt <- names(DT)[cc == "factor"]
    if(length(ncvt) > 0)
    {
        cvt <- rep("character", length(ncvt))
        names(cvt) <- ncvt
        convert_columns(DT, cvt)
    }
    if(is.character(funapp)) funapp <- get(funapp)
    DT[, do.call(funapp, c(.BY, list(...))), by = c(names(DT))]
}

get.dt.names <- function(reg.names,
                         DT,
                         ignore.case = TRUE,
                         value = TRUE,
                         ...)
{
    grep(reg.names, names(DT), value = value, ignore.case = ignore.case, ...)
}

fun_expr <- function(funstring, args, ...)
{
    nms <- names(args)
    sepa <- rep("", length(args))
    sepa[nms != ""] <- "="
    args <- paste(paste(nms, sepa, args, sep = ""), collapse = ",")
    paste(c(funstring, "(", args, ")"), collapse = "")
}

assign_expr <- function(var, exprst)
{
    var = paste0("'", var, "'")
    var = fun_expr("c", var)
    paste(var, exprst, sep = ":=")
}

dt.filter.na <- function(DT,
                         numeric.cols = names(DT),
                         good.fun = is.finite,
                         ...)
{
    DT[, over(lapply(.SD, good.fun, ...), "&"), .SDcols = numeric.cols]
}

dt.filter.out.na <- function(DT,
                             ...)
{
    F <- dt.filter.na(DT, ...)
    DT[F]
}

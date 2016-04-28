suppressMessages(library("codetools", quiet = TRUE))
suppressMessages(library("data.table", quiet = TRUE))
suppressMessages(library("methods", quiet = TRUE))
suppressMessages(library("ggplot2", quiet = TRUE))
suppressMessages(library("lubridate", quiet = TRUE))
suppressMessages(library("Matrix", quiet = TRUE))
suppressMessages(library("weights", quiet = TRUE))
options(max.print = 1500)
options(width = 100)
options(datatable.allow.cartesian = TRUE)
Sys.setenv(R_HISTSIZE = "300000") ## TODO look into 'track' package

#Making "A"+"B" = "AB"
setGeneric("%+%", function(e1, e2) standardGeneric("%+%"))
setMethod("%+%", c("ANY", "ANY"), function(e1, e2) {return(paste0(as.character(e1), as.character(e2)))})

DAYSECONDS = 24 * 60 * 60

#creates a dummy datatable to run tests on
if(!"dummyDT" %in% ls()) dummyDT = data.table(a = c("HAHA", "HIHI", "HOHO", "HUHU"), b = rnorm(4), y = 1:4, dt = rep(Sys.Date(), 4))

load_or_install <- function(library_name)
{
    if(library_name %in% row.names(installed.packages())) return(library(library_name, character.only = TRUE))
    install.packages(library_name)
    library(library_name, character.only = TRUE)
}

###Type handling and stuff#######

empty_date = function() as.IDate("1970-01-01")[0]
empty_time = function() as.ITime("00:00:00")[0]
empty_posix = function() as.POSIXct(numeric(0), origin = "1970-01-01")

util.type_fun_map <- list(character = character,
                          factor = factor,
                          integer = integer,
                          numeric = numeric,
                          logical = logical,
                          IDate = empty_date,
                          IDate.y_m_d = empty_date,
                          ITime = empty_time,
                          IDate.y.m.d = empty_date,
                          IDate.d.m.y = empty_date,
                          IDate.ymd = empty_date,
                          POSIX = empty_posix)

csv_convert_date <- function(format) function(x) format(x, format = format)

util.type_csv_map <- c(IDate = "integer",
                       IDate.y_m_d = "character",
                       IDate.y.m.d = csv_convert_date("%Y.%m.%d"),
                       IDate.d.m.y = csv_convert_date("%d.%m.%Y"),
                       IDate.ymd = csv_convert_date("%Y%m%d"),
                       POSIX = csv_convert_date("%Y%m%d %H:%M:%S") ## Won't take into account the timezone
                       )


gen_conv_date <- function(format) function(x) as.IDate(x, format = format)
gen_conv_posix <- function(format) function(x) as.POSIXct(x, format = format)
sec_conv_posix <- function(origin="1970-01-01") function(x) as.POSIXct(as.numeric(x), origin = origin)
mill_conv_posix <- function(origin="1970-01-01") function(x) as.POSIXct(as.numeric(x) / 1000, origin = origin)

convIDate <- function(x)
{
    if(class(x) != "integer") x = as.integer(x)
    as.IDate(x, origin = "1970-01-01")
}

with_missing <- function(conv_fun, default_val, null_val = "")
{
    function(x) {
        F = x != null_val
        y = rep(default_val, length(x))
        y[F] = conv_fun(x[F])
        y
    }
}

util.type_conv_map <- list(IDate = convIDate,
                           ITime = function(x)as.ITime(x, format = "%H:%M:%S"),
                           factor = as.factor,
                           IDate.y_m_d = as.IDate,
                           IDate.y.m.d = gen_conv_date("%Y.%m.%d"),
                           IDate.d.m.y = gen_conv_date("%d.%m.%Y"),
                           IDate.ymd = gen_conv_date("%Y%m%d"),
                           POSIXct = with_missing(as.POSIXct, default_val = as.POSIXct(NaN, origin = "1970-01-01")),
                           POSIX = gen_conv_posix("%Y-%m-%dT%H:%M:%S"),
                           POSIXsec = sec_conv_posix(),
                           POSIXmill = mill_conv_posix())

over <- function(L, FUN, default_value = NULL, ...)
{
    if(length(L) == 0) return(default_value)
    if(is.character(FUN)) FUN = getMethod(FUN)
    
    N = length(L)
    R = L[[N]]
    if(N == 1) return (R)

    for(i in (N - 1):1) R = FUN(L[[i]], R, ...)
    
    return(R)
}

pca <- eigen

ncomb <- function(n, k) {factorial(n) / (factorial(k) * factorial(n - k))}

first <- function(X)
{
    if(is.list(X)) X[[1]]
    else X[1]
}

last <- function(X)
{
    if(is.list(X)) X[[length(X)]]
    else X[[length(X)]]
}

positive_val <- function(X)
{
    X[X < 0] <- 0 
    X
}
negative_val <-function(X)
{
    X[X > 0] <-0
    X
}


fusion_factors <- function(F1, F2, lev)
{
    return(factor(c(as.character(F1), as.character(F2)), levels = lev))
}

eqfactors <- function(F1, F2)
{
    return(as.character(F1) == as.character(F2))
}

safe_sum<-function(X, Y)
{
    X[!is.finite(X)] = 0
    Y[!is.finite(Y)] = 0
    X + Y
}

safe_mean <- function(...)
{
    N = -1
    for(X in list(...))
    {
        if(N == -1)
        {
            N = length(X)
            nobs = rep(0,N)
            sums = rep(0,N)
        }
        if(length(X) != N) stop("one item as a different size")
        goodX =  is.finite(X)
        nobs = nobs + goodX
        sums[goodX] = sums[goodX] + X[goodX]
    }
    return(sums / nobs)
}

replaceNA <- function(X, Y = 0)
{
    Y = rep(Y, length.out = length(X))
    goodX = is.finite(X)
    X[!goodX] = Y[!goodX]
    return(X)
}

filter_sum <- function(X, Y, filter)
{
    Z = X
    Z[filter] = Z[filter] + Y
    return(Z)
}

quit_save <- function(info = "", folder = "Rhistory")
{
    V = scan(pipe("ls"), what = "character")
    if(!folder %in% V) system(paste("mkdir", folder))
    date = gsub("-", "", Sys.Date())
    subfolder = paste(folder, date, sep = "/")
    V = scan(pipe(paste("ls", folder)), what = "character")	
    if(!date %in% V) system(paste("mkdir", subfolder))
    filename = paste(c(info, "Rhistory", gsub(" |-|:", "", Sys.time())), collapse=".")
    filename = paste(c(subfolder, filename), collapse = "/")
    savehistory(file = filename)
    print(c("saved rhistory: ", filename))
    q()
    
}

pmaxnarm <- function(X, Y)
{
    pmax(X, Y, na.rm = TRUE)
}


fit_gamma_dist_moments <- function(X, ...)
{
    X = X[is.finite(X)]
    M = mean(X)
    V = var(X)
    Kappa = M ^ 2 / V
    Scale = V / M
    H = pretty_hist(X,...)
    scaling = length(X) * (H$breaks[2] - H$breaks[1])
    if(!is.null(list(...)$freq) && list(...)$freq == FALSE) scaling = 1
    
    points(H$breaks, dgamma(H$breaks, scale = Scale, shape = Kappa) * scaling, col = 'red', type = 'l')
    legend("topright", c(paste("kappa=", format(Kappa, digits = 2)), paste("scale=", format(Scale, digits = 2))))
    c(Kappa, Scale)
}

grad_dgamma <- function(D, par)
{
    shap = par[1]
    scal = par[2]
    dscale = (-shap / scal + D / scal ^ 2)
    dshape = (log(D) - log(scal) - digamma(shap))
    return(-c(sum(dshape), sum(dscale)))
}

hessian_dgamma <- function(D, par)
{
    shap = par[1]
    scal = par[2]
    dscale2 = sum(shap / scal ^ 2 - 2 * D / scal ^ 3)
    dscaledshape = (-1 / scal) * length(D)
    dshape2 = (-trigamma(shap)) * length(D)
    return(-cbind(c(dshape2, dscaledshape), c(dscaledshape, dscale2)))
}

neg_fun <- function(FUN) function(...) {-FUN(...)}
apply.fun <- function(f, X, ...) f(X, ...)

#grad cond shows that shape*scale=mean(D)

like_gamma <- function(D, par, ...) -sum(log(dgamma(D, shape = par[1], scale = par[2])))
like_gamma1 <- function(D, shap, ...)
{
    scal = mean(D) / shap
    -sum(log(dgamma(D, shape = shap, scale = scal)))
}

dlike_gamma1 <- function(D, shap, ...)
{
    scal = mean(D) / shap
    sum(-(log(D) - log(scal) - digamma(shap)))
}

ddlike_gamma1 <- function(D, shap, ...)
{
    scal = mean(D) / shap
    as.matrix(-sum(1 / shap - trigamma(shap)), 1, 1)
}

fit_gamma_dist_like <- function(X, ...)
{
    X = X[is.finite(X) & X>0]
    M = mean(X)
    V = var(X)
    Kappa = M ^ 2 / V
    Scale = V / M    
    
    #MLO = optim(c(Kappa,Scale),fn = like_gamma,gr=grad_dgamma,method="L-BFGS-B",lower = c(1,1E-5),D=X,control=list(maxit=1000))
    MLN = nlminb(start = c(Kappa, Scale), objective = like_gamma, gradient = grad_dgamma, hessian = hessian_dgamma, lower  = c(1, 1E-5), D = X)
    Kappa = MLN$par[1]
    Scale = MLN$par[2]
    H = pretty_hist(X, ...)
    scaling = length(X) * (H$breaks[2] - H$breaks[1])
    if(!is.null(list(...)$freq) && list(...)$freq == FALSE)
    {
        scaling = 1
    }
    points(H$breaks, dgamma(H$breaks, scale = Scale, shape = Kappa) * scaling, col = 'red', type = 'l')
    legend("topright", c(paste("kappa=", format(Kappa, digits = 2)), paste("scale=", format(Scale, digits = 2))))
    list(c(Kappa, Scale), H)
}

dist_diff_gamma_2 <- function(X, scale)
{
    1 / (4 * scale) * (scale + abs(X)) * exp(-abs(X) / scale)
}

dist_diff_gamma_n <- function(k, X, scale)
{
    Coeff = matrix(0, k, k)
    Coeff[1, 1] = 1
    rev_ord = k:1
    ord = 1:k
    F = sapply(ord, factorial)
    Y = X / scale
    Y = -abs(Y)
    if(k == 1) return(1 / 2 * exp(Y))
    for(j in 1:(k - 1))
    {
        V = Coeff[j,] * (-1) ^ ord * F
        V = cumsum(c(0, V[-k])[rev_ord])[rev_ord]
        V = (-1) ^ (ord) * V
        Coeff[j + 1,] = V
    }
    M = t(sapply(Y, "^", 0:(k - 1)))
    R = M %*% Coeff[k,]
    1 / (2 ^ k * factorial(k - 1)) * R * exp(Y)
}

I_n_numeric <- function(x, k, scale)
{
    Nb = 1000
    xinf = min(x, log(1E-6) * scale)
    I = xinf + 1:1000 * (x - xinf) / 1000
    F = I ^ k * exp(I / scale)
    return(sum(F * (x - xinf) / 1000))	
}

tail_like_exp <- function(alpha, X, p)
{
    xp = -log(1 - p) / alpha
    
    n = length(X)
    L = rep(0, length(X))	
    nm = sum(X < xp)
    
    L1 = 0
    L1 = lchoose(n, nm) + nm * log(p) + log(1 - p) * (n - nm)
        
    L[X > xp] = log(alpha) - alpha * (X[X > xp]) + alpha * xp
    
    c(L1, sum(L), n, nm)
    sum(L) / (n - nm) + L1 / n
}

binom_like <- function(p, X)
{
    n = length(X)
    nm = sum(X == 0)
    lchoose(n, nm) + nm * log(p) + log(1 - p) * (n - nm)	
}

gridl <- function(log = NULL, ...)
{
    grid(list(...)$nx, list(...)$ny)
}

max_tail_like_exp <- function(D, p)
{
    ##finding corresponding alphas for all x
    X = sort(D,decreasing = TRUE)
    N = length(X)
    if(N == 0 | all(D == 0)) return(0)
                                      
    nm = (N-1):0
    CX = cumsum(X)
    alphas = -log(1 - p) / X
    F = is.finite(alphas)
    nm = nm[F]
    alphas = alphas[F]
    CX = CX[F]	
    L1 = (lchoose(N, nm) + nm * log(p) + (N - nm) * log(1 - p)) / N
    L2 = log(alphas) - alphas * CX / (N - nm) + log(1 - p)
    DL = 1 / alphas - CX / (N - nm)
    L = L1 + L2
    i = which.max(L)
    alphas_m = alphas + c(diff(alphas), 0)
    alphas_0 = ((N - nm) / CX)[-length(CX)]
    alphas_m[c(alphas_0 > alphas[-length(alphas)] & alphas_0 < alphas[-1], FALSE)] = alphas_0[alphas_0 > alphas[-length(alphas)] & alphas_0 < alphas[-1]]
    Lm1 =  (lchoose(N,nm) + nm * log(p) + (N-nm) * log(1 - p)) / N
    Lm2 = log(alphas_m) - alphas * CX / (N - nm) + log(1 - p)
    Lm = Lm1 + Lm2
    Fi = Lm > L
    alphas[Fi] = alphas_m[Fi]
    L[Fi] = Lm[Fi]
    na = length(alphas)
    if(DL[na] > 0)
    {
        alphas_m[na] = N / CX[na]
        L1[na] = (lchoose(N, 0) + nm[na] * log(p) + (N - nm[na]) * log(1 - p)) / N
        L2[na] = log(alphas_m[na]) - alphas_m[na] * CX / (N - nm[na]) + log(1 - p)
        L[na] = L1[na] + L2[na]
    }
    alphas[which.max(L)]
}

tail_like_gamma <- function(kappa, theta, X ,p)
{
    xp = qgamma(p, rate = kappa, shape = theta)
    
    n = length(X)
    L = rep(0, length(X))
    nm = sum(X < xp)

    L1 = 0
    L1 = lchoose(n, nm) + nm * log(p) + log(1 - p) * (n - nm)
	
    L[X > xp] = -kappa * log(theta) - log(Gamma(kappa)) + (kappa - 1) * log(X[X > xp]) - (X[X > xp] / theta) + log(1 - p)

    c(L1, sum(L), n, nm)
    sum(L) / (n - nm) + L1 / n	
}

cart_apply <- function(L1, L2, funapp, ..., fapply = lapply, fapply2 = fapply, unlist = FALSE)
{
    CAR <- fapply(L1, function(i1, L2, ...) fapply2(L2, function(x, y, funa,...) funa(x, y, ...), i1, ...), L2, funapp, ...)
    if(unlist) base::unlist(CAR)
    else base::unlist(CAR)
}

## applies when we want to compute f(xi, xj) only when xi <= xj
half_cart_apply <- function(L, funapp, ..., fapply = lapply, fapply2 = fapply)
{
    N = length(L)
    if(N > 0) LI = 1:N
    else LI = numeric(0)
    fapply(LI, function(i, ...) fapply2(i:N, function(j, ...) funapp(L[[i]], L[[j]], ...), ...), ...) 
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
    DT[, do.call(funapp, c(.BY, ...)), by = c(names(DT))]
}

divide_intervals <- function(D, n)
{
    il = sapply(D, function(dh){dh[2] - dh[1]})
    if(sum(il) == 0)
    {
        dv = unlist(lapply(D, function(d, nd){rep(d[1], nd)}, n / length(D)))
        if(length(dv) != n) dv = c(dv, dv[1:(n - length(dv))])
        return(dv)
    }
    il = il / sum(il)
    nil = n * il
    pil = nil %% 1;
    demi = which(pil == .5)	
    add = .1
    for(i in demi)
    {
        nil[i] = nil[i] + add;
        add = -add;
    }
    nil = round(nil);
    unlist(mapply(FUN = function(d, np){d[1] + (d[2] - d[1]) * seq(0.5, np - .5, 1) / np}, D, nil))
}

loadConfig <- function(conf_file)
{
   T =  scan(conf_file, sep = ",", what = "character")
   CF = NULL
   N = length(T)
   CF = as.list(T[1:N %% 2 == 0])
   names(CF) = T[1:N %% 2 == 1]
   return(CF)
}

divide_args <- function(args)
{
    add_args = grep("=", args, value = TRUE)
    L = strsplit(add_args, "=")
    R = NULL
    for(l in L)
    {
        l[[1]] = gsub("-", "", l[[1]])
        R[[l[[1]]]] = l[[2]]
    }
    R
}

transform_filename <- function(fn, subs = base_sub_list)
{
    if(is.null(subs)) return(fn)
    subs[sapply(subs, is.null)] = ""
    
    NM = sort(names(subs), decreasing = TRUE)
    for(nm in NM)
    {
        pat = gsub(x = nm, pattern = "\\$", replacement = "\\\\$")
        fn <- as.vector(sapply(fn, {function(fn, repls) sapply(repls, function(repl) gsub(x = fn, pattern = pat, replacement = repl))}, repls = subs[[nm]]))
    }
    return(fn)
}

replace_variables = transform_filename

make_empty_factors <- function(N)
{
    factor(character(N))
}

type_funs <- list(numeric = numeric,
                  integer = integer,
                  character = character,
                  logical = logical,
                  factor = make_empty_factors,
                  IDate = function(N) as.IDate(rep(NA, N)))

empty_column <- function(type, N)
{
    ft = type_funs[[type]]
    ft(N)
}

rbind_any_tables <- function(D1, D2, use_copy = FALSE)
{
    #row binds D1 and D2 no matter by adding the missing columns in each table
    #will fail if cols have different types in D1 and D2
    if(use_copy)
    {
        D1 = copy(D1)
        D2 = copy(D2)
    }
    c1 = col_classes(D1)
    c2 = col_classes(D2)
    n1 = names(c1)
    n2 = names(c2)
    common = intersect(n1, n2)
    if(! all(c1[common] == c2[common]))
    {
        cs = c1[common][c1[common] != c2[common]]
        print(c("different colTypes: ", cs))
        return()
    }
    m1 = setdiff(n2, n1)
    m2 = setdiff(n1, n2)
    L1 = lapply(c2[m1], empty_column, N = D1[,.N])
    L2  =lapply(c1[m2], empty_column, N = D2[,.N])
    if(length(L1) > 0) D1 = cbind(D1, as.data.table(L1))
    
    if(length(L2) > 0) D2 = cbind(D2,as.data.table(L2))
    
    rbind(D1, D2, use.names = TRUE)
}

load_data_table <- function(io, io_header, col_types, ...)
{
    H = scan(io_header, what = "character", sep = ",")
    ctypes  = unlist(col_types[H])
    as.data.table(read.table(io, colClasses = ctypes, ...))
}

evalLand <- function(Tab, Filts)
{
    RF = TRUE
    if(class(Filts) == "logical") return(Filts)
    
    if(class(Filts) == "expression")
    {
        for(F in Filts) RF  = RF & Tab[,eval(F)]
        RF[!is.finite(RF)] = FALSE
        return(RF)
    }
    for(i in 1:length(Filts)) RF = RF & Tab[,eval(Filts[[i]])]
    RF[!is.finite(RF)] = FALSE
    RF
}

permut <- function(V)
{
    n = length(V)    
    if(n == 1)
    {
        return(list(V))
    }
    L = NULL
    for(v in V)
    {
        Lv = arrange(V[V != v])
        L = c(L,lapply(Lv, c, v))        
    }
    return(L)
}

null_fun <- function(...)
{
    0
}

one_const_fun <- function(...)
{
    1
}

const_fun <- function(val=0,...)
{
    val
}

mpaste <- function(c1, c2, ...)
{
    unlist(lapply(c1, paste, c2, ...))
}

mpaste0 <- function(c1, c2, ...)
{
    unlist(lapply(c1, paste0, c2, ...))
}

recenter <- function(T, name_cols, fun = mean, ...)
{
    for(name_col in name_cols)
    {
        setnames(T, name_col, "x__c")
        T[, x__c := x__c - fun(x__c, ...)]
        setnames(T, "x__c", name_col)
    }
}

add_rank_column <- function(T, name_cols, prefix = "rnk", pct = FALSE, fby = NULL)
{
    for(name_col in name_cols)
    {
        N = dim(T)[1]
        ex_str = "%rncol := rank(-%col)"
        if(pct) ex_str = paste0(ex_str, "/N")
        ex_str = gsub("%col", name_col, ex_str)
        ex_str = gsub("%rncol", paste(prefix, name_col, sep="_"), ex_str)
        T[, eval(parse(text = ex_str)), by = fby]
    }
    T
}

center <- function(X, cent = mean(X))
{
    return(X - cent)
}

scale_and_center <- function(X)
{
    return((X - mean(X, na.rm = TRUE)) / sd(X, na.rm = TRUE))
}

flip <- function(DT, by)
{
    #nicely prints low dimension data tables
    if(missing(by)) return(cbind(data.table(col = names(DT)), t(as.matrix(DT))))
    keys = as.character(DT[[by]])
    DT = DT[, setdiff(names(DT), by), with = FALSE]
    FD = as.data.table(t(as.matrix(DT)))
    setnames(FD, keys)
    return(cbind(data.table(col = names(DT)), FD))
}

confidence_interval <- function(n, N, conf_quant)
{
    pe = n / N
    z  = qnorm(conf_quant)
    err=(2 * n + z ^ 2) / (2 * (N + z ^ 2))
    pm = z * sqrt(z ^ 2 + 4 * n * (1 - pe)) / (2 * (N + z ^ 2))
    list(conf_sup = err + pm, conf_inf = err - pm, estimate = pe)
}

independance_test_categorial <- function(Dobs, Dexp, nobs, qt = .05)
{
    N = length(Dobs)
    list(chi2_val = nobs*sum((Dobs - Dexp) ^ 2 / Dexp), thresh = qchisq(qt, df = N - 1))
}

nearly_equal <- function(x, y, tol = .Machine$double.eps ^ .5)
{
    abs(x - y) < tol
}

nearly_in_set <- function(x, set, tol = .Machine$double.eps ^ .5)
{
    if(is.null(set)) return(FALSE)
    over(lapply(set, nearly_equal, x = x, tol = tol), "|")
}

random_distrib <- function(N, skew)
{
    P = rgamma(N, skew)
    return(P / sum(P))
}

random_cat_var <- function(var, set_size)
{
    var_name = var$name
    items = var$items
    if(is.null(items))
    {
        nvals = var$nvars
        items = 1:nvals
    }
    proba
    V = list(sample(items, set_size, replace = TRUE))
    names(V) = var_name
    V
}

random_data_set <- function(set_size, varsdescription)
{
    D = NULL
    as.data.table(lapply(varsdescription, random_cat_var, set_size))
}

date_to_hexmode <- function(d)
{
    as.hexmode(as.integer(as.POSIXct(d)))
}

dtgrep <- function(D, vals, akeys, fgrep = grep, keep_match_col = FALSE, ...)
{
    if(is.character(fgrep)) fgrep = getFunction(fgrep)
    if(length(akeys) == 0) return(D)
    
    cols = match(akeys,names(D))
    ncols = 1:dim(D)[2]
    if(!keep_match_col) ncols = setdiff(ncols, cols)
    
    if(dim(D)[1] > 0) F = D[, over(mapply(fgrep, vals, .SD, MoreArgs = list(...), SIMPLIFY = FALSE), intersect), .SDcols = cols]
    else F = integer(0)
    
    if(sum(F) > 0) MD = copy(D[F, .SD, .SDcols = ncols])
    else MD = copy(D[0,.SD, .SDcols = ncols][1])
    
    if(keep_match_col) setnames(MD, akeys, paste(akeys, "match", sep="_"))
    return(MD)
}

which.equals <- function(X, Y)
{
    if(is.factor(X)) X = as.character(X)
    if(is.factor(Y)) Y = as.character(Y)
    which(X == Y)
}

grep_merge <- function(D1, D2, keys = character(0), akeys = character(0), fgrep = grep, ...)
{
    ## performs a merge using an approximative grep
    ## D = merge(D1,D2,keys = keys)
    if(dim(D1)[1] == 0)
    {
        R = merge(D1, D2, by = c(keys, akeys))
        R[,eval(paste(akeys, "match", sep="_")) := R[, akeys, with = FALSE]]
        return(R)
    }
    nk = length(keys)
    if(nk > 0) FK = 1:nk
    else FK = integer(0)
    nak = length(akeys)
    FAK = setdiff(1:(nk + nak), FK)
    
    R = D1[,dtgrep(dtgrep(D2, .BY[FK], keys, which.equals), .BY[FAK], akeys, fgrep, keep_match_col = TRUE, ...), by = c(keys,akeys)]
    if(dim(D1)[1] == 0) return(R[0])
    return(R)
}

amerge <- function(D1, D2, keys = character(0), akeys = character(0), fgrep = grep, similarity_fun, ...)
{
    if(length(keys)==0) D = cartesian_data_table(D1, D2)
    else D = merge(D1, D2, by = keys, suffixes = c("1", "2"))
    akeys1 = paste0(akeys, "1")
    akeys2 = paste0(akeys, "2")
    D[similarity_fun(as.data.frame(D)[akeys1], as.data.frame(D)[akeys2], ...)]   
}

min_adist <- function(X,
                      Y,
                      max.dist = list(min_distance = 0, max_distance = 0.1, diff_distance = 1),
                      nkeep = 1,
                      value = FALSE,
                      ...)
{
    #assuming X as a length of 1
    X = as.character(X)
    Y = as.character(Y)
    ncx = nchar(X)
    ncy = nchar(Y)
    diff_dist = abs(ncx - ncy)
    max_dist = pmax(ncx, ncy)
    min_dist = pmin(ncx, ncy)
    minf = ifelse(is.null(max.dist$min), 0, max.dist$min)
    maxf = ifelse(is.null(max.dist$max), 0, max.dist$max)
    difff = ifelse(is.null(max.dist$diff), 0, max.dist$diff)
    
    maxd = minf * min_dist + difff * diff_dist + maxf * max_dist
    D = as.vector(adist(X, Y, ...))
    nkeep = min(length(D), nkeep)
    if(nkeep == 0) return(list(indices = integer(0), edist = integer(0)))
    IK = order(D)[1:nkeep]
    IK = IK[ is.finite(D[IK]) & D[IK] <= maxd[IK] ]
    if(value) MK = Y[IK]
    else MK = IK
    return(list(matches = MK , edist = D[IK]))
}

alternation <- function(v)
{
    paste(c("(", paste(v, collapse = "|"), ")"), collapse="")
}

rbind_no_warnings <- function(...)
{
    rbind(..., use.names = TRUE)
}

defined <- function(x)!missing(x)

assign_matrix_vect <- function(M, i, j, values)
{
    n = nrow(M)
    M[(j -1 ) * n + i] = values
    M
}


rating_normalize <- function(X, fun_proj = exp, filter = TRUE)
{
    fun_proj(X) / fun_proj(max(X[as.logical(filter)], na.rm = TRUE))
}

rank_normalize <- function(X, rank_norm)
{
    if(length(X) == 0) return(X)
    centering = median(sort(X[[1]], decreasing = TRUE)[rank_norm], na.rm = TRUE)
    return(lapply(X, "-", centering))
}

make_list_expression <- function(exprvec, parse = TRUE)
{
    nme = names(exprvec)
    if(!length(nme)) nme = paste("V", 1:length(exprvec), sep="")
    varnames = paste(paste("`", nme, sep=""), "`", sep="")
    exprs = paste(varnames, exprvec, sep="=")
    exprs = paste0(paste0("list(", paste(exprs, collapse = ",")), ")")
    if(!parse) return (exprs)
    return(parse(text = exprs))
}

get_keyboard_input <- function(...)
{
    io = stdin()
    x = readLines(io, 1)
    return(x)
}

press_key_to_continue <- function(...)
{
    cat("Press key to continue\n")
    get_keyboard_input()
}


y_to_continue <- function()
{
    "yes" == get_keyboard_input()
}

category_plot <- function(x, y, ctg, ...)
{
    colors = factor_palette(ctg)
    cols = colors[ctg]
    plot(x, y, col = cols)
    legendize(levels(ctg), colors)
    return(NULL)
}

legendize <- function(labels, colors)
{
    legend(x = "topright", labels, text.col = colors, bty = "n")
} 
col_combine <- function(cols)
{
    vcor = apply(col2rgb(cols), 1, mean)
    rgb(red = vcor["red"], green = vcor["green"], blue = vcor["blue"], maxColorValue = 256)
}

factor_palette <- function(facts, fpalette = rainbow, ...)
{
    facts = as.factor(facts)
    n = length(levels(facts))
    fpalette(n, ...)
}


#uuid

get_uuid <- function()
{
    system("uuid", intern = TRUE)
}

yyyymmdd <- function(date)
{
    if(is(date, "Date") || is(date, "POSIXt"))
    {
        return(format(date, "%Y%m%d"))
    }
    return(date)
}

find_sequence <- function(R)
{
    R = unique(sort(R))
    N = length(R)
    V = R[-1] - R[-N] != 1
    return(list(from = R[c(TRUE, V)], to = R[c(V, TRUE)]))
}

cor0 <- function(x, y, ...)
{
    #correlation enforcing E(x) = E(y) = 0
    
    return(mean(x * y,...) / sqrt(mean(x^2, ...) * mean(y ^ 2, ...)))
}

Chi2_test <- function(X, Y, nb_quant, ...)
{
    
    X = data.table(X)
    nb_points_X = length(X[,X])
    cat("number of points is ", nb_points_X, "\n")
    interval = 1:nb_quant / nb_quant
    if(is.vector(Y))
    {
        nb_points_Y = length(Y)
        Y_dist = data.table(interval, quant = quantile(Y, probs = interval, na.rm = TRUE))
    }
    else if(is.function(Y)) 
    {
        Y_dist = data.table(interval, quant = Y(interval, ...))
    }
    
    biggest = max(X, Y_dist[, quant])
    Y_dist[dim(Y_dist)[1], quant := biggest]
    
    setkeyv(Y_dist, "quant")
    setkeyv(X, "X")
    
    X_QUANT = Y_dist[X, roll = -Inf][, .N, by = interval]
    
    X_QUANT[,chi2 := (N - nb_points_X / nb_quant) ^ 2 / nb_points_X * nb_quant]
    
    print(X_QUANT[order(interval)])
    
    TEST=sum(X_QUANT[, chi2])
    
    print(TEST)
    
    reject_percent = 100 * pchisq(TEST, df = nb_quant - 1)

    jlog.info(gblog, "The null hypothesis that the empirical distrib can be generated by the hypothetic distribution is rejected at ", reject_percent, "%")
    reject_percent
}

proportion <- function(q1, q2)
{
    q1 / ( q1 + q2 )
}

add_to_date <- function(date, y = 0, m = 0, d = 0, ...)
{
    date = as.POSIXct(date)
    year(date) <- year(date) + y
    month(date) <- month(date) + m
    day(date) <- day(date) + d
    return(date)
}

safe_max <- function(X, ..., na.rm = FALSE)
{
    #Max without warning
    if(na.rm) X = X[!is.na(X)]
    if(length(X) == 0) return(NaN)
    max(X)
}

safe_min <- function(X, ..., na.rm = FALSE)
{
    #Min without warning
    if(na.rm) X = X[!is.na(X)]
    if(length(X) == 0) return(NaN)
    min(X)
}

un_diff_int <- function(S1, S2)
{
    setdiff(union(S1, S2),intersect(S1, S2))
}

remove_duplicates_dt <- function(D, key)
{
    D[,`_nrecord` := 1:.N, by = c(key)]
    D = D[`_nrecord` == 1]
    D[,`_nrecord`:=NULL]
    D
}

#CJ of data.table reorders inputs
#this is a cartesian data table similar to CJ using expand.grid

get_memory_usage <- function()
{
    sort(sapply(ls(), function(x) object.size(get(x))))
}

mb_object_size <- function(x)
{
    return(object.size(x) / 2 ^ 20)
}

#Total volatility contained in a covariance matrix
total_vol <- function(S)
{
    sum(diag(S))
}

#identity for object of class Matrix
Matrix.identity <- function(n)
{
    Diagonal(n)
}

#Makes a diagonal matrix from a vector
Matrix.diag <- function(V)
{
   Diagonal(length(V), V)
}


#Multi purposes generics
setGeneric("shift", function(X, ...) standardGeneric("shift"))

## Memory usage
mem_usage <- function(LOBJS = NULL,
                      envir = .GlobalEnv,
                      N = 10)
{
    if(is.null(LOBJS)) sort(sapply(ls(envir = envir, all.names = TRUE),function(x){mb_object_size(get(x, envir = envir))}), decreasing = TRUE)[1:N]
    else sort(sapply(LOBJS, mb_object_size), decreasing = TRUE)[1:N]
}

safe_system <- function(command,
                        error_message = paste("Error running command:", command),
                        safe = TRUE,
                        ...)
{
    err = system(command, ...)
    if(err != 0 & safe) stop(error_message)
}

## Deep assignation of retrieve in list

deep.list <- function(L,
                      keys,
                      value)
{
    if(length(keys) == 1)
    {
        if(missing(value)) return(L[[keys]])
        L[[keys]] = value
        return(L)
    }
    if(missing(value)) return(deep.list(L[[keys[1]]], keys[-1]))
    L[[keys[1]]] <- deep.list(L[[keys[1]]], keys[-1], value)
    L
}

rep.list <- function(X,
                     N)
{
    R <- list()
    R[1:N] <- list(X)
    R
}

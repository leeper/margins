# function to cleanup I(), etc. in formulas
gsub_bracket <- function(a, b) {
    tmp <- regmatches(a, gregexpr(paste0("(",b,"\\().+(\\))"), a))
    regmatches(a, gregexpr(paste0("(",b,"\\().+(\\))"), a)) <- 
      gsub(")$","", gsub(paste0("^",b,"\\("), "", tmp))
    a
}
# function to drop multipliers, powers, etc.
drop_operators <- function(a) {
    # remove mathematical operators
    a <- gsub("^[:digit:]+[\\^\\+\\-\\*\\/]", "", a)
    a <- gsub("[[\\^\\+-\\*/]][[:digit:]+]$", "", a)
    # need to remove mathematical expressions
    exprs <- c("exp", "log", "sin", "cos", "tan", "sinh", "cosh", 
               "sqrt", "pnorm", "dnorm", "asin", "acos", "atan", 
               "gamma", "lgamma", "digamma", "trigamma")
    for(i in seq_along(exprs)){
        a <- gsub_bracket(a, exprs[i])
    }
    a
}

margins_calculator <- 
function(x, mm = NULL, ...){
    # setup objects
    if(is.null(mm))
        mm <- as.data.frame(model.matrix(x)) # data
    datmeans <- as.data.frame(t(as.matrix(colMeans(mm)))) # data means
    tl <- names(mm)[names(mm) != "(Intercept)"] # terms
    est <- coef(x) # coefficients
    termorder <- attributes(terms(x))$order # term orders
    vc <- vcov(x) # var-cov matrix
    n <- nrow(mm)
    
    # add variables to `mm` if they are represented in I() terms but not in their original forms
    Iterms <- grepl("I\\(", tl)
    Iterm_vars <- drop_operators(gsub_bracket(tl[Iterms],"I"))
    w <- which(!Iterm_vars %in% tl[!Iterms])
    if(length(w)){
        stop("AsIs term(s) without corresponding unmodified variables in formula\n  will not have marginal effect calculated correctly")
        #for(i in w) {
        #    mm[,Iterm_vars[w]] <- # do something
        #}
    }
    
    tl <- gsub_bracket(tl,"I")
    names(mm) <- gsub_bracket(names(mm), "I") # I() terms
    anyfactors <- grepl("factor(", attributes(terms(x))$term.labels, fixed = TRUE)
    if(any(anyfactors)){
        names(mm) <- gsub_bracket(names(mm), "factor") # factor() terms
        for(i in which(anyfactors)){
            termorder <- c(termorder[0:(i-1)],
                           rep(termorder[i],nlevels(x$model[,attributes(terms(x))$term.labels[i]])-1),
                           tail(termorder, length(termorder) - i))
        }
    }
    
    
    # make interaction terms derivable by replacing `:` with `*`
    # also drop factor() expressions
    f <- gsub(":", "*", paste(gsub_bracket(est[names(est) != "(Intercept)"], "factor"), 
                              gsub_bracket(tl, "factor"), sep="*", collapse=" + "))
    # find unique, first-order terms
    tmpnames <- unique(tl[termorder == 1])
    utmp <- unique(drop_operators(tmpnames))
    u <- gsub_bracket(utmp, "factor")
    
    # Marginal Effect calculation (linear models)
    # do the calculation using the symbolic derivative
    d <- with(mm, eval(deriv3(reformulate(f)[[2]], u)))
    MEs <- attributes(d)$gradient
    
    # Variance calculation
    # without higher-order terms it is just:
    vctmp <- vc
    colnames(vctmp) <- rownames(vctmp) <- gsub_bracket(colnames(vctmp), "factor")
    #Variances <- diag(vctmp[u,u])
    
    # but...using the delta method
    if(any(termorder > 1))
        warning("Variance estimates are incorrect for variables included in higher-order terms")
    betas <- setNames(tl, paste0('beta', seq_along(tl)))
    f2 <- reformulate(gsub(":", "*", paste(names(betas), 
                               gsub_bracket(tl, "factor"), sep="*", collapse=" + ")))[[2]]
    Variances <- setNames(sapply(u, function(z) {
        this_me <- D(f2, z) # ME
        a <- all.vars(this_me)
        a <- a[grepl("beta",a)] # coefs included in ME
        gradmat <- do.call(cbind, lapply(a, function(b) {
            #grad <- with(mm, eval(D(this_me, b))) # gradient evaluated at *data*
            grad <- with(datmeans, eval(D(this_me, b))) # gradient evaluated at *means*
            # repeat length 1 vectors
            #if(length(grad)==1) rep(grad, nrow(mm)) else grad
        }))
        colnames(gradmat) <- betas[a]
        # estimate variance using delta method
        gradmat %*% vc[colnames(gradmat), colnames(gradmat)] %*% t(gradmat)
    }), u)
    
    colnames(MEs) <- utmp
    structure(list(Effect = MEs,
                   Variance = Variances,
                   model = x), class = c("margins"))
}

margins <- 
function(x, newdata = NULL, ...) {
    UseMethod("margins")
}

margins.lm <- function(x, newdata = NULL, ...){
    margins_calculator(x, ...)
}

margins.glm <- 
function(x, 
         newdata = NULL, 
         type = "link", # "link" (linear/xb); "response" (probability scale)
         ...){
    out <- margins_calculator(x, ...)
    # configure link function
    dfun <- switch(x$family$link, 
                  probit = dnorm, 
                  logit = dlogis,
                  cauchit = dcauchy,
                  log = exp,
                  cloglog = function(z) 1 - exp(-exp(z)),
                  inverse = function(z) 1/z,
                  identity = function(z) 1,
                  sqrt = function(z) z^2,
                  "1/mu^2" = function(z) z^(-0.5),
                  stop("Unrecognized link function")
                  )
    sfun <- switch(x$family$link, 
                  probit = function(z) -z, 
                  logit = function(z) 1 - 2 * plogis(z),
                  cauchit = function(z) 1, # not setup
                  log = function(z) 1, # not setup
                  cloglog = function(z) 1, # not setup
                  inverse = function(z) 1, # not setup
                  identity = function(z) 1, # not setup
                  sqrt = function(z) 1, # not setup
                  "1/mu^2" = function(z) 1, # not setup
                  stop("Unrecognized link function")
                  )

    if(type == "response"){
        # Marginal Effect calculation (response scale for GLMs)
        out$Effect <- apply(out$Effect, 2, `*`, predict(x, newdata = as.data.frame(model.matrix(x)), type = "response"))
    }
    
    out
    
}

margins.plm <- 
function(x, newdata = NULL, ...) {
    # FOR SOME REASON THIS ISN'T CAPTURING INTERACTION TERMS
    if(x$args$model != 'pooling') {
        warning("marginal effects not likely to be correct")
        margins_calculator(x, ...)
        #mm <- cbind(model.matrix(x), model.matrix(~0+attributes(x$model)$index[,1]))
    } else {
        margins_calculator(x, ...)
    }
}

margins.pglm <- 
function(x, 
         newdata = NULL, 
         type = "link", # "link" (linear/xb); "response" (probability scale)
         ...){
    out <- margins_calculator(x, ...)
    # configure link function
    dfun <- switch(x$family$link, 
                  probit = dnorm, 
                  logit = dlogis,
                  cauchit = dcauchy,
                  log = exp,
                  cloglog = function(z) 1 - exp(-exp(z)),
                  inverse = function(z) 1/z,
                  identity = function(z) 1,
                  sqrt = function(z) z^2,
                  "1/mu^2" = function(z) z^(-0.5),
                  stop("Unrecognized link function")
                  )
    sfun <- switch(x$family$link, 
                  probit = function(z) -z, 
                  logit = function(z) 1 - 2 * plogis(z),
                  cauchit = function(z) 1, # not setup
                  log = function(z) 1, # not setup
                  cloglog = function(z) 1, # not setup
                  inverse = function(z) 1, # not setup
                  identity = function(z) 1, # not setup
                  sqrt = function(z) 1, # not setup
                  "1/mu^2" = function(z) 1, # not setup
                  stop("Unrecognized link function")
                  )

    if(type == "response"){
        # Marginal Effect calculation (response scale for GLMs)
        out$Effect <- apply(out$Effect, 2, `*`, predict(x, newdata = as.data.frame(model.matrix(x)), type = "response"))
    }
    
    
    out
}

margins.polr <- function(x, newdata = NULL, 
         ...) {
    
}

margins.censReg <- function(x, newdata = NULL, 
         ...) {
    
}

print.margins <- function(x, digits = getOption('digits',4), ...){
    tab <- 
    cbind.data.frame('dy/dx' = colMeans(x$Effect), 
                     'Std.Err.' = sapply(x$Variance, sqrt))
    print(tab, digits = digits)
    invisible(x)
}

summary.margins <- function(x, ...){
    out <- data.frame(Factor = colnames(x$Effect), 
                      Effect = colMeans(x$Effect),
                      'Std.Err.' = sqrt(x$Variance),
                      row.names = 1:ncol(x$Effect))
    out
}

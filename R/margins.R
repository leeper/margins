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
function(x, ...){
    # setup objects
    mm <- as.data.frame(model.matrix(x)) # data
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
                           termorder[(i+1):length(termorder)])
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
    MEs <- attributes(with(mm, eval(deriv3(reformulate(f)[[2]], u))))$gradient
    
    # Variance calculation
    # without higher-order terms it is just:
    Variances <- diag(vc[utmp,utmp])
    if(any(termorder > 1))
        warning("Variance estimates are incorrect for variables included in higher-order terms")
    
    colnames(MEs) <- utmp
    structure(list(Effect = MEs,
                   Variance = Variances,
                   model = x), class = c("margins"))
}

margins <- 
function(x, atmeans = FALSE, ...) {
    UseMethod("margins")
}

margins.lm <- function(x, ...){
    margins_calculator(x, ...)
}

margins.glm <- 
function(x, 
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
        out$Effect <- apply(out$Effect, 2, `*`, predict(x, newdata = mm, type = "response"))
    }
    
    
    
}

margins.plm <- 
function(x, ...) {
    margins_calculator(x, ...)    
    # FOR SOME REASON THIS ISN'T CAPTURING INTERACTION TERMS
}


margins.polr <- function(x, ...) {
    
}

margins.censReg <- function(x, ...) {
    
}

print.margins <- function(x, ...){
    print(cbind.data.frame('dy/dx' = colMeans(x$Effect), 
                           'Std.Err.' = sqrt(x$Variance)))
    invisible(x)
}

summary.margins <- function(x, ...){
    out <- data.frame(Factor = colnames(x$Effect), 
                      Effect = colMeans(x$Effect), 
                      row.names = 1:ncol(x$Effect))
    out
}

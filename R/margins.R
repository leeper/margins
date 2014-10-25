margins <- 
function(x, atmeans = FALSE, ...) {
    UseMethod("margins")
}

print.margins <-
function(x, intercept = FALSE, ...){
    out <- data.frame(Factor = x$Factor, 
                      Effect = x$MarginalEffects, 
                      row.names = seq_along(x$Factor))
    #out[["Std. Error"]] <- something something something
    #out[["t.statistic"]] <- out[["Effect"]]/out[["Std. Error"]]
    #out[["Pr(>|z|)"]] <- something something something
    
    if(intercept)
        print(out)
    else
        print.data.frame(out[!out[["Factor"]] == "(Intercept)",])
}

margins.lm <- 
function(x, 
         at = NULL, # should probably just be one value per variable
                    # Stata's implementation is quite complex
         atmeans = FALSE,
         ...){
    tl <- attributes(terms(x))$term.labels
    mm <- x$model
    est <- coef(x)

    if(atmeans) {
        # replace data with means of data
        tmp <- as.list(colMeans(mm))
        # insert `at` values
        if(!is.null(at)){
            if(any(! names(at) %in% names(mm)))
                stop("Unrecognized variable name in 'at'")
            # expand possible combinations of `at` values
            tmp <- expand.grid(append(tmp, at))
        }
        # replace `mm` with updated data
        mm <- as.data.frame(tmp)
        rm(tmp)
    } else {
        if(!is.null(at)){
            if(any(! names(at) %in% names(mm)))
                stop("Unrecognized variable name in 'at'")
            # overwrite `at` values
            mm[ , names(at)] <- at
        }
    }

    # function to cleanup I(), etc. in formulas
    gsub_bracket <- function(a, b) {
        tmp <- regmatches(a, gregexpr(paste0("(",b,"\\().+(\\))"), a))
        regmatches(a, gregexpr(paste0("(",b,"\\().+(\\))"), a)) <- 
          gsub(")$","", gsub(paste0("^",b,"\\("), "", tmp))
        a
    }
    tl <- gsub_bracket(tl,"I")
    names(mm) <- gsub_bracket(names(mm), "I")
    
    # make interaction terms derivable by replacing `:` with `*`
    f <- gsub(":", "*", paste(est[-1], tl, sep="*", collapse=" + "))

    # find unique, first-order terms
    tmpnames <- unique(tl[attributes(terms(x))$order == 1])
    tmpnames <- gsub("^[:digit:]+[\\^\\+\\-\\*\\/]", "", tmpnames)
    tmpnames <- gsub("[\\^\\+-\\*/][[:digit:]+]$", "", tmpnames)
    # need to remove mathematical expressions
    # but that's complicated because it requires undoing the calculation of those expressions in the model.matrix
    #exprs <- c("exp", "log", "sin", "cos", "tan", "sinh", "cosh", "sqrt", "pnorm", "dnorm", "asin", "acos", "atan", "gamma", "lgamma", "digamma", "trigamma")
    #for(i in seq_along(exprs)){
    #    tmpnames <- gsub_bracket(tmpnames, exprs[i])
    #}
    u <- unique(tmpnames)

    # effect calculation
    # need to force this to return a vector result even for first-order lm's
    MEs <- lapply(u, function(z) {
        d <- D(reformulate(f)[[2]], z)
        with(mm, eval(d))
    })
    # variance calculation
    #Var <- 
    
    # format output
    out <- list()
    out$Factor <- u
    out$MarginalEffects <- sapply(MEs, mean)
    out$Effect <- MEs
    out$Variance <- NULL
    return(structure(out, class = c("margins")))
}

margins.glm <- 
function(x, 
         at = NULL, 
         atmeans = FALSE, 
         ...) {
    # link function
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
    b.est <- coef(x)
    if(atmeans) {
        tmp <- as.list(colMeans(mm))
        if(!is.null(at)){
            if(any(! names(at) %in% names(mm)))
                stop("Unrecognized variable name in 'at'")
            tmp <- expand.grid(append(tmp, list(a = 1:2)))
        }
        mm <- as.data.frame(tmp)
    } else {
        if(!is.null(at)){
            if(any(! names(at) %in% names(mm)))
                stop("Unrecognized variable name in 'at'")
            mm[,names(at)] <- at
        }
    }
    mm <- model.matrix(x) # automatically generates interactions & factors
    if(any(attributes(terms(x))$order > 1))
        warning("Interactions not currently handled correctly")
    e <- mean(dfun(as.matrix(mm) %*% b.est)) * b.est
    # drop non-unique I() terms
    anyI <- grepl("I\\(.+\\)", names(e))
    if(anyI) {
        tmpnames <- gsub("[(I\\()(\\))]", "", names(e))
        tmpnames <- gsub("^[:digit:]+[\\^\\+\\-\\*\\/]", "", tmpnames)
        tmpnames <- gsub("[^\\+-\\*/][[:digit:]+]$", "", tmpnames)
        e <- e[unique(tmpnames)]
    }
    
    out <- data.frame(Factor = names(e), 
                      Effect = e, 
                      row.names = seq_along(e))
    s <- sfun(t(as.matrix(colMeans(mm))) %*% b.est)
    dr <- (out$Effect/b.est) * ( diag(1, ncol(mm), ncol(mm)) + 
                                 c(s) * ( b.est %*% t(as.matrix(colMeans(mm))) )
                                 )
    variance <- dr %*% vcov(x) %*% t(dr)
    out <- list()
    out$Factor <- names(e)
    out$Effect <- e
    return(structure(out, class = c("margins")))
}

margins.polr <- function(x, ...) {
    
}

margins.censReg <- function(x, ...) {
    
}


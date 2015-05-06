.discretechange <- function(x, from, to, atmeans = FALSE) {
    # get rid of `atmeans` argument and pass arguments to aggregate
    dat1 <- .at_builder(x$model, at = from, atmeans = atmeans)
    dat2 <- .at_builder(x$model, at = to, atmeans = atmeans)
    change <- predict(x, newdata = dat2) - predict(x, newdata = dat2)
    return(change)
}

.margins2 <- function(x, at, atmeans = FALSE) {
    myfun <- function(z) {
        if(missing(at))
            dat <- .at_builder(x$model, atmeans = atmeans)
        else
            dat <- .at_builder(x$model, at = at, atmeans = atmeans)
        predict(x, newdata = dat)
    }
    d <- numDeriv::grad(myfun, 2)
    if(atmeans)
        return(d)
    else
        return(mean(d))
}



.margins <- 
function(x, mm, factors = "continuous", atmeans = FALSE, 
         predicted, dpredicted, ...){
    datmeans <- as.data.frame(t(colMeans(mm))) # data means
    tl <- names(mm)[names(mm) != "(Intercept)"] # terms
    est <- coef(x) # coefficients
    termorder <- attributes(terms(x))$order # term orders
    vc <- vcov(x) # var-cov matrix
    n <- nrow(mm)
    if(missing(predicted))
        predicted <- rep(1, n)
    if(missing(dpredicted))
        predicted <- rep(1, n)
    
    # fix I() variable names if they ARE NOT represented in first-order forms
    Iterms <- grepl("I\\(", tl)
    Iterms_pre <- tl[Iterms]
    # Iterms_vars = variable names from I() expressions
    Iterms_vars <- drop_operators(gsub_bracket(Iterms_pre,"I"), dropdigits = TRUE)
    Iw <- which(!Iterms_vars %in% tl[!Iterms])
    # THIS DOESN'T HANDLE I() EXPRESSIONS WITH MULTIPLE VARIABLE NAMES
    # THIS WOULD CAPTURE MULTIPLE VARIABLE NAMES, BUT THEN CODE NEEDS TO IDENTIFY
    # AND CHECK FOR PRESENCE OF ALL CONSTITUENT VARIABLES
    # sapply(parse(text=Iterms_vars), all.vars)
    if(any(sapply(parse(text=Iterms_vars), function(z) length(all.vars(z))>1))){
        warning("Marginal effects for variables included in\n a multi-variable I() expression do not include\n effect due to I() term")
        tmp <- gsub("(\\^|\\+|\\-|\\*|\\|/)", "", Iterms_pre)
        Iterms_post <- paste0("IVAR.", drop_operators(gsub_bracket(tmp,"I"), dropdigits = FALSE))
    } else {
        # Iterms_post = new variable names for I() expressions preserving information
        Iterms_post <- paste0("IVAR.", drop_operators(gsub_bracket(Iterms_pre,"I"), dropdigits = FALSE))
    }
    if(length(Iw)){
        tl[tl %in% Iterms_pre[Iw]] <- 
        names(est)[names(est) %in% Iterms_pre[Iw]] <- 
        colnames(mm)[colnames(mm) %in% Iterms_pre[Iw]] <- 
        colnames(vc)[colnames(vc) %in% Iterms_pre[Iw]] <- 
        rownames(vc)[rownames(vc) %in% Iterms_pre[Iw]] <- Iterms_post[Iw]
    }
    # fix I() variable names that ARE represented in first-order form
    tl <- gsub_bracket(tl,"I")
    names(mm) <- gsub_bracket(names(mm), "I") # I() terms
    colnames(vc) <- rownames(vc) <- gsub_bracket(colnames(vc), "I")
    
    anyfactors <- grepl("factor(", attributes(terms(x))$term.labels, fixed = TRUE)
    if(any(anyfactors)){
        # fix termorder values for factor variables
        for(i in which(anyfactors)){
            termorder <- c(termorder[0:(i-1)],
                           rep(termorder[i],nlevels(x$model[,attributes(terms(x))$term.labels[i]])-1),
                           tail(termorder, length(termorder) - i))
        }
        # fix factor() variable names
        Fterms <- grepl("factor\\(", tl)
        Fterms_pre <- tl[Fterms]
        # Fterms_vars = variable names from I() expressions
        Fterms_vars <- drop_operators(gsub_bracket(Fterms_pre,"factor"), dropdigits = TRUE)
        Fw <- which(!Fterms_vars %in% tl[!Fterms])
        # Fterms_post = new variable names for I() expressions preserving information
        Fterms_post <- paste0("FVAR.", drop_operators(gsub_bracket(Fterms_pre,"factor"), dropdigits = FALSE))
        if(length(Fw)){
            tl[tl %in% Fterms_pre[Fw]] <- 
            #names(datmeans)[names(datmeans) %in% Fterms_pre[Fw]] <- 
            names(est)[names(est) %in% Fterms_pre[Fw]] <- 
            colnames(mm)[colnames(mm) %in% Fterms_pre[Fw]] <- 
            colnames(vc)[colnames(vc) %in% Fterms_pre[Fw]] <- 
            rownames(vc)[rownames(vc) %in% Fterms_pre[Fw]] <- Fterms_post[Fw]
        }
    }
    
    # make interaction terms derivable by replacing `:` with `*`
    f <- gsub(":", "*", paste(est[names(est) != "(Intercept)"], tl, sep="*", collapse=" + "))
    
    # find unique, first-order terms
    u <- unique(drop_operators(unique(tl[termorder == 1])), dropdigits = TRUE)
    
    
    
    # Marginal Effect calculation (linear models)
    # g() = regression equation
    # f() = inverse link function
    # ME = (f(g(x)))' = f'(g(x)) * g'(x) # <- applying chain rule
    
    # Take partial derivative of regression equation with respect to each constituent variable
    # using symbolic derivative via `deriv3`
    d <- with(mm, eval(deriv3(reformulate(f)[[2]], u)))
    fprime <- attributes(d)$gradient
    # apply chain rule for glms by multiplying f'(g(x)) by g'(x) (which is saved in `predicted`)
    # in `lm`, or if `response = "link"`, `predicted` should be a vector of 1's
    MEs <- apply(fprime, 2, `*`, predicted)
    if(!is.matrix(MEs))
        MEs <- t(MEs)
    
    if(factors == "discrete"){
        # calculate first-differences for factor variables
        warning("Argument 'factors' is currently ignored. Partial effects rather than first-differences reported.")
    }
    
    
    # Variance calculation
    # Var(ME) = E %*% Var(\beta) %*% t(E), where E = derivatives of MEs with respect to each coefficient
    # Var(ME) = (f'(g(x)) * g'(x))' %*% Var(\beta) %*% t((f'(g(x)))')
    # where e'(f(g(x))) = e'(f'(g(x)) * g'(x)) = e'(f(g(x))) * f'(g(x)) * g'(x)
    betas <- setNames(tl, paste0('beta', seq_along(tl)))
    f2 <- reformulate(gsub(":", "*", paste(names(betas), 
                               gsub_bracket(tl, "factor"), sep="*", collapse=" + ")))[[2]]
    est2 <- setNames(est, c(names(est)[1], names(betas))) # play with names of coefficient vector
    # `grad` is matrix of partial derivatives of marginal effects
    grad <- t(sapply(u, function(this_var) {
        # this ME
        this_me <- D(f2, this_var)
        # take the partial derivatives of this marginal effect 
        #if(atmeans)
            with(datmeans, attributes(with(as.data.frame(t(est2)), eval(deriv(this_me, names(betas)))))$gradient)
        #else
        #    with(mm, attributes(with(as.data.frame(t(est2)), eval(deriv(this_me, names(betas)))))$gradient)
    }))
    # Apply chain rule
    #chain <- grad
    #chain <- grad * mean(predicted) * mean(dpredicted)
    chain <- grad * mean(predicted) # close
    #chain <- grad * mean(dpredicted)
    #chain <- apply(grad, 2, `*`, t(MEs))
    colnames(chain) <- betas
    # Apply delta method
    Variances <- diag(chain %*% vc[colnames(chain), colnames(chain)] %*% t(chain))

    # CLEANUP FOR OUTPUT
    # restore I() variable names (if they are represented in I() terms but not in their original forms)
    if(length(Iw)){
        u[u %in% Iterms_post[Iw]] <- 
        colnames(MEs)[colnames(MEs) %in% Iterms_post[Iw]] <- 
        names(Variances)[names(Variances) %in% Iterms_post[Iw]] <- Iterms_pre[Iw]
    }
    # restore factor() variable names
    if(any(anyfactors)){
        u[u %in% Fterms_post[Fw]] <- 
        colnames(MEs)[colnames(MEs) %in% Fterms_post[Fw]] <- 
        names(Variances)[names(Variances) %in% Fterms_post[Fw]] <- Fterms_pre[Fw]
    }
    
    colnames(MEs) <- u
    structure(list(Effect = MEs,
                   Variance = Variances,
                   model = x), class = c("margins"))
}

margins_calculator <- 
function(x, mm, factors = "continuous", ...){
    datmeans <- as.data.frame(t(colMeans(mm))) # data means
    tl <- names(mm)[names(mm) != "(Intercept)"] # terms
    est <- coef(x) # coefficients
    termorder <- attributes(terms(x))$order # term orders
    vc <- vcov(x) # var-cov matrix
    n <- nrow(mm)
    
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
            names(datmeans)[names(datmeans) %in% Fterms_pre[Fw]] <- 
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
    # do the calculation using the symbolic derivative
    d <- with(mm, eval(deriv3(reformulate(f)[[2]], u)))
    MEs <- attributes(d)$gradient
    
    # Variance calculation using the delta method
    betas <- setNames(tl, paste0('beta', seq_along(tl)))
    f2 <- reformulate(gsub(":", "*", paste(names(betas), 
                               gsub_bracket(tl, "factor"), sep="*", collapse=" + ")))[[2]]
    Variances <- setNames(sapply(u, function(z) {
        this_me <- D(f2, z) # ME
        a <- all.vars(this_me)
        a <- a[grepl("beta",a)] # only the coefs included in ME
        gradmat <- do.call(cbind, lapply(a, function(b) {
            #grad <- with(mm, eval(D(this_me, b))) # gradient evaluated at *data*
            grad <- with(datmeans, eval(D(this_me, b))) # gradient evaluated at *means*
        }))
        colnames(gradmat) <- betas[a]
        # estimate variance using delta method
        gradmat %*% vc[colnames(gradmat), colnames(gradmat)] %*% t(gradmat)
    }), u)
    
    if(factors == "discrete"){
        # calculate first-differences for factor variables    
    }
    
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

margins <- 
function(x, newdata = NULL, ...) {
    UseMethod("margins")
}

margins.lm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         factors = "continuous", ...){
    if(is.null(newdata)) {
        newdata <- eval(x$call$data)
        data_list <- at_builder(newdata, terms = x$terms, at = at, atmeans = atmeans)
    } else {
        data_list <- at_builder(newdata, terms = x$terms, at = at, atmeans = atmeans)
    }
    out <- lapply(data_list, margins_calculator, x = x, factors = factors, ...)
    class(out) <- "marginslist"
    out
}

margins.glm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         factors = "continuous",
         type = "link", # "link" (linear/xb); "response" (probability/etc. scale)
         ...){
    # configure link function
    g <- getlink(x$family$link)
    dfun <- g$dfun
    sfun <- g$sfun
    
    # calculate marginal effects
    if(is.null(newdata)) {
        newdata <- eval(x$call$data)
        data_list <- at_builder(newdata, terms = x$terms, at = at, atmeans = atmeans)
    } else {
        data_list <- at_builder(newdata, terms = x$terms, at = at, atmeans = atmeans)
    }
    out <- lapply(data_list, margins_calculator, x = x, factors = factors, ...)
    class(out) <- "marginslist"
    
    # handle margins types
    if(type == "response"){
        # Marginal Effect calculation (response scale for GLMs)
        out <- lapply(out, function(z) {
            z$Effect <- apply(z$Effect,2, `*`, dfun(predict(x, type = "link"))) # <- THIS RUNS PREDICTION ON ORIGINAL DATA
            z
        })
        # atmeans=TRUE
        #out$Effect <- out$Effect * as.numeric(dfun(colMeans(newdata) %*% coef(x)))
        
        # NEED TO MODIFY VARIANCES ???
        warning("Variances for marginal effects on response scale are incorrect")
        
    } else if(type == "link"){
        # Marginal Effect calculation (link scale for GLMs)
        # 
    } else {
        stop("Unrecognized value for 'type'")
    }
    out
}

margins.plm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         factors = "continuous", ...) {
    # calculate marginal effects
    if(is.null(newdata)) {
        newdata <- eval(x$call$data)
        data_list <- at_builder(newdata, terms = terms(x$formula), at = at, atmeans = atmeans)
    } else {
        data_list <- at_builder(newdata, terms = terms(x$formula), at = at, atmeans = atmeans)
    }
    
    # FOR SOME REASON THIS ISN'T CAPTURING INTERACTION TERMS
    if(x$args$model != 'pooling') {
        warning("marginal effects not likely to be correct")
        out <- lapply(data_list, margins_calculator, x = x, factors = factors, ...)
        class(out) <- "marginslist"
        #mm <- cbind(model.matrix(x), model.matrix(~0+attributes(x$model)$index[,1]))
    } else {
        out <- lapply(data_list, margins_calculator, x = x, factors = factors, ...)
        class(out) <- "marginslist"
    }
}

margins.pglm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         factors = "continuous", 
         type = "link", # "link" (linear/xb); "response" (probability scale)
         ...){
    # configure link function
    g <- getlink(x$family$link)
    dfun <- g$dfun
    sfun <- g$sfun
    
    if(is.null(newdata)) {
        newdata <- eval(x$call$data)
        data_list <- at_builder(newdata, terms = terms(x$formula), at = at, atmeans = atmeans)
    } else {
        data_list <- at_builder(newdata, terms = terms(x$formula), at = at, atmeans = atmeans)
    }
    out <- margins_calculator(x, mm = newdata, factors = factors, ...)

    if(type == "response"){
        # Marginal Effect calculation (response scale for GLMs)
        out$Effect <- apply(out$Effect, 2, `*`, predict(x, newdata = newdata, type = "response"))
    }
    
    
    out
}

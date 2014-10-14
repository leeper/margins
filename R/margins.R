margins <- 
function(x, atmeans = FALSE, ...) {
    UseMethod("margins")
}

print.margins <-
function(x, intercept = FALSE, ...){
    out <- data.frame(Factor = names(x$Effect), 
                      Effect = x$Effect, 
                      row.names = seq_along(x$Effect))
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
    b.est <- coef(x)
    if(atmeans) {
        # replace data with means of data
        tmp <- as.list(colMeans(x$model))
        # insert `at` values
        if(!is.null(at)){
            if(any(! names(at) %in% names(x$model)))
                stop("Unrecognized variable name in 'at'")
            # expand possible combinations of `at` values
            tmp <- expand.grid(append(tmp, at))
        }
        # replace `x$model` with updated data
        x$model <- as.data.frame(tmp)
        rm(tmp)
    } else {
        if(!is.null(at)){
            if(any(! names(at) %in% names(x$model)))
                stop("Unrecognized variable name in 'at'")
            # overwrite `at` values
            x$model[ , names(at)] <- at
        }
    }
    # retrieve data from model matrix
    mm <- model.matrix(x) # automatically generates interactions, factors, and I() variables
    # first-order, second-order, etc. interactions
    ord <- attributes(terms(x))$order
    # drop intercept
    f <- attributes(terms(x))$factors[-1,]
    # drop non-unique I() terms from list of marginal effects to calculate
    # THIS IS IMPERFECT BECAUSE IT WON'T CAPTURE NAIVE INTERACTIONS I(var1*var1) OR ANYTHING SIMILARLY OR MORE COMPLICATED
    if(any(grepl("I\\(.+\\)", rownames(f)))) {
        tmpnames <- gsub("[(I\\()(\\))]", "", rownames(f))
        tmpnames <- gsub("^[:digit:]+[\\^\\+\\-\\*\\/]", "", tmpnames)
        tmpnames <- gsub("[^\\+-\\*/][[:digit:]+]$", "", tmpnames)
        f <- f[unique(tmpnames),]
    }
    
    # store observation-level MEs
    MEs <- setNames(data.frame(matrix(NA, nrow(mm), ncol(mm[ , -1, drop=FALSE][, ord == 1, drop = FALSE]))), 
                    names(coef(x)[-1][ord == 1]))
    
    # first-order effects
    MEs[,names(coef(x)[-1][ord == 1])] <- as.list(coef(x)[-1][ord == 1])
    #e <- rowSums(f[, ord == 1, drop = FALSE] * coef(x)[-1][ord == 1])
    
    # VARIANCE CALCULATION (ONLY WORKS IF NO INTERACTION, ETC. TERMS)
    #s <- t(as.matrix(colMeans(mm))) %*% b.est
    #dr <- (e/b.est) * (diag(1, ncol(mm), ncol(mm)) + c(s) * (b.est %*% t(as.matrix(colMeans(mm)))))
    #variance <- dr %*% vcov(x) %*% t(dr)
    #variance <- matrix(NA, nrow=nrow(e), ncol=nrow(e))
    
    # handle higher-order terms (interactions)
    if(any(ord > 1)) {
        # insert interaction coefficients into `i` matrix
        i <- f[, ord > 1, drop = FALSE] * coef(x)[-1][ord > 1]
        if(nrow(i) > 0){
            # for every constituent term, update ME based on effects from complex terms
            for(j in 1:nrow(i)){
                # if this constituent term is in the complex term, proceed
                g <- grep(rownames(i)[j], colnames(i))
                if(length(g)){
                    # `itmp` is the matrix of complex terms
                    itmp <- i[ , g, drop = FALSE]
                    # for each complex term, add effects to first-order effects
                    sapply(colnames(itmp), function(k){
                        # find constituent terms
                        s <- setdiff(strsplit(k, ":")[[1]], rownames(i)[j])
                        # NEED SOMETHING HERE TO SKIP I() TERMS
                        # multiply out complex terms
                        v <- x$model[, s, drop = FALSE]
                        v <- if(ncol(v)>1) do.call(`*`, v) else v[,1]
                        # store effect results
                        MEs[,rownames(i)[j]] <<- MEs[,rownames(i)[j]] + (v * b.est[k])
                        #e[rownames(i)[j]] <<- e[rownames(i)[j]] + mean(v * b.est[k])
                        
                        # THIS IS WHERE VARIANCE NEEDS TO BE RECALCULATED
                    })
                }
                # NEED TO DO UPDATED VARIANCE CALCULATIONS
                # reference: https://files.nyu.edu/mrg217/public/interaction.html
                # THIS WILL BE TRICKY WITH I() TERMS
            }
        }
        #variance <- matrix(NA, nrow=nrow(e), ncol=nrow(e))
    } 
    
    # format output
    out <- list()
    out$Factor <- names(e)
    out$Effect <- colMeans(MEs)
    out$MarginalEffects <- MEs
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
        tmp <- as.list(colMeans(x$model))
        if(!is.null(at)){
            if(any(! names(at) %in% names(x$model)))
                stop("Unrecognized variable name in 'at'")
            tmp <- expand.grid(append(tmp, list(a = 1:2)))
        }
        x$model <- as.data.frame(tmp)
    } else {
        if(!is.null(at)){
            if(any(! names(at) %in% names(x$model)))
                stop("Unrecognized variable name in 'at'")
            x$model[,names(at)] <- at
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


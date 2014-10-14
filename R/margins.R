margins <- 
function(x, atmeans = FALSE, ...) {
    UseMethod("margins")
}

print.margins <-
function(x, intercept = FALSE, ...){
    if(intercept)
        print.data.frame(x)
    else
        print.data.frame(x[!x[["Factor"]] == "(Intercept)",])
}

margins.lm <- 
function(x, 
         at = NULL, 
         atmeans = FALSE,
         ...){
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
    mm <- model.matrix(x) # automatically generates interactions, factors, and I() variables
    ord <- attributes(terms(x))$order
    f <- attributes(terms(x))$factors[-1,]
    # drop non-unique I() terms
    if(any(grepl("I\\(.+\\)", rownames(f)))) {
        tmpnames <- gsub("[(I\\()(\\))]", "", rownames(f))
        tmpnames <- gsub("^[:digit:]+[\\^\\+\\-\\*\\/]", "", tmpnames)
        tmpnames <- gsub("[^\\+-\\*/][[:digit:]+]$", "", tmpnames)
        f <- f[unique(tmpnames),]
    }
    
    # first-order effects
    e <- rowSums(f[, ord == 1] * coef(x)[-1][ord == 1])
    # VARAINCE CALCULATION (ONLY WORKS IF NO INTERACTION, ETC. TERMS)
    s <- t(as.matrix(colMeans(mm))) %*% b.est
    dr <- (e/b.est) * (diag(1, ncol(mm), ncol(mm)) + c(s) * (b.est %*% t(as.matrix(colMeans(mm)))))
    variance <- dr %*% vcov(x) %*% t(dr)
    
    # Interaction terms
    if(any(ord > 1)) {
        i <- f[, ord > 1, drop = FALSE] * coef(x)[-1][ord > 1]
        if(nrow(i) > 0){
            for(j in 1:nrow(i)){
                g <- grep(rownames(i)[j], colnames(i))
                if(length(g)){
                    itmp <- i[,g, drop = FALSE]
                    sapply(colnames(itmp), function(k){
                        s <- setdiff(strsplit(k, ":")[[1]], rownames(i)[j])
                        # MAY NEED SOMETHING HERE TO SKIP I() TERMS
                        v <- x$model[, s, drop = FALSE]
                        v <- if(ncol(v)>1) do.call(`*`, v) else v[,1]
                        e[rownames(i)[j]] <<- e[rownames(i)[j]] + mean(v * b.est[k])
                    })
                }
                # NEED TO DO UPDATED VARIANCE CALCULATIONS
                # reference: https://files.nyu.edu/mrg217/public/interaction.html
                # THIS WILL BE TRICKY WITH I() TERMS
            }
        }
        variance <- matrix(NA, nrow=f, ncol=f)
    } 
    
    # format output
    out <- data.frame(Factor = names(e), 
                      Effect = e, 
                      row.names = seq_along(e))
    #out[["Std. Error"]] <- sqrt(diag(variance))
    #out[["t.statistic"]] <- out[["Effect"]]/out[["Std. Error"]]
    #out[["Pr(>|z|)"]] <- 2 * (1 - pt(abs(out[["t.statistic"]]), nrow(mm) - ncol(mm)))
    return(structure(out, class = c("margins", "data.frame")))
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
    #out[["Std. Error"]] <- sqrt(diag(variance))
    #out[["t.statistic"]] <- out[["Effect"]]/out[["Std. Error"]]
    #out[["Pr(>|z|)"]] <- 2 * (1 - pt(abs(out[["t.statistic"]]), nrow(mm) - ncol(mm)))
    return(structure(out, class = c("margins", "data.frame")))
}

margins.polr <- function(x, ...) {
    
}

margins.censReg <- function(x, ...) {
    
}


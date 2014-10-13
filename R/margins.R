margins <- 
function(x, atmeans = FALSE, ...) {
    UseMethod("margins")
}

margins.lm <- 
function(x, atmeans = FALSE, ...){
    b.est <- coef(x)
    mm <- model.matrix(x)
    if(atmeans) {
        out <- data.frame(Effects = b.est, row.names = names(b.est))
    } else {
        out <- data.frame(Effects = b.est, row.names = names(b.est))
    }
    s <- xb
    dr <- (out$Effects/b.est) * (diag(1, ncol(mm), ncol(mm)) + c(s) * (b.est %*% t(as.matrix(colMeans(mm)))))
    variance <- dr %*% vcov(x) %*% t(dr)
    out[["Std. Error"]] <- sqrt(diag(variance))
    out[["t.statistic"]] <- out[["Effects"]]/out[["Std. Error"]]
    out[["Pr(>|z|)"]] <- 2 * (1 - pt(abs(out[["t.statistic"]]), nrow(mm) - ncol(mm)))
    return(structure(out, class = c("margins", "data.frame")))
}

margins.glm <- 
function(x, atmeans = FALSE, at = NULL, ...) {
    b.est <- coef(x)
    if(!is.null(at)){
        x$model[,names(at)] <- at
    }
    mm <- model.matrix(x)
    xb <- t(as.matrix(colMeans(mm))) %*% b.est
    
    famlink <- paste0(x$family$family, x$family$link)
    dfun <- switch(famlink, 
                  binomialprobit = dnorm, 
                  binomiallogit = dlogis,
                  binomialcauchit = dcauchy,
                  binomiallog = exp,
                  binomialcloglog = function(z) 1 - exp(-exp(z)),
                  Gammainverse = function(z) 1/z,
                  Gammaidentity = function(z) 1,
                  Gammalog = exp,
                  poissonlog = exp,
                  poissonidentity = function(z) 1,
                  poissonsqrt = function(z) z^2,
                  gaussianidentity = function(z) 1,
                  gaussianlog = exp,
                  gaussianinverse = function(z) 1/z,
                  "inverse.gaussian1/mu^2" = NULL,
                  inverse.gaussianinverse = function(z) -1/z,
                  inverse.gaussianidentity = function(z) 1,
                  inverse.gaussianlog = exp,
                  quasilogit = dlogis,
                  quasiprobit = dnorm,
                  quasicloglog = NULL,
                  quasiidentity = function(z) 1,
                  quasiinverse = function(z) 1/z,
                  quasilog = exp,
                  "quasi1/mu^2" = NULL,
                  quasisqrt = function(z) z^2,
                  stop("Unrecognized model family or link function")
                  )
    sfun <- switch(famlink, 
                  binomialprobit = function(z) -z, 
                  binomiallogit = function(z) 1 - 2 * plogis(z),
                  binomialcauchit = NULL,
                  binomiallog = NULL,
                  binomialcloglog = NULL,
                  Gammainverse = NULL,
                  Gammaidentity = NULL,
                  Gammalog = NULL,
                  poissonlog = NULL,
                  poissonidentity = NULL,
                  poissonsqrt = NULL,
                  gaussianidentity = function(z) z,
                  gaussianlog = NULL,
                  gaussianinverse = NULL,
                  "inverse.gaussian1/mu^2" = NULL,
                  inverse.gaussianinverse = NULL,
                  inverse.gaussianidentity = NULL,
                  inverse.gaussianlog = NULL,
                  quasilogit = NULL,
                  quasiprobit = NULL,
                  quasicloglog = NULL,
                  quasiidentity = NULL,
                  quasiinverse = NULL,
                  quasilog = NULL,
                  "quasi1/mu^2" = NULL,
                  quasisqrt = NULL,
                  stop("Unrecognized model family or link function")
                  )
    if(atmeans) {
        out <- data.frame(Effects = dfun(t(as.matrix(colMeans(mm))) %*% b.est) * b.est, row.names = names(b.est))
    } else {
        out <- data.frame(Effects = mean(dfun(as.matrix(mm) %*% b.est)) * b.est, row.names = names(b.est))
    }
    s <- sfun(xb)
    dr <- (out$Effects/b.est) * (diag(1, ncol(mm), ncol(mm)) + c(s) * (b.est %*% t(as.matrix(colMeans(mm)))))
    variance <- dr %*% vcov(x) %*% t(dr)
    out[["Std. Error"]] <- sqrt(diag(variance))
    out[["t.statistic"]] <- out[["Effects"]]/out[["Std. Error"]]
    out[["Pr(>|z|)"]] <- 2 * (1 - pt(abs(out[["t.statistic"]]), nrow(mm) - ncol(mm)))
    return(structure(out, class = c("margins", "data.frame")))
}

margins.polr <- function(x, ...) {
    
}

margins.censReg <- function(x, ...) {
    
}


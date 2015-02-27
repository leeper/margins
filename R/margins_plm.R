margins.plm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         factors = "continuous", ...) {
    # calculate marginal effects
    if(is.null(newdata)) {
        newdata <- if(!is.null(x$call$data)) eval(x$call$data) else x$model
    }
    data_list <- at_builder(newdata, terms = terms(x$formula), at = at, atmeans = atmeans)
    
    # FOR SOME REASON THIS ISN'T CAPTURING INTERACTION TERMS
    if(x$args$model != 'pooling') {
        warning("marginal effects not likely to be correct")
        out <- lapply(data_list, .margins, x = x, factors = factors, ...)
        class(out) <- "marginslist"
        #mm <- cbind(model.matrix(x), model.matrix(~0+attributes(x$model)$index[,1]))
    } else {
        out <- lapply(data_list, .margins, x = x, factors = factors, ...)
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
    
    if(is.null(newdata)) {
        newdata <- if(!is.null(x$call$data)) eval(x$call$data) else x$model
    }
    data_list <- at_builder(newdata, terms = terms(x$formula), at = at, atmeans = atmeans)
    out <- .margins(x, mm = newdata, factors = factors, ...)

    if(type == "response"){
        # Marginal Effect calculation (response scale for GLMs)
        out$Effect <- apply(out$Effect, 2, `*`, predict(x, newdata = newdata, type = "response"))
    }
    
    
    out
}

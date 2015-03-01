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
    
    # calculate marginal effects
    if(is.null(newdata)) {
        newdata <- if(!is.null(x$call$data)) eval(x$call$data) else x$model
    }
    data_list <- at_builder(newdata, terms = x$terms, at = at, atmeans = atmeans)
    if(type == "link") {
        out <- lapply(data_list, function(z) {
            m <- .margins(x = x, mm = z$mm, factors = factors, atmeans = atmeans, 
                          predicted = rep(1, nrow(z$mm)), dpredicted = rep(1, nrow(z$mm)), ...)
            attr(m, "Variables") <- attributes(z)$Variables
            m
        })
    } else if (type == "response") {
        out <- lapply(data_list, function(z) {
            # predicted values: ME = f'(g(x)) = f'(g(x)) * g'(x)
            predicted <- dfun(predict(x, newdata = z$data, type = "link"))
            # Var(ME) = (f'(g(x)))' %*% Var(\beta) %*% t((f'(g(x)))')
            dpredicted <- numDeriv::grad(dfun, predict(x, newdata = z$data, type = "response"))
            m <- .margins(x = x, mm = z$mm, factors = factors, atmeans = atmeans, 
                          predicted = predicted, 
                          dpredicted = dpredicted, ...)
            attr(m, "Variables") <- attributes(z)$Variables
            m
        })
        warning("Variances for marginal effects on response scale are incorrect")
    } else {
        stop("Unrecognized value for 'type'")
    }
    class(out) <- "marginslist"
    out
}

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
            .margins(x = x, mm = z$mm, factors = factors, atmeans = atmeans, dpred = rep(1, nrow(z)), ...)
        })
    } else if (type == "response") {
        out <- lapply(data_list, function(z) {
            .margins(x = x, mm = z$mm, factors = factors, atmeans = atmeans, dpred = dfun(predict(x, newdata = z$data, type = "link")), ...)
        })
        warning("Variances for marginal effects on response scale are incorrect")
    } else {
        stop("Unrecognized value for 'type'")
    }
    class(out) <- "marginslist"
    out
}

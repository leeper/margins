#' @export
margins.glm <- 
function(x, 
         newdata, 
         at = NULL, 
         atmeans = FALSE, 
         ...){
    # configure link derivative function
    g <- getlink(x$family$link)
    
    # setup data
    if (missing(newdata)) {
        newdata <- if(!is.null(x$call$data)) eval(x$call$data) else x$model
    }
    data_list <- at_builder(newdata, terms = x$terms, at = at, atmeans = atmeans)
    
    # calculate marginal effects
    out <- lapply(data_list, function(thisdata) {
        m <- .margins(x = x, data = thisdata, atmeans = atmeans, ...)
        attr(m, "Variables") <- attributes(z)$Variables
        m
    })
    
    if (type == "response") {
        warning("Variances for marginal effects on response scale are incorrect")
    }
    
    structure(out, class = "marginslist")
}

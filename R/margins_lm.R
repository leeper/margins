margins.lm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         factors = "continuous", ...){
    if(is.null(newdata)) {
        newdata <- if(!is.null(x$call$data)) eval(x$call$data) else x$model
    }
    data_list <- at_builder(newdata, terms = x$terms, at = at, atmeans = atmeans)
    out <- lapply(data_list, function(z) {
              .margins(x = x, mm = z$mm, factors = factors, atmeans = atmeans, 
                       predicted = rep(1, nrow(z$mm)), 
                       dpredicted = rep(1, nrow(z$mm)), ...)
           })
    class(out) <- "marginslist"
    out
}

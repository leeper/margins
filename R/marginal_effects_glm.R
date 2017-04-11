#' @rdname marginal_effects
#' @export
marginal_effects.glm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         type = c("response", "link"), 
         eps = 1e-7, 
         ...) {
    
    type <- match.arg(type)
    
    # identify classes of terms in `model`
    vars <- find_terms_in_model(model)
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    # add discrete differences for logical terms
    out1 <- lapply(c(vars$nnames, vars$lnames), dydx, data = data, model = model, type = type, eps = eps, ...)
    
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    out2 <- list()
    for (i in seq_along(vars$fnames)) {
        out2[[i]] <- dydx.factor(data = data, model = model, vars$fnames[i], type = type, fwrap = (vars$fnames != vars$fnames2)[i], ...)
    }
    
    out <- c(out1, out2)
    out <- do.call("cbind.data.frame", out[vapply(out, function(x) length(x) > 0, FUN.VALUE = logical(1))])
    return(out)
}

#' @rdname marginal_effects
#' @importFrom prediction find_data
#' @export
marginal_effects.nnet <- 
function(model, 
         data = find_data(model, parent.frame()), 
         eps = 1e-7, 
         ...) {
    
    # identify classes of terms in `model`
    vars <- find_terms_in_model(model)
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    # add discrete differences for logical terms
    out1 <- lapply(c(vars$nnames, vars$lnames), dydx, data = data, model = model, type = NULL, eps = eps, ...)
    
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    out2 <- list()
    for (i in seq_along(vars$fnames)) {
        out2[[i]] <- dydx.factor(data = data, model = model, vars$fnames[i], fwrap = (vars$fnames != vars$fnames2)[i], type = NULL, ...)
    }
    
    out <- c(out1, out2)
    out <- do.call("cbind.data.frame", out[vapply(out, function(x) length(x) > 0, FUN.VALUE = logical(1))])
    return(out)
}

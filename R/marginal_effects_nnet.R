#' @rdname marginal_effects
#' @importFrom prediction find_data
#' @export
marginal_effects.nnet <- 
function(model, 
         data = find_data(model, parent.frame()), 
         variables = NULL,
         eps = 1e-7, 
         ...) {
    
    # identify classes of terms in `model`
    vars <- find_terms_in_model(model)

    # subset of variables for which to compute the marginal effects
    if (!is.null(variables)) {
        tmp <- c(vars$nnames, vars$lnames, vars$fnames, vars$fnames2)
        if (all(variables %in% tmp)) {
            vars$nnames <- vars$nnames[vars$nnames %in% variables]
            vars$lnames <- vars$lnames[vars$lnames %in% variables]
            vars$fnames <- vars$fnames[vars$fnames %in% variables]
            vars$fnames2 <- vars$fnames2[vars$fnames2 %in% variables]
        } else {
            stop('Some values in `variables` are not in the model terms.')
        }
    }
    
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

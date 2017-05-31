#' @rdname marginal_effects
#' @importFrom prediction find_data
#' @export
marginal_effects.merMod <- 
function(model, 
         data = find_data(model), 
         variables = NULL,
         type = c("response", "link"), 
         eps = 1e-7, 
         ...) {
    
    type <- match.arg(type)
    
    # extract term names from `model`
    term_names <- all.vars(terms(model))
    
    # identify classes of terms in `model`
    nnames <- colnames(attributes(terms(model))[["factors"]])
    lnames <- NULL
    fnames <- NULL
    classes <- rep("numeric", length(nnames))
    warning("factor variables are not handled as factor for models of class 'merMod'")

    # subset of variables for which to compute the marginal effects
    if (!is.null(variables)) {
        if (any(!variables %in% nnames)) {
            stop("Some values in 'variables' are not in the model terms.")
        }
        nnames <- nnames[nnames %in% variables]
    }
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    # add discrete differences for logical terms
    out1 <- lapply(c(nnames, lnames), dydx, data = data, model = model, type = type, eps = eps, ...)
    
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    out2 <- list()
    for (i in seq_along(fnames)) {
        out2[[i]] <- dydx.factor(data = data, model = model, fnames[i], type = type, fwrap = FALSE, ...)
    }
    
    out <- c(out1, out2)
    out <- do.call("cbind.data.frame", out[vapply(out, function(x) length(x) > 0, FUN.VALUE = logical(1))])
    return(out)
}

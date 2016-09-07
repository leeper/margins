get_instant_pdiff <- function(data, model, variable, type = c("response", "link"), eps = 1e-4) {
    # @title Instantaneous change in fitted values (numerical derivative)
    # @description This is an internal function used to calculate instantaneous change (numerical derivative) in y-hat between observed values in `data` and the smallest machine-precise change in the value of `data`. This is used by \code{marginal_effects} for numeric variables. It currently only uses the "simple" derivative method. This might change in the future
    # @param data The dataset on which to to calculate `predict(model)` (and the slope thereof)
    # @param model The model object to pass to `predict()`
    # @param variable A character string specifying the variable to calculate the difference for
    # @param type The type of prediction. Default is \dQuote{response}.
    
    type <- match.arg(type)
    
    # calculate numerical derivative
    d <- data
    P0 <- prediction(model = model, data = d, type = type)[["fitted"]]
    if (is.numeric(d[[variable]])) {
        d[[variable]] <- d[[variable]] + eps
    } else {
        warning(paste0("Class of variable, ", variable, ", is unrecognized. Returning NA."))
        return(rep(NA, nrow(data)))
    }
    P1 <- prediction(model = model, data = d, type = type)[["fitted"]]
    out <- ( P1 - P0) / eps
    
    return(out)
}

get_factor_pdiff <- function(data, model, variable, type = c("response", "link"), fwrap = FALSE) {
    # @title Discrete change in fitted values
    # @description This is an internal function used to calculate discrete change in y-hat between factor levels and base factor level. This is used by \code{marginal_effects} for factor variables.
    # @param data The dataset on which to to calculate `predict(model)` (and the slope thereof)
    # @param model The model object to pass to `predict()`
    # @param variable A character string specifying the variable to calculate the difference for
    # @param type The type of prediction. Default is \dQuote{response}.
    # @param fwrap A logical specifying how to name factor columns in the response.
    
    type <- match.arg(type)
    
    levs <- levels(as.factor(data[[variable]]))
    base <- levs[1]
    levs <- levs[-1]
    
    # setup response object
    if (isTRUE(fwrap)) {
        outcolnames <- paste0("factor(", variable, ")", levs)
    } else {
        outcolnames <- paste0(variable, levs)
    }
    out <- structure(rep(list(list()), length(levs)), 
                     class = "data.frame", 
                     names = outcolnames, 
                     row.names = seq_len(nrow(data)))
    
    # setup base data and prediction
    if (is.factor(data[[variable]])) {
        D0 <- build_datalist(data, at = setNames(list(base), variable))[[1]]
    } else if (is.logical(data[[variable]])) {
        D0 <- build_datalist(data, at = setNames(list(as.logical(base)), variable))[[1]]
    }
    # setup functions through predict_factory
    P0 <- prediction(model = model, data = D0, type = type)[["fitted"]]
    
    # calculate difference for each factor level
    for (i in seq_along(levs)) {
        if (is.factor(data[[variable]])) {
            D <- build_datalist(data, at = setNames(list(levs[i]), variable))[[1]]
        } else if (is.logical(data[[variable]])) {
            D <- build_datalist(data, at = setNames(list(as.logical(levs[i])), variable))[[1]]
        }
        P1 <- prediction(model = model, data = D, type = type)[["fitted"]]
        out[[outcolnames[i]]] <- P1 - P0
    }
    # return vector of differences
    return(out)
}

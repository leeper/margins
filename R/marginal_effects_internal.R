#' @rdname mfx
#' @title Marginal Effect of a Given Variable
#' @description Differentiate an Estimated Model with Respect to One Variable
#' @param data The dataset on which to to calculate \eqn{\hat{y}}.
#' @param model The model object to pass to \code{\link{prediction}}.
#' @param variable A character string specifying the variable to calculate the derivative for.
#' @param type The type of prediction. Default is \dQuote{response}.
#' @param eps The value of the step \eqn{\epsilon} to use in calculation of the numerical derivative.
#' @param fwrap A logical specifying how to name factor columns in the response.
#' @details
#' These functions provide a simple interface to the calculation of marginal effects for specific variables used in a model, and are the workhorse functions called internally by \code{\link{marginal_effects}}.
#' 
#' \code{mfx} is an S3 generic with classes implemented for specific variable types. The method for numeric variables uses one-sided numerical differentiation (\deqn{h = \max(|x|, 1) \sqrt{\epsilon}}{h = max(|x|, 1)sqrt(epsilon)}, where \eqn{\epsilon}{epsilon} is given by \code{eps}) to extract the marginal effect, or instantaneous change (numerical derivative) in \eqn{\hat{y}}. It currently only uses a \dQuote{simple}, one-sided derivative method (in the language of the numDeriv package) . This might change in the future.
#' 
#' For factor variables (or character variables, which are implicitly coerced to factors by modelling functions) and logical variables, discrete differences in predicted outcomes are reported instead (i.e., change in predicted outcome when factor is set to a given level minus the predicted outcome when the factor is set to its baseline level). If you want to use numerical differentiation for factor variables (which you probably do not want to do), enter them into the original modelling function as numeric values rather than factors.
#' 
#' @return A data.frame, typically with one column unless the variable is a factor with more than two levels.
#' @seealso \code{\link{marginal_effects}}, \code{\link{margins}}
#' 
#' @export
mfx_numeric <- function(data, model, variable, type = c("response", "link"), eps = 1e-4) {
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

#' @rdname mfx
#' @export
mfx_factor <- function(data, model, variable, type = c("response", "link"), fwrap = FALSE) {
    
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
    if (is.logical(data[[variable]])) {
        D0 <- build_datalist(data, at = setNames(list(as.logical(base)), variable))[[1]]
    } else {
        D0 <- build_datalist(data, at = setNames(list(base), variable))[[1]]
    } 
    # setup functions through predict_factory
    P0 <- prediction(model = model, data = D0, type = type)[["fitted"]]
    
    # calculate difference for each factor level
    for (i in seq_along(levs)) {
        if (is.logical(data[[variable]])) {
            D <- build_datalist(data, at = setNames(list(as.logical(levs[i])), variable))[[1]]
        } else {
            D <- build_datalist(data, at = setNames(list(levs[i]), variable))[[1]]
        }
        P1 <- prediction(model = model, data = D, type = type)[["fitted"]]
        out[[outcolnames[i]]] <- P1 - P0
    }
    # return vector of differences
    return(out)
}

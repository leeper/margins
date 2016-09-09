#' @rdname mfx
#' @title Marginal Effect of a Given Variable
#' @description Differentiate an Estimated Model with Respect to One Variable
#' @param data The dataset on which to to calculate \eqn{\hat{y}}.
#' @param model The model object to pass to \code{\link{prediction}}.
#' @param variable A character string specifying the variable to calculate the derivative for.
#' @param type The type of prediction. Default is \dQuote{response}.
#' @param eps The value of the step \eqn{\epsilon} to use in calculation of the numerical derivative.
#' @param fwrap A logical specifying how to name factor columns in the response.
#' @param \dots Ignored.
#' @details
#' These functions provide a simple interface to the calculation of marginal effects for specific variables used in a model, and are the workhorse functions called internally by \code{\link{marginal_effects}}.
#' 
#' \code{mfx} is an S3 generic with classes implemented for specific variable types. S3 method dispatch, somewhat atypically, is based upon the class of \code{data[[variable]]}.
#' 
#' For numeric (and integer) variables, the method uses a simple \dQuote{central difference} numerical differentiation:
#' \deqn{\frac{f(x + \frac{1}{2}h) - f(x - \frac{1}{2}h}){dh}}{(f(x + 0.5h) - f(x - 0.5h))/(2h)}, where (\eqn{h = \max(|x|, 1) \sqrt{\epsilon}}{h = max(|x|, 1)sqrt(epsilon)} and the value of \eqn{\epsilon}{epsilon} is given by argument \code{eps}. This procedure is subject to change in the future.
#' 
#' For factor variables (or character variables, which are implicitly coerced to factors by modelling functions), discrete differences in predicted outcomes are reported instead (i.e., change in predicted outcome when factor is set to a given level minus the predicted outcome when the factor is set to its baseline level). If you want to use numerical differentiation for factor variables (which you probably do not want to do), enter them into the original modelling function as numeric values rather than factors.
#' 
#' For ordered factor variables, the same approach as factors is used. This may contradict the output of modelling function summaries, which rely on \code{options("contrasts")} to determine the contrasts to use (the default being \code{\link[stats]{contr.poly}} rather than \code{\link[stats]{contr.treatment}}, the latter being used normally for unordered factors).
#' 
#' For logical variables, the same approach as factors is used, but always moving from \code{FALSE} to \code{TRUE}.
#' 
#' @return A data.frame, typically with one column unless the variable is a factor with more than two levels.
#' @examples
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
#' mfx(head(mtcars), x, "hp")
#' 
#' @seealso \code{\link{marginal_effects}}, \code{\link{margins}}
#' @export
mfx <- function(data, model, variable, ...) {
    UseMethod("mfx", data[[variable]])
}

#' @rdname mfx
#' @export
mfx.default <- function(data, model, variable, type = c("response", "link"), eps = 1e-7, ...) {
    
    type <- match.arg(type)
    
    if (!is.numeric(data[[variable]])) {
        # return empty for unidentified variable class
        warning(paste0("Class of variable, ", variable, ", is unrecognized. Returning NA."))
        return(rep(NA, nrow(data)))
    }
    
    d0 <- d1 <- data
    
    # set value of `h` based on `eps` to deal with machine precision
    setstep <- function(x) {
        x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
    }
    
    # calculate numerical derivative
    d0[[variable]] <- d0[[variable]] - setstep(d0[[variable]])
    P0 <- prediction(model = model, data = d0, type = type)[["fitted"]]
    
    d1[[variable]] <- d1[[variable]] + setstep(d1[[variable]])
    P1 <- prediction(model = model, data = d1, type = type)[["fitted"]]
    
    out <- ( P1 - P0) / (d1[[variable]] - d0[[variable]])
    
    # return data.frame with column of derivatives
    class(out) <- c("marginaleffect", "numeric")
    return(structure(setNames(list(out), variable), 
                     class = c("data.frame"), 
                     row.names = seq_len(nrow(data))))
}

#' @rdname mfx
#' @export
mfx.factor <- function(data, model, variable, type = c("response", "link"), fwrap = FALSE, ...) {
    
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
    D0 <- build_datalist(data, at = setNames(list(base), variable))[[1]]
    
    # setup functions through predict_factory
    P0 <- prediction(model = model, data = D0, type = type)[["fitted"]]
    
    # calculate difference for each factor level
    for (i in seq_along(levs)) {
        D <- build_datalist(data, at = setNames(list(levs[i]), variable))[[1]]
        P1 <- prediction(model = model, data = D, type = type)[["fitted"]]
        out[[outcolnames[i]]] <- structure(P1 - P0, class = c("marginaleffect", "numeric"))
    }
    # return data.frame with column(s) of differences
    return(out)
}

#' @rdname mfx
#' @export
mfx.ordered <- mfx.factor

#' @rdname mfx
#' @export
mfx.logical <- function(data, model, variable, type = c("response", "link"), ...) {
    
    type <- match.arg(type)
    
    # setup response object
    out <- structure(list(list()), 
                     class = "data.frame", 
                     names = variable, 
                     row.names = seq_len(nrow(data)))
    
    # setup base data and prediction
    D0 <- build_datalist(data, at = setNames(list(FALSE), variable))[[1]]
    
    # setup functions through predict_factory
    P0 <- prediction(model = model, data = D0, type = type)[["fitted"]]
    
    # calculate difference for moving FALSE to TRUE
    D1 <- build_datalist(data, at = setNames(list(TRUE), variable))[[1]]
    P1 <- prediction(model = model, data = D1, type = type)[["fitted"]]
    out[[variable]] <- P1 - P0
    
    # return data.frame with column of differences
    class(out[[1]]) <- c("marginaleffect", "numeric")
    return(out)
}

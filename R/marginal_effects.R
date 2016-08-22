#' @title Differentiate a Model Object
#' @description Extract marginal effects (via numerical differentiation) and predicted differences in factor changes from a model object, conditional on data
#' @param data A data.frame over which to calculate marginal effects.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param eps A numeric value specifying the \dQuote{step} to use when calculating numerical derivatives. By default this is the smallest floating point value that can be represented on the present architecture.
#' @details This function uses numeric differentiation (\code{\link[numDeriv]{grad}}) to extract marginal effects from an estimated model with respect to all numeric variables specified in \code{data} and returns a data.frame containing the unit-specific marginal effects with respect to each variable included (or not included) in the model. (Note that this is not each \emph{coefficient}.) For factor variables (or character variables, which are implicitly coerced to factors by modelling functions), discrete differences in predicted outcomes are reported instead (i.e., change in predicted outcome when factor is set to a given level minus the predicted outcome when the factor is set to its baseline level). If you want to use numerical differentiation for factor variables (which you probably do not want to do), enter them into the original modelling function as numeric values rather than factors.
#' 
#' Variable class coercion (other than \code{factor(x)}) inside a formula passed to, for example, \code{\link[stats]{lm}} may cause weird behavior, or errors.
#'
#' @return An data.frame with dimensions equal to \code{data}, where each row is an observation and each column is the marginal effect of that variable for the data values provided by \code{data}.
#' @examples
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
#' marginal_effects(x)
#'
#' # factor variables report discrete differences
#' x <- lm(mpg ~ factor(cyl) * factor(am), data = mtcars)
#' marginal_effects(x)
#' 
#' @seealso \code{\link{margins}}, \code{\link{build_margins}}, \code{\link{extract_marginal_effects}}
#' @keywords models
#' @export
marginal_effects <- function(model, data, type = c("response", "link"), eps = 1e-7) {
    
    # setup data, if missing
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    
    type <- match.arg(type)
    
    # identify factors versus numeric terms in `model`
    classes <- attributes(terms(model))[["dataClasses"]][-1]
    classes[classes == "character"] <- "factor"
    nnames <- clean_terms(names(classes)[classes != "factor"])
    fnames <- clean_terms(names(classes)[classes == "factor"])
    fnames2 <- names(classes)[classes == "factor"] # for checking stupid variable naming behavior by R
    
    # setup obs-x-term data.frame of obs-specific marginal effects
    out <- structure(matrix(NA_real_, nrow = nrow(data), ncol = length(nnames)), 
                     rownames = seq_len(nrow(data)))
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    for (i in seq_along(nnames)) {
        out[, i] <- get_instant_pdiff(data = data, model = model, variable = nnames[i], type = type, eps = eps)
    }
    out <- setNames(as.data.frame(out, optional = TRUE), nnames)
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    if (any(classes == "factor")) {
        for (i in seq_along(fnames)) {
            out <- cbind(out, get_factor_pdiff(data = data, model = model, fnames[i], type = type, fwrap = (fnames != fnames2)[i]))
        }
    }
    for (i in seq_along(out)) {
        class(out[[i]]) <- c("marginaleffect", "numeric")
    }
    return(out)
}

get_instant_pdiff <- function(data, model, variable, type = c("response", "link"), eps = 1e-7) {
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
    d[[variable]] <- d[[variable]] + eps
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
    D0 <- build_datalist(data, at = setNames(list(base), variable))[[1]]
    # setup functions through predict_factory
    P0 <- prediction(model = model, data = D0, type = type)[["fitted"]]
    
    # calculate difference for each factor level
    for (i in seq_along(levs)) {
        D <- build_datalist(data, at = setNames(list(levs[i]), variable))[[1]]
        P1 <- prediction(model = model, data = D, type = type)[["fitted"]]
        out[[outcolnames[i]]] <- P1 - P0
    }
    # return vector of differences
    return(out)
}

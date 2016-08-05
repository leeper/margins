#' @title Differentiate a Model Object
#' @description Extract marginal effects via numerical differentiation from a model object, conditional on data
#' @param data A data.frame over which to calculate marginal effects.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param method A character string indicating the numeric derivative method to use when estimating marginal effects. \dQuote{simple} optimizes for speed; \dQuote{Richardson} optimizes for accuracy. See \code{\link[numDeriv]{grad}} for details.
#' @details This function uses numeric differentiation (\code{\link[numDeriv]{grad}}) to extract marginal effects of an estimated model with respect to all variables specified in \code{data} and returns a data.frame containing the unit-specific marginal effects with respect to each variable included (or not included) in the model. (Note that this is not each \emph{coefficient}.)
#' @return An data.frame with dimensions equal to \code{data}, where each row is an observation and each column is the marginal effect of that variable for the data values provided by \code{data}.
#' @examples
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
#' marginal_effects(x)
#'
#' @seealso \code{\link{margins}}, \code{\link{build_margins}}, \code{\link{extract_marginal_effects}}
#' @keywords models
#' @export
marginal_effects <- function(model, data, type = c("response", "link"), method = c("simple", "Richardson", "complex")) {
    
    # setup data
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    
    type <- match.arg(type)
    method <- match.arg(method)
    
    out <- lapply(1:nrow(data), function(datarow) {
        # setup function through predict_factory
        FUN <- .build_predict_fun(data = data[datarow, , drop = FALSE], model = model, type = type)
        # extract gradient at input value
        numDeriv::grad(FUN, unlist(data[datarow,]), method = method)
        # NEED TO HANDLE FACTORS USING `get_discrete_diff()`
    })
    
    # return obs-x-term data.frame of obs-specific marginal effects
    out <- do.call("rbind.data.frame", out)
    out[] <- lapply(out, `class<-`, "marginaleffect")
    structure(out, names = names(data))
}

get_discrete_diff <- function(data, model, variable, type = c("response", "link")) {
    # @title Discrete change in fitted values
    # @description Calculate discrete change in y-hat between x=0 and x=1. This is used by \code{marginal_effects} for factor variables.
    # @param data The dataset on which to to calculate `predict(model)` (and the slope thereof)
    # @param model The model object to pass to `predict()`
    # @param variable A character string specifying the variable to calculate the difference for
    # @param type The type of prediction. Default is \dQuote{response}.
    # @param method The differentiation method to use. Passed to `numDeriv::grad()`. One of \dQuote{Richardson}, \dQuote{simple}, \dQuote{complex}.
    
    type <- match.arg(type)
    
    # setup data
    D0 <- build_datalist(data, at = setNames(list(0), variable))
    D1 <- build_datalist(data, at = setNames(list(1), variable))
    # setup functions through predict_factory
    P0 <- .build_predict_fun(data = D0, model = model, type = type)(D0)
    P1 <- .build_predict_fun(data = D1, model = model, type = type)(D1)
    # return vector of differences
    return(P1 - P0)
}

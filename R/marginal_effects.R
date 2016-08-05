#' @title Differentiate a Model Object
#' @description Extract marginal effects via numerical differentiation from a model object, conditional on data
#' @param data A data.frame over which to calculate marginal effects.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param method A character string indicating the numeric derivative method to use when estimating marginal effects. \dQuote{simple} optimizes for speed; \dQuote{Richardson} optimizes for accuracy. See \code{\link[numDeriv]{grad}} for details.
#' @details This function uses numeric differentiation (\code{\link[numDeriv]{grad}}) to extract marginal effects from an estimated model with respect to all variables specified in \code{data} and returns a data.frame containing the unit-specific marginal effects with respect to each variable included (or not included) in the model. (Note that this is not each \emph{coefficient}.)
#'
#' Note that numerical differentiation is only used for numeric variables. For factor variables (or character variables, which are implicitly coerced to factors by modelling functions), disrete differences in outcomes are reported instead. If you want to use numerical differentiation for these variables (which you probably do not want), enter them into the original modelling function as numeric values rather than factors.
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
marginal_effects <- function(model, data, type = c("response", "link"), method = c("simple", "Richardson", "complex")) {
    
    # setup data, if missing
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    
    type <- match.arg(type)
    method <- match.arg(method)
    
    # identify factors versus numeric terms in `model`
    classes <- attributes(terms(model))[["dataClasses"]]
    classes[classes == "character"] <- "factor"
    nnames <- clean_terms(names(classes)[classes != "factor"])
    fnames <- clean_terms(names(classes)[classes == "factor"])
    
    out <- lapply(seq_len(nrow(data)), function(datarow) {
        # setup function through predict_factory
        FUN <- .build_predict_fun(data = data[datarow, , drop = FALSE], model = model, type = type)
        
        # initialize output object
        vec <- setNames(numeric(length(nnames)), nnames)
        
        # extract gradient at input value
        vec[nnames] <- numDeriv::grad(FUN, unlist(data[datarow, nnames, drop = FALSE]), method = method)
        
        return(vec)
    })
    
    # setup obs-x-term data.frame of obs-specific marginal effects
    out <- do.call("rbind.data.frame", out)
    out[] <- lapply(out, `class<-`, "marginaleffect")
    out <- structure(out, names = nnames)
    
    # add discrete differences for factor variables
    ## exact number depends on number of factor levels
    if (any(classes == "factor")) {
        for (i in seq_along(fnames)) {
            out <- cbind(out, get_pdiff(data = data, model = model, fnames[i], type = type))
        }
    }
    
    return(out)
}

get_pdiff <- function(data, model, variable, type = c("response", "link")) {
    # @title Discrete change in fitted values
    # @description This is an internal function used to calculate discrete change in y-hat between factor levels and base factor level. This is used by \code{marginal_effects} for factor variables.
    # @param data The dataset on which to to calculate `predict(model)` (and the slope thereof)
    # @param model The model object to pass to `predict()`
    # @param variable A character string specifying the variable to calculate the difference for
    # @param type The type of prediction. Default is \dQuote{response}.
    # @param method The differentiation method to use. Passed to `numDeriv::grad()`. One of \dQuote{Richardson}, \dQuote{simple}, \dQuote{complex}.
    
    type <- match.arg(type)
    
    levs <- levels(as.factor(data[[variable]]))
    base <- levs[1]
    levs <- levs[-1]
    
    # setup response object
    outcolnames <- paste0(variable, ":", levs)
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

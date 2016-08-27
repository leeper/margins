#' @rdname marginal_effects
#' @title Differentiate a Model Object
#' @description Extract marginal effects (via numerical differentiation) and predicted differences in factor changes from a model object, conditional on data
#' @param data A data.frame over which to calculate marginal effects.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param eps A numeric value specifying the \dQuote{step} to use when calculating numerical derivatives. By default this is the smallest floating point value that can be represented on the present architecture.
#' @param \dots Arguments passed to methods. For methods, currently ignored.
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
marginal_effects <- function(model, data, ...) {
    UseMethod("marginal_effects")
}

#' @rdname marginal_effects
#' @export
marginal_effects.lm <- function(model, data, type = c("response", "link"), eps = 1e-7, ...) {
    
    # setup data, if missing
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    
    type <- match.arg(type)
    
    # extract term names from `model`
    term_names <- all.vars(terms(model))
    
    # identify classes of terms in `model`
    classes <- attributes(terms(model))[["dataClasses"]][-1]
    classes[classes == "character"] <- "factor"
    ## cleanup names of terms
    terms2 <- sapply(names(classes), function(x) all.vars(parse(text = x)))
    names(classes)[names(classes) %in% names(terms2)] <- terms2[names(classes) %in% names(terms2)]
    
    # identify factors versus numeric terms in `model`
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

#' @rdname marginal_effects
#' @export
marginal_effects.glm <- marginal_effects.lm

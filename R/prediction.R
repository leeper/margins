#' @rdname prediction
#' @title Extract Predictions from a Model Object
#' @description Extract predicted values via \code{\link[stats]{predict}} from a model object, conditional on data
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}.
#' @param data A data.frame over which to calculate marginal effects.
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param \dots Additional arguments passed to \code{\link[stats]{predict}} methods.
#' @details This function is simply a wrapper around \code{\link[stats]{predict}} that returns a data.frame containing predicted values with respect to all variables specified in \code{data}. It is used internally by \code{\link{build_margins}}.
#' @return An data.frame with number of rows equal to number of rows in \code{data}, where each row is an observation and the two columns represent fitted/predicted values and the standard errors thereof.
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
#' prediction(x)
#' 
#' @seealso \code{\link{marginal_effects}}, \code{\link{build_margins}}
#' @keywords models
#' @export
prediction <- function(model, data, ...) {
    UseMethod("prediction")
}

#' @rdname prediction
#' @export
prediction.lm <- function(model, data, type = "response", ...) {
    # setup data
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = "data.frame", 
              row.names = seq_len(length(pred[["fit"]])))
}

#' @rdname prediction
#' @export
prediction.glm <- function(model, data, type = c("response", "link"), ...) {
    # setup data
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = "data.frame", 
              row.names = seq_len(length(pred[["fit"]])))
}

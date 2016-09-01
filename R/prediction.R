#' @rdname prediction
#' @title Extract Predictions from a Model Object
#' @description Extract predicted values via \code{\link[stats]{predict}} from a model object, conditional on data
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}.
#' @param data A data.frame over which to calculate marginal effects.
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM). For models of class \dQuote{polr} (from \code{\link[MASS]{polr}}), possible values are \dQuote{class} or \dQuote{probs}; both are returned.
#' @param \dots Additional arguments passed to \code{\link[stats]{predict}} methods.
#' @details This function is simply a wrapper around \code{\link[stats]{predict}} that returns a data.frame containing predicted values with respect to all variables specified in \code{data}. It is used internally by \code{\link{build_margins}}.
#' 
#' Methods are curently implemented for the following object classes:
#' \itemize{
#'   \item \dQuote{lm}, see \code{\link[stats]{lm}}
#'   \item \dQuote{glm}, see \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}
#'   \item \dQuote{gls}, see \code{\link[MASS]{gls}}
#'   \item \dQuote{polr}, see \code{\link[MASS]{polr}}
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#'   \item \dQuote{ivreg}, see \code{\link[AER]{ivreg}}
#'   \item \dQuote{nls}, see \code{\link[stats]{nls}}
#'   \item \dQuote{coxph}, see \code{\link[survival]{coxph}}
#'   \item \dQuote{survreg}, see \code{\link[survival]{survreg}}
#' }
#' 
#' @return A data.frame with class \dQuote{prediction} that has a number of rows equal to number of rows in \code{data}, where each row is an observation and the first two columns represent fitted/predicted values (\code{fitted}) and the standard errors thereof (\code{se.fitted}). Additional columns may be reported depending on the object class.
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

#' @importFrom utils head
#' @export
print.prediction <- function(x, digits = 4, ...) {
    f <- x[["fitted"]]
    if (is.numeric(f)) {
        m <- mean(x[["fitted"]], na.rm = TRUE)
        m <- sprintf(paste0("%0.", digits, "f"), m)
        message(paste0("Average prediction: ", m, ", for ", length(f), " ", ngettext(length(f), "observation", "observations")))
    } else if (is.factor(f)) {
        m <- sort(table(x[["fitted"]]), decreasing = TRUE)[1]
        message(paste0("Modal prediction: ", shQuote(names(m)), " for ", m, " of ", length(f), " ", 
                ngettext(length(f), "observation", "observations"),
                " with total ", nlevels(f), " ", ngettext(nlevels(f), "level", "levels") ))
    } else {
        print(head(x), ...)
    }
    invisible(x)
}

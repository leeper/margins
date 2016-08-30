#' @rdname marginal_effects
#' @title Differentiate a Model Object
#' @description Extract marginal effects (via numerical differentiation) and predicted differences in factor changes from a model object, conditional on data
#' @param data A data.frame over which to calculate marginal effects. This is optional, but may be required when the underlying modelling function sets \code{model = FALSE}.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param eps A numeric value specifying the \dQuote{step} to use when calculating numerical derivatives. By default this is the smallest floating point value that can be represented on the present architecture.
#' @param \dots Arguments passed to methods. For methods, currently ignored.
#' @details This function uses numeric differentiation to extract marginal effects from an estimated model with respect to all numeric variables specified in \code{data} and returns a data.frame containing the unit-specific marginal effects with respect to each variable included (or not included) in the model. (Note that this is not each \emph{coefficient}.) For factor variables (or character variables, which are implicitly coerced to factors by modelling functions), discrete differences in predicted outcomes are reported instead (i.e., change in predicted outcome when factor is set to a given level minus the predicted outcome when the factor is set to its baseline level). If you want to use numerical differentiation for factor variables (which you probably do not want to do), enter them into the original modelling function as numeric values rather than factors.
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

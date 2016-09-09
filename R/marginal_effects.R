#' @rdname marginal_effects
#' @title Differentiate a Model Object
#' @description Extract marginal effects (via numerical differentiation) and predicted differences in factor changes from a model object, conditional on data
#' @param data A data.frame over which to calculate marginal effects. This is optional, but may be required when the underlying modelling function sets \code{model = FALSE}.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param eps A numeric value specifying the \dQuote{step} to use when calculating numerical derivatives. By default this is the smallest floating point value that can be represented on the present architecture.
#' @param \dots Arguments passed to methods. For methods, currently ignored.
#' @details This function extracts unit-specific marginal effects from an estimated model with respect to \emph{all} variables specified in \code{data} and returns a data.frame. (Note that this is not each \emph{coefficient}.) See \code{\link{mfx}} for computational details, or to extract the marginal effect for only one variable. Note that for factor and logical class variables, discrete changes in the outcome are reported rather than instantaneous marginal effects.
#' 
#' Methods are currently implemented for the following object classes:
#' \itemize{
#'   \item \dQuote{lm}, see \code{\link[stats]{lm}}
#'   \item \dQuote{glm}, see \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#' }
#'
#' @note Variable class coercion (other than \code{factor(x)}) inside a formula passed to, for example, \code{\link[stats]{lm}} may cause weird behavior, or errors.
#'
#' @return An data.frame with dimensions equal to \code{data}, where each row is an observation and each column is the marginal effect of that variable for the data values provided by \code{data}.
#' @references
#'   Miranda, Mario J. and Paul L. Fackler. 2002. Applied Computational Economics and Finance. p. 103.
#' 
#' @examples
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
#' marginal_effects(x)
#'
#' # factor variables report discrete differences
#' x <- lm(mpg ~ factor(cyl) * factor(am), data = mtcars)
#' marginal_effects(x)
#' 
#' @seealso \code{\link{mfx}}, \code{\link{margins}}, \code{\link{build_margins}}, \code{\link{extract_marginal_effects}}
#' @keywords models
#' @export
marginal_effects <- function(model, data, ...) {
    UseMethod("marginal_effects")
}

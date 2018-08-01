#' @rdname marginal_effects
#' @title Differentiate a Model Object with Respect to All (or Specified) Variables
#' @description Extract marginal effects from a model object, conditional on data, using \code{\link{dydx}}.
#' @param data A data.frame over which to calculate marginal effects. This is optional, but may be required when the underlying modelling function sets \code{model = FALSE}.
#' @param variables A character vector with the names of variables for which to compute the marginal effects. The default (\code{NULL}) returns marginal effects for all variables.
#' @param model A model object, perhaps returned by \code{\link[stats]{lm}} or \code{\link[stats]{glm}}
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param eps A numeric value specifying the \dQuote{step} to use when calculating numerical derivatives. By default this is the smallest floating point value that can be represented on the present architecture.
#' @param as.data.frame A logical indicating whether to return a data frame (the default) or a matrix.
#' @param varslist A list structure used internally by \code{\link{margins}}. Users should not set this.
#' @param \dots Arguments passed to methods, and onward to \code{\link{dydx}} methods and possibly further to \code{\link[prediction]{prediction}} methods. This can be useful, for example, for setting \code{type} (predicted value type), \code{eps} (precision), or \code{category} (category for multi-category outcome models), etc.
#' @details Users likely want to use the fully featured \code{\link{margins}} function rather than \code{marginal_effects}, which merely performs estimation of the marginal effects but simply returns a data frame. \code{\link{margins}}, by contrast, does some convenient packaging around these results and supports additional functionality, like variance estimation and counterfactual estimation procedures. The methods for this function provide lower-level functionality that extracts unit-specific marginal effects from an estimated model with respect to \emph{all} variables specified in \code{data} (or the subset specified in \code{variables}) and returns a data frame. See \code{\link{dydx}} for computational details. Note that for factor and logical class variables, discrete changes in the outcome are reported rather than instantaneous marginal effects.
#' 
#' Methods are currently implemented for the following object classes:
#' \itemize{
#'   \item \dQuote{betareg}, see \code{\link[betareg]{betareg}}
#'   \item \dQuote{glm}, see \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}
#'   \item \dQuote{ivreg}, see \code{\link[AER]{ivreg}}
#'   \item \dQuote{lm}, see \code{\link[stats]{lm}}
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#'   \item \dQuote{merMod}, see \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}
#'   \item \dQuote{multinom}, see \code{\link[nnet]{multinom}}
#'   \item \dQuote{nnet}, see \code{\link[nnet]{nnet}}
#'   \item \dQuote{polr}, see \code{\link[MASS]{polr}}
#'   \item \dQuote{svyglm}, see \code{\link[survey]{svyglm}}
#' }
#'
#' A method is also provided for the object classes \dQuote{margins} to return a simplified data frame from complete \dQuote{margins} objects.
#' 
#' @return An data frame with number of rows equal to \code{nrow(data)}, where each row is an observation and each column is the marginal effect of a variable used in the model formula.
#' @examples
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
#' marginal_effects(x)
#'
#' # factor variables report discrete differences
#' x <- lm(mpg ~ factor(cyl) * factor(am), data = mtcars)
#' marginal_effects(x)
#' 
#' # get just marginal effects from "margins" object
#' require('datasets')
#' m <- margins(lm(mpg ~ hp, data = mtcars[1:10,]))
#' marginal_effects(m)
#' marginal_effects(m)
#'
#' # multi-category outcome
#' if (requireNamespace("nnet")) {
#'   data("iris3", package = "datasets")
#'   ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
#'                     species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
#'   m <- nnet::nnet(species ~ ., data = ird, size = 2, rang = 0.1,
#'                   decay = 5e-4, maxit = 200, trace = FALSE)
#'   marginal_effects(m) # default
#'   marginal_effects(m, category = "v") # explicit category
#' }
#' 
#' @seealso \code{\link{dydx}}, \code{\link{margins}}
#' @keywords models
#' @export
marginal_effects <- function(model, data, variables = NULL, ...) {
    UseMethod("marginal_effects")
}

#' @rdname marginal_effects
#' @export
marginal_effects.margins <- function(model, data, variables = NULL, ...) {
    if (!missing(data)) {
        stop("Argument 'data' is ignored for objects of class 'margins'")
    }
    if (!is.null(variables)) {
        stop("Argument 'variables' is ignored for objects of class 'margins'")
    }
    w <- grepl("^dydx", names(model))
    out <- model[, w, drop = FALSE]
    attributes(out) <- attributes(model)[names(attributes(model)) != "names"]
    names(out) <- names(model)[w]
    out
}

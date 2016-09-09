#' @rdname margins
#' @title Marginal Effects Estimation
#' @description \code{margins} is an S3 generic function for building a \dQuote{margins} object from a model object. Methods are currently implemented for \dQuote{lm} (and, implicitly, \dQuote{glm}) class objects.
#' @param model A model object of class \dQuote{lm}.
#' @param data A data frame containing the data at which to evaluate the marginal effects, as in \code{\link[stats]{predict}}. This is optional, but may be required when the underlying modelling function sets \code{model = FALSE}.
#' @param at A list of one or more named vectors, specifically values at which to calculate the marginal effects. See \code{\link{build_datalist}} for details on use.
#' @param \dots Arguments passed to methods, and in turn to \code{\link{build_margins}}.
#' @details Methods for this generic return a \dQuote{marginslist} list of one or more \dQuote{margins} objects. A \dQuote{margins} object is a data.frame consisting of the original data, predicted values and standard errors thereof, estimated marginal effects from the model \code{model}, with attributes describing various features of the marginal effects estimates.
#' 
#' Some modelling functions set \code{model = FALSE} by default. For \code{margins} to work best, this should be set to \code{TRUE}. Otherwise the \code{data} argument to \code{margins} is probably required.
#' 
#' See \code{\link{mfx}} for details on estimation of marginal effects.
#' 
#' Methods are currently implemented for the following object classes:
#' \itemize{
#'   \item \dQuote{lm}, see \code{\link[stats]{lm}}
#'   \item \dQuote{glm}, see \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#' }
#'
#' The \code{margins} method for objects of class \dQuote{lm} or \dQuote{glm} simply constructs a list of data.frames (using \code{\link{build_datalist}}) and calls \code{\link{build_margins}} on each. You can call \code{build_margins} directly, but it requires the explicit specification of a dataset over which to estimate the quantities of interest.
#' 
#' Alternatively, you can use \code{\link{marginal_effects}} to retrieve a data frame of marginal effects without constructing a \dQuote{margins} object. That can be efficient for plotting, etc., given the time-consuming nature of variance estimation. Use \code{\link{extract_marginal_effects}} to retrieve a data frame of marginal effects from a \dQuote{margins} object.
#' 
#' @return An object of class \dQuote{marginslist}, composed of one or more objects of class \dQuote{margins}.
#' @author Thomas J. Leeper
#' @references
#' Greene, W.H. 2012. Econometric Analysis, 7th Ed. Boston: Pearson.
#' 
#' Stata manual: \code{margins}. Retrieved 2014-12-15 from \url{http://www.stata.com/manuals13/rmargins.pdf}.
#' @examples
#' # linear model
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
#' margins(x)
#'
#' # generalized linear model
#' x <- glm(am ~ hp, data = head(mtcars), family = binomial)
#' margins(x, type = "response")
#' margins(x, type = "link")
#' 
#' # specifying a custom `vcov` argument
#' library("sandwich")
#' x <- lm(Sepal.Length ~ Sepal.Width, data = head(iris))
#' summary(margins(x))
#' ## heteroskedasticity-consistent covariance matrix
#' summary(margins(x, vcov = vcovHC(x)))
#' 
#' @seealso \code{\link{marginal_effects}}, \code{\link{build_margins}}
#' @keywords models
#' @export
margins <- 
function(model, ...) {
    UseMethod("margins")
}


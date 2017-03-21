#' @rdname margins
#' @name margins-package
#' @docType package
#' @title Marginal Effects Estimation
#' @description This package is an R port of Stata's \samp{margins} command, implemented as an S3 generic \code{margins()} for model objects, like those of class \dQuote{lm} and \dQuote{glm}. \code{margins()} is an S3 generic function for building a \dQuote{margins} object from a model object. Methods are currently implemented for \dQuote{lm} (and, implicitly, \dQuote{glm}) class objects and support is expanding. See Details, below.
#' 
#' The package also provides a low-level function, \code{\link{marginal_effects}}, to estimate those quantities and return a data frame of unit-specific effects and another, \code{\link{dydx}}, to provide variable-specific derivatives from models. Some of the underlying architecture for the package is provided by the low-level function \code{\link[prediction]{prediction}}, which provides a consistent data frame interface to \code{\link[stats]{predict}} for a large number of model types.
#' @param model A model object of class \dQuote{lm}.
#' @param data A data frame containing the data at which to evaluate the marginal effects, as in \code{\link[stats]{predict}}. This is optional, but may be required when the underlying modelling function sets \code{model = FALSE}.
#' @param at A list of one or more named vectors, specifically values at which to calculate the marginal effects. These are used to modify the value of \code{data} (see \code{\link[prediction]{build_datalist}} for details on use).
#' @param \dots Arguments passed to methods, and in turn to \code{\link{build_margins}}.
#' @details Methods for this generic return a \dQuote{margins} object, which is a data frame consisting of the original data, predicted values and standard errors thereof, estimated marginal effects from the model \code{model}, with attributes describing various features of the marginal effects estimates.
#' 
#' Some modelling functions set \code{model = FALSE} by default. For \code{margins} to work best, this should be set to \code{TRUE}. Otherwise the \code{data} argument to \code{margins} is probably required.
#' 
#' See \code{\link{dydx}} for details on estimation of marginal effects.
#' 
#' Methods are currently implemented for the following object classes:
#' \itemize{
#'   \item \dQuote{lm}, see \code{\link[stats]{lm}}
#'   \item \dQuote{glm}, see \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}
#'   \item \dQuote{loess}, see \code{\link[stats]{loess}}
#'   \item \dQuote{merMod}, see \code{\link[lme4]{lmer}}
#' }
#'
#' The \code{margins} method for objects of class \dQuote{lm} or \dQuote{glm} simply constructs a list of data frames (using \code{\link{build_datalist}}) and calls \code{\link{build_margins}} on each. You can call \code{build_margins} directly, but it requires the explicit specification of a dataset over which to estimate the quantities of interest.
#' 
#' Alternatively, you can use \code{\link{marginal_effects}} to retrieve a data frame of marginal effects without constructing a \dQuote{margins} object. That can be efficient for plotting, etc., given the time-consuming nature of variance estimation.
#' 
#' @return A data frame of class \dQuote{margins}, composed of one or more row-stacked sets of marginal effects estimates. If \code{at = NULL} (the default), then the data frame will have a number of rows equal to \code{nrow(data)}. Otherwise, the number of rows will be a multiple thereof based upon the intersection of values specified in \code{at}. A special list column, \code{.at}, will contain information on the combination of values from \code{at} reflected in each row observation. The \code{summary.margins()} method provides for pretty printing of the results.
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
#' # use of 'at' argument
#' ## modifying original data values
#' margins(x, at = list(hp = 150))
#' ## AMEs at various data values
#' margins(x, at = list(hp = c(150, 300), cyl = c(4,6)))
#' 
#' # use of 'data' argument to obtain AMEs for a subset of data
#' margins(x, data = mtcars[mtcars[["cyl"]] == 4,])
#' margins(x, data = mtcars[mtcars[["cyl"]] == 6,])
#' 
#' # summary() method
#' summary(margins(x, at = list(hp = c(150, 300))))
#' ## control row order of summary() output
#' summary(margins(x, at = list(hp = c(150, 300))), order = "hp")
#' 
#' # generalized linear model
#' x <- glm(am ~ hp, data = head(mtcars), family = binomial)
#' margins(x, type = "response")
#' margins(x, type = "link")
#' 
#' # specifying a custom `vcov` argument
#' if (require("sandwich")) {
#'   x2 <- lm(Sepal.Length ~ Sepal.Width, data = head(iris))
#'   summary(margins(x2))
#'   ## heteroskedasticity-consistent covariance matrix
#'   summary(margins(x2, vcov = vcovHC(x2)))
#' }
#'
#' @seealso \code{\link{marginal_effects}}, \code{\link{dydx}}, \code{\link{build_margins}}, \code{\link[prediction]{prediction}}
#' @keywords models package
#' @export
margins <- 
function(model, ...) {
    UseMethod("margins")
}

#' @export
prediction::prediction
#' @export
prediction::find_data

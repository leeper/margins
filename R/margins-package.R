#' @rdname margins
#' @name margins-package
#' @title Marginal effects for model objects
#' @aliases margins-package
#' @docType package
#' @description This package is an R port of Stata's \samp{margins} command, implemented as an S3 generic \code{margins()} for model objects, like those of class \dQuote{lm} and \dQuote{glm}. The package also provides a low-level function, \code{\link{marginal_effects}}, to estimate those quantities and return a data frame of unit-specific effects and another, \code{\link{dydx}}, to provide variable-specific derivatives from models. Some of the underlying architecture for the package is provided by the low-level function \code{\link[prediction]{prediction}}, which provides a consistent data frame interface to \code{\link[stats]{predict}} for a large number of model types.
#' @author Thomas J. Leeper
#' @keywords package
#' @seealso \code{\link{margins}}, \code{\link{marginal_effects}}, \code{\link[prediction]{prediction}}
NULL

#' @export
prediction::prediction
#' @export
prediction::find_data

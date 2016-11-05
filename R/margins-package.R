#' @name margins-package
#' @title Marginal effects for model objects
#' @aliases margins-package
#' @docType package
#' @description This package is an R port of Stata's \samp{margins} command, implemented as an S3 generic \code{margins()} for model objects, like those of class \dQuote{lm} and \dQuote{glm}. The function also provides a low-level function, \code{\link{marginal_effects}}, to estimate those quantities and return a data.frame of unit-specific effects; and a imports the low-level function \code{\link[prediction]{prediction}}, which provides a consistent data frame interface to \code{\link[stats]{predict}}.
#' @author Thomas J. Leeper
#' @keywords package
#' @seealso \code{\link{margins}}, \code{\link{marginal_effects}}, \code{\link[prediction]{prediction}}
NULL

#' @export
prediction::prediction
#' @export
prediction::find_data

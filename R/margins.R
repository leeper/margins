#' @title Marginal Effects Estimation
#' @description S3 generic function for building a \dQuote{margins} object from a model object
#' @param model A model object.
#' @param \dots Additional arguments passed to methods. See, for example, \code{\link{margins.lm}}.
#' @details Alternatively, use \code{\link{marginal_effects}} to retrieve only marginal effects without constructing a \dQuote{margins} object.
#' 
#' Use \code{\link{extract_marginal_effects}} to retrieve a data.frame of marginal effects from a \dQuote{margins} object.
#' @seealso \code{\link{marginal_effects}}, \code{\link{margins.lm}}, \code{\link{margins.glm}}
#' @export
margins <- 
function(model, ...) {
    UseMethod("margins")
}

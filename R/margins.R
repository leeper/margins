#' @title Marginal Effects Estimation
#' @description S3 generic function for estimating marginal effects from modelling objects
#' @param x A model object.
#' @param \dots Addition arguments passed to methods.
#' @seealso \code{\link{margins.lm}}, \code{\link{margins.glm}}
#' @importFrom compiler cmpfun
#' @export
margins <- 
function(x, ...) {
    UseMethod("margins")
}

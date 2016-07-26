#' @title Marginal Effects for OLS Regression
#' @description Calculate marginal effects for estimated models
#' @param x A model object of class \dQuote{lm}.
#' @param newdata A data.frame containing the data at which to evaluate the marginal effects, as in \code{predict}.
#' @param at A list of one or more named vectors, specifically values at which to calculate the marginal effects. See examples. NOTE: THIS DOES NOT CURRENTLY WORK WITH VARIABLES THAT ARE TRANSFORMED IN FORMULA EXPRESSIONS (e.g., \code{y~factor(x)} or \code{y~I(x^2)}).
#' @param atmeans A logical indicating whether to calculate marginal effects at the means (i.e., partial effects at the average), as opposed to the default average marginal effects (i.e., average partial effects), which is the default.
#' @param factors A character string indicating whether, if \dQuote{continuous} (the default), to calculate marginal effects treating factor variables as continuous or, if \dQuote{discrete}, to calculate marginal effects for factor variables as discrete first differences (i.e., the difference in fitted values when the factor is 1 versus 0).
#' @param \dots Arguments passed to \code{.margins}.
#' @details Calculate marginal effects and associated standard errors for a model object.
#' @return An object of class \dQuote{margins}.
#' @author Thomas J. Leeper
#' @references
#' Greene, W.H. 2012. Econometric Analysis, 7th Ed. Boston: Pearson.
#' 
#' Stata manual: \code{margins}. Retrieved 2014-12-15 from \url{http://www.stata.com/manuals13/rmargins.pdf}.
# @seealso
#' @examples
#' library("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
#' (m <- margins(x)[[1]])
#' @importFrom numDeriv grad
#' @export
margins.lm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         factors = "continuous", ...){
    if (is.null(newdata)) {
        newdata <- if (!is.null(x$call$data)) eval(x$call$data) else x$model
    }
    data_list <- at_builder(newdata, terms = x$terms, at = at, atmeans = atmeans)
    out <- lapply(data_list, function(z) {
               m <- .margins(x = x, mm = z$mm, factors = factors, atmeans = atmeans, 
                             predicted = rep(1, nrow(z$mm)), 
                             dpredicted = rep(1, nrow(z$mm)), ...)
               attr(m, "Variables") <- attributes(z)$Variables
               m
           })
    class(out) <- "marginslist"
    out
}

#' @title Extract Marginal Effect columns from a \dQuote{margins} object
#' @description Extract Marginal Effect columns from a \dQuote{margins} object 
#' @param x An object of class \dQuote{margins}, as returned by \code{\link{margins}}.
#' @return A data.frame containing only those columns containing estimated marginal effects data.
#' @examples
#' require('datasets')
#' extract_marginal_effects(margins(lm(mpg ~ hp, data = mtcars[1:10,])))
#' @export
extract_marginal_effects <- function(x) {
    w <- which(sapply(x, inherits, "marginaleffect"))
    out <- x[, w, drop = FALSE]
    attributes(out) <- attributes(x)[names(attributes(x)) != "names"]
    names(out) <- names(x)[w]
    out
}

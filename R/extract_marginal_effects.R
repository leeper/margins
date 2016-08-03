#' @rdname extract_marginal_effects
#' @title Extract Marginal Effect columns from a \dQuote{margins} object
#' @description Extract Marginal Effect columns from a \dQuote{margins} object 
#' @param x An object of class \dQuote{margins}, as returned by \code{\link{margins}}.
#' @param \dots Ignored.
#' @return A data.frame containing only those columns containing estimated marginal effects data.
#' @examples
#' require('datasets')
#' m <- margins(lm(mpg ~ hp, data = mtcars[1:10,]))
#' extract_marginal_effects(m)
#' extract_marginal_effects(m[[1]])
#' @export
extract_marginal_effects <- function(x, ...) {
    UseMethod("extract_marginal_effects")
}

#' @rdname extract_marginal_effects
#' @export
extract_marginal_effects.margins <- function(x, ...) {
    w <- which(sapply(x, inherits, what = "marginaleffect"))
    out <- x[, w, drop = FALSE]
    attributes(out) <- attributes(x)[names(attributes(x)) != "names"]
    names(out) <- names(x)[w]
    out
}

#' @rdname extract_marginal_effects
#' @export
extract_marginal_effects.marginslist <- function(x, ...) {
    lapply(x, extract_marginal_effects, ...)
}

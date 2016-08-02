#' @title Extract Marginal Effect columns from a \dQuote{margins} object
#' @descriptionExtract Marginal Effect columns from a \dQuote{margins} object 
#' @param x An object of class \dQuote{margins}, as returned by \code{\link{margins}}.
#' @return A data.frame containing only those columns containing estimated marginal effects data.
#' @examples
#' require('datasets')
#' extract_marginal_effects(margins(lm(mpg ~ hp, data = mtcars[1:10,])))
#' @export
extract_marginal_effects <- function(x) {
    x <- x[,which(sapply(x, inherits, "marginaleffect")), drop = FALSE]
    structure(x, class = c("margins", "data.frame"))
}

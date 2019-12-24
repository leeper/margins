#' @export
coef.margins <- function(object, ...) {
    attr(object, "vcov")
}

#' @export
coef.summary.margins <- function(object, ...) {
    attr(object, "vcov")
}

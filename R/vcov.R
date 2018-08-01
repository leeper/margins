#' @export
vcov.margins <- function(object, ...) {
    attr(object, "vcov")
}

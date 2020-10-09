#' @export
coef.margins <- function(object, ...) {
    summary(object)$AME
}

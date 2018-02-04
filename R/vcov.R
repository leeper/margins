#' @export
vcov.margins <- function(object, ...) {
    vc <- attr(object, "vcov")
    if (length(vc) > 1) {
        warning("margins() with an 'at' specification requested. vcov() gives a list of matrices!")
        return(vc)
    } else {
        return(vc[[1L]])
    }
}

#' @export
print.margins <- 
function(x, digits = 4, ...) {
    print(colMeans(extract_marginal_effects(x)), digits = digits, ...)
    invisible(x)
}

#' @export
print.marginslist <- function(x, ...) {
    for (i in 1:length(x)) {
        print(x[[i]], ...)
        cat("\n")
    }
    invisible(x)
}

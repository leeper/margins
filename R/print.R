#' @export
print.margins <- 
function(x, digits = 4, ...) {
    print(colMeans(extract_marginal_effects(x), na.rm = TRUE), digits = digits, ...)
    invisible(x)
}

#' @export
print.marginslist <- function(x, ...) {
    for (i in seq_len(length(x))) {
        print(x[[i]], ...)
        message("\n")
    }
    invisible(x)
}

#' @importFrom utils capture.output
#' @export
print.margins <- 
function(x, digits = 4, row.names = FALSE, ...) {
    print(summary(x), row.names = row.names, ...)
    invisible(x)
}

#' @export
print.marginslist <- function(x, row.names = FALSE, ...) {
    for (i in 1:length(x)) {
        print(summary(x[[i]]), row.names = row.names, ...)
        cat("\n")
    }
}

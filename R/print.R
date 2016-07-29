#' @export
print.margins <- 
function(x, digits = 4, row.names = FALSE, ...) {
    if (attributes(x)$atmeans) {
        cat("Marginal Effects at Means\n")
    } else {
        cat("Average Marginal Effects\n")
    }
    print(colMeans(x$Effects))
    invisible(x)
}

#' @export
print.marginslist <- function(x, ...) {
    for (i in 1:length(x)) {
        print(summary(x[[i]]), ...)
        cat("\n")
    }
}

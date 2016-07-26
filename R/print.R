#' @export
print.margins <- 
function(x, digits = 4, row.names = FALSE, ...){
    if (attributes(x)$atmeans) {
        cat("Marginal Effects at Means\n")
    } else {
        cat("Average Marginal Effects\n")
    }
    if (length(x) > 1) {
        m <- lapply(x, function(z) do.call("rbind", lapply(z, mean)))
        print(m)
    } else {
        print(do.call("rbind", lapply(x[[1]], mean)))
    }
    #v <- attributes(x)$Variables
    #if(!is.null(v)) {
    #    cat(paste0("Variables set at: ", paste0(names(v), " = ", v, collapse = "; "), "\n\n"))
    #}
    #print(summary(x, digits = digits), row.names = row.names, ...)
    cat("\n")
    invisible(x)
}

#' @export
print.marginslist <- function(x, ...) {
    for (i in 1:length(x)) {
        print(x[[i]], ...)
    }
}

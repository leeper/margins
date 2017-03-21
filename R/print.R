#' @export
print.margins <- 
function(x, digits = 4, order = NULL, ...) {
    if (is.null(attributes(x)[["at"]])) {
        message("Average marginal effects")
        if (!is.null(attributes(x)[["call"]])) {
            message(deparse(attributes(x)[["call"]]), "\n")
        }
        x <- marginal_effects(x, with_at = FALSE)
        names(x) <- gsub("^dydx_", "", names(x))
        print(aggregate(. ~ 1L, data = x, FUN = mean, na.rm = TRUE), digits = digits, row.names = FALSE, ...)
    } else {
        message("Average marginal effects at specified values")
        if (!is.null(attributes(x)[["call"]])) {
            message(deparse(attributes(x)[["call"]]), "\n")
        }
        tmp <- cbind(x[, grepl("^dydx_", names(x)), drop = FALSE])
        xby <- x[ , attributes(x)[["at"]], drop = FALSE]
        out <- aggregate(tmp, xby, FUN = mean, na.rm = TRUE)
        #names(out)[names(out) == "at"] <- paste0("_at(", paste0(names(xby), collapse = ","), ")")
        names(out)[!grepl("^dydx", names(out))] <- paste0("at(", names(out)[!grepl("^dydx", names(out))], ")")
        names(out) <- gsub("^dydx_", "", names(out))
        print(out, digits = digits, row.names = FALSE, ...)
    }
    invisible(x)
}

#' @export
print.summary.margins <- function(x, digits = 4, ...) {
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    for (i in seq_len(ncol(x))[-which(names(x) %in% c("factor", ".at", attributes(x)[["at"]]))]) {
        x[[i]] <- sprintf(fmt, x[[i]])
    }
    print(`class<-`(x, "data.frame"), ..., digits = digits, row.names = FALSE)
    invisible(x)
}

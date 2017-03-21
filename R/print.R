#' @export
print.margins <- 
function(x, digits = 4, order = NULL, ...) {
    if (is.null(x[[".at"]][[1L]])) {
        x <- marginal_effects(x)
        names(x) <- gsub("^dydx_", "", names(x))
        print(aggregate(. ~ 1, data = x, FUN = mean, na.rm = TRUE), digits = digits, row.names = FALSE, ...)
    } else {
        at_labels <- unlist(lapply(x[[".at"]], collapse_at_vals, order = order))
        tmp <- cbind(x[, grepl("^dydx_", names(x)), drop = FALSE])
        names(tmp) <- gsub("^dydx_", "", names(tmp))
        tmp[[".at"]] <- at_labels
        print(aggregate(. ~ .at, data = tmp, FUN = mean, na.rm = TRUE), digits = digits, row.names = FALSE, ...)
    }
    invisible(x)
}

#' @export
print.summary.margins <- function(x, digits = 4, ...) {
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    if (is.null(attributes(x)[["at"]])) {
        message("Average Marginal Effects")
        x[[".at"]] <- ""
    } else {
        message("Average Marginal Effects at specified values")
    }
    if (!is.null(attributes(x)[["call"]])) {
        message(deparse(attributes(x)[["call"]]), "\n")
    }
    for (i in seq_len(ncol(x))[-which(names(x) %in% c("Factor", ".at"))]) {
        x[[i]] <- sprintf(fmt, x[[i]])
    }
    print(`class<-`(x, "data.frame"), digits = digits, row.names = FALSE, ...)
    invisible(x)
}

collapse_at_vals <- function(atvals, order = NULL, colon = ": ", sep = "", collapse = ", ") {
    # convert a named vector of at values into a single character string
    if (!is.null(order)) {
        order <- order[!duplicated(order)]
        atvals <- atvals[c(order, sort(names(atvals)[!names(atvals) %in% order]))]
    }
    paste0(names(atvals), colon, atvals, sep = sep, collapse = collapse)
}

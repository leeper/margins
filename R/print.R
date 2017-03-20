#' @export
print.margins <- 
function(x, digits = 4, ...) {
    if (is.na(x[[".at"]][1])) {
        vals <- colMeans(marginal_effects(x), na.rm = TRUE)
        names(vals) <- gsub("^dydx_", "", names(vals))
        print(vals, digits = digits, ...)
    } else {
        tmp <- split(x, x[[".at"]])
        lapply(tmp, function(one) {
            vals <- colMeans(marginal_effects(one), na.rm = TRUE)
            names(vals) <- gsub("^dydx", "", names(vals))
            cat(one[[".at"]][1], ":\n", sep = "")
            print(vals, digits = digits, ...)
        })
    }
    invisible(x)
}

#' @export
print.summary.margins <- function(x, digits = 4, ...) {
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    if (is.null(attributes(x)[["at"]])) {
        message("Average Marginal Effects")
    } else {
        atvals <- paste(names(attributes(x)[["at"]]), "=", attributes(x)[["at"]][1,], collapse = ", ")
        message("Average Marginal Effects, with ", atvals)
    }
    if (!is.null(attributes(x)[["call"]])) {
        message(deparse(attributes(x)[["call"]]), "\n")
    }
    for (i in seq_len(ncol(x))[-1L]) {
        x[[i]] <- sprintf(fmt, x[[i]])
    }
    print(`class<-`(x, "data.frame"))
    invisible(x)
}

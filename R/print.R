#' @export
print.margins <- 
function(x, digits = 4, order = NULL, ...) {
    # check for weights
    is_weighted <- attr(x, "weighted")
    if (isTRUE(is_weighted)) {
        wts <- x[["_weights"]]
    }
    
    # check for `at` specification
    at_names <- setdiff(names(attr(x, "at")), "index")
    if (is.null(at_names)) {
        if (isTRUE(is_weighted)) {
            message("Average marginal effects (survey-weighted)")
        } else {
            message("Average marginal effects")
        }
        if (!is.null(attributes(x)[["call"]])) {
            message(deparse(attributes(x)[["call"]]), "\n")
        }
        tmp <- marginal_effects(x, with_at = FALSE)
        names(tmp) <- gsub("^dydx_", "", names(tmp))
        if (isTRUE(is_weighted)) {
            out <- data.frame(lapply(tmp, stats::weighted.mean, w = wts, na.rm = TRUE), check.names = FALSE)
        } else {
            out <- data.frame(lapply(tmp, mean, na.rm = TRUE), check.names = FALSE)
        }
        print(out, digits = digits, row.names = FALSE, ...)
    } else {
        if (isTRUE(is_weighted)) {
            message("Average marginal effects at specified values (survey-weighted)")
        } else {
            message("Average marginal effects at specified values")
        }
        if (!is.null(attr(x, "call"))) {
            message(deparse(attributes(x)[["call"]]), "\n")
        }
        if (isTRUE(is_weighted)) {
            tmp <- x[, c(at_names, "_weights", names(x)[grepl("^dydx_", names(x))]), drop = FALSE]
            xby <- x[ , at_names, drop = FALSE]
            splits <- split(tmp, xby)
            out <- do.call("rbind", lapply(splits, function(set) {
                cbind(set[1L, at_names, drop = FALSE], data.frame(lapply(set[, !names(set) %in% c("_weights", at_names), drop = FALSE], stats::weighted.mean, w = set[["_weights"]], na.rm = TRUE), check.names = FALSE))
            }))
        } else {
            tmp <- x[, grepl("^dydx_", names(x)), drop = FALSE]
            xby <- x[ , at_names, drop = FALSE]
            out <- aggregate(tmp, xby, FUN = mean, na.rm = TRUE)
        }
        names(out)[!grepl("^dydx", names(out))] <- paste0("at(", names(out)[!grepl("^dydx", names(out))], ")")
        names(out) <- gsub("^dydx_", "", names(out))
        print(out, digits = digits, row.names = FALSE, ...)
    }
    invisible(x)
}

#' @export
print.summary.margins <- function(x, digits = 4, ...) {
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    for (i in seq_len(ncol(x))[-which(names(x) %in% c("factor", ".at", attr(x, "at_vars")))]) {
        x[[i]] <- sprintf(fmt, x[[i]])
    }
    print(`class<-`(x, "data.frame"), ..., digits = digits, row.names = FALSE)
    invisible(x)
}

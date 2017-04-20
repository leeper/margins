#' @export
summary.margins <- 
function(object, level = 0.95, by_factor = TRUE, FUN = weighted.mean, ...) {
    at_names <- attributes(object)[["at"]]
    if (is.null(at_names)) {
        out <- summarize_one(object, level = level, ...)
        out <- out[order(out[["factor"]]), ]
    } else {
        at_split <- split(object, object[at_names])
        out <- list()
        for (i in seq_along(at_split)) {
            out[[i]] <- summarize_one(at_split[[i]], level = level, FUN = FUN, ...)
            out[[i]] <- cbind(out[[i]], at_split[[i]][1L, at_names, drop = FALSE])
        }
        out <- do.call("rbind", out)
        if (isTRUE(by_factor)) {
            out <- out[do.call('order', unname(out[c("factor", at_names)])), ]
        } else {
            out <- out[do.call('order', unname(out[c(at_names, "factor")])), ]
        }
    }
    structure(out[, c("factor", at_names, names(out)[!names(out) %in% c("factor", at_names)]), drop = FALSE], 
              class = c("summary.margins", "data.frame"),
              row.names = seq_len(nrow(out)),
              type = attributes(object)[["type"]],
              call = attributes(object)[["call"]],
              vce = attributes(object)[["vce"]],
              iterations = attributes(object)[["iterations"]],
              level = level,
              at = attributes(object)[["at"]])
}

summarize_one <- function(object, level = 0.95, FUN = weighted.mean, ...) {
    mes <- marginal_effects(object)
    names(mes) <- gsub("^dydx_", "", names(mes))
    variances <- unlist(object[1L, grepl("Var_dydx_", names(object), fixed = TRUE), drop = TRUE])
    tab <- structure(list("factor" = names(mes), 
                          "AME" = unlist(lapply(mes, FUN = FUN, na.rm = TRUE)),
                          "SE" = if (is.null(variances)) rep(NA_real_, ncol(mes)) else sqrt(variances)
                          ),
                     class = "data.frame", row.names = names(mes))
    tab[["z"]] <- tab[,"AME"]/tab[,"SE"]
    tab[["p"]] <- 2 * pnorm(abs(tab[,"z"]), lower.tail = FALSE)
    cbind(tab, confint(object = object, level = level))
}

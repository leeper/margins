#' @export
summary.margins <- 
function(object, at = object[[".at"]], level = 0.95, ...) {
    if (is.na(object[[".at"]][1])) {
        summarize_one(object, level = level, ...)
    } else {
        tmp <- split(object, object[[".at"]])
        lapply(tmp, summarize_one, level = level, ...)
    }
}

summarize_one <- function(object, level = 0.95, ...) {
    mes <- marginal_effects(object)
    names(mes) <- gsub("^dydx_", "", names(mes))
    variances <- unlist(object[1L, grepl("Var_dydx_", names(object), fixed = TRUE), drop = TRUE])
    tab <- structure(list(Factor = names(mes), 
                          "dy/dx" = colMeans(mes, na.rm = TRUE),
                          "Std.Err." = if (is.null(variances)) rep(NA_real_, ncol(mes)) else sqrt(variances)
                          ),
                     class = "data.frame", row.names = names(mes))
    tab[["z value"]] <- tab[,"dy/dx"]/tab[,"Std.Err."]
    tab[["Pr(>|z|)"]] <- 2 * pnorm(abs(tab[,"z value"]), lower.tail = FALSE)
    structure(cbind(tab, confint(object = object, level = level)),
              class = c("summary.margins", "data.frame"),
              call = attributes(object)[["call"]],
              at = attributes(object)[["at"]])
}

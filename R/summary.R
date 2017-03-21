#' @export
summary.margins <- 
function(object, level = 0.95, order = "Factor", ...) {
    if (is.null(attributes(object)[["at"]])) {
        out <- summarize_one(object, level = level, ...)
        out[[".at"]] <- NA_character_
        out <- out[order(out[["Factor"]]),]
    } else {
        object[[".at"]] <- unlist(lapply(object[[".at"]], collapse_at_vals, order = if (order == "Factor") NULL else order))
        at_split <- split(object, object[[".at"]])
        out <- list()
        for (i in seq_along(at_split)) {
            out[[i]] <- summarize_one(at_split[[i]], level = level, ...)
            out[[i]][[".at"]] <- names(at_split)[i]
        }
        out <- do.call("rbind", out)
        if (order[1L] == "Factor") {
            out <- out[order(out[["Factor"]], out[[".at"]]),]
        } else {
            out <- out[order(out[[".at"]], out[["Factor"]]),]
        }
    }
    structure(out[ , c("Factor", ".at", names(out)[!names(out) %in% c("Factor", ".at")])], 
              type = attributes(object)[["type"]],
              call = attributes(object)[["call"]],
              vce = attributes(object)[["vce"]],
              iterations = attributes(object)[["iterations"]],
              at = attributes(object)[["at"]])
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
              call = attributes(object)[["call"]])
}

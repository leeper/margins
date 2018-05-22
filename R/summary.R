#' @export
summary.margins <- 
function(object, level = 0.95, by_factor = TRUE, ...) {
    
    # check for weights
    is_weighted <- attr(object, "weighted")
    
    # check for `at` specification
    at_names <- setdiff(names(attr(object, "at")), "index")
    if (is.null(at_names)) {
        out <- summarize_one(object, level = level, is_weighted = is_weighted, ...)
        out <- out[order(out[["factor"]]), , drop = FALSE]
    } else {
        at_split <- split(object, object[at_names])
        out <- list()
        for (i in seq_along(at_split)) {
            out[[i]] <- summarize_one(at_split[[i]], level = level, is_weighted = is_weighted, ...)
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

summarize_one <- function(object, level = 0.95, is_weighted = FALSE, ...) {
    mes <- marginal_effects(object)
    names(mes) <- gsub("^dydx_", "", names(mes))
    variances <- unlist(object[1L, grepl("Var_dydx_", names(object), fixed = TRUE), drop = TRUE])
    if (isTRUE(is_weighted)) {
        wts <- object[["_weights"]]
        ames <- unlist(lapply(mes, function(one) stats::weighted.mean(one, w = wts, na.rm = TRUE)))
    } else {
        ames <- unlist(lapply(mes, FUN = mean, na.rm = TRUE))
    }
    tab <- structure(list("factor" = names(mes), 
                          "AME" = ames,
                          "SE" = if (is.null(variances)) rep(NA_real_, ncol(mes)) else sqrt(variances)
                          ),
                     class = "data.frame", row.names = names(mes))
    tab[["z"]] <- tab[,"AME"]/tab[,"SE"]
    tab[["p"]] <- 2 * pnorm(abs(tab[,"z"]), lower.tail = FALSE)
    cbind(tab, confint(object = object, level = level))
}

#' @export
summary.margins <- 
function(object, level = 0.95, ...) {
    mes <- extract_marginal_effects(object)
    variances <- attributes(mes)[["variances"]]
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

#' @export
summary.marginslist <- 
function(object, row.names = FALSE, ...) {
    lapply(object, summary, ...)
}

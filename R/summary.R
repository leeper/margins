#' @export
summary.margins <- 
function(object, digits = 4, level = 0.95, ...) {
    if (attributes(object)[["atmeans"]]) {
        if (is.null(attributes(object)[["at"]])) {
            cat("Marginal Effects at Means\n")
        } else {
            cat("Marginal Effects at Means, with ", attributes(object)[["at"]], "\n")
        }
    } else {
        if (is.null(attributes(object)[["at"]])) {
            cat("Average Marginal Effects\n")
        } else {
            cat("Average Marginal Effects, with ", attributes(object)[["at"]], "\n")
        }
    }
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    mes <- extract_marginal_effects(object)
    tab <- data.frame(Factor = names(mes), 
                      "dy/dx" = colMeans(mes),
                      "Std.Err." = sqrt(attributes(mes)[["Variances"]]),
                      check.names = FALSE, stringsAsFactors = FALSE)
    tab[,"z value"] <- tab[,"dy/dx"]/tab[,"Std.Err."]
    tab[,"Pr(>|z|)"] <- 2 * pnorm(abs(tab[,"z value"]), lower.tail = FALSE)
    tab <- cbind(tab, confint(object = object, level = level))
    for (i in 2:ncol(tab)) {
        tab[,i] <- sprintf(fmt, tab[, i])
    }
    tab
}

#' @export
summary.marginslist <- 
function(object, row.names = FALSE, ...) {
    for (i in 1:length(object)) {
        print(summary(object[[i]]), row.names = row.names, ...)
        cat("\n")
    }
}

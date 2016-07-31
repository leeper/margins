#' @export
summary.margins <- 
function(object, digits = 4, level = 0.95, ...) {
    x <- object[,grep("^_", names(object)), drop = FALSE]
    x[, names(x) %in% c("_fitted", "_fitted.se")] <- NULL
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    tab <- data.frame(Factor = colnames(x), 
                      "dy/dx" = colMeans(x),
                      "Std.Err." = sqrt(x),
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
function(object, ...) {
    for (i in 1:length(object)) {
        print(summary(object[[i]]), ...)
        cat("\n")
    }
}

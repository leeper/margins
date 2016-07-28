#' @export
summary.margins <- 
function(object, digits = 4, ...){
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    tab <- data.frame(Factor = colnames(object$Effects), 
                      "dy/dx" = colMeans(object$Effects),
                      "Std.Err." = sqrt(object$Variances),
                      check.names = FALSE, stringsAsFactors = FALSE)
    tab[,"z value"] <- tab[,"dy/dx"]/tab[,"Std.Err."]
    tab[,"Pr(>|z|)"] <- 2 * pnorm(abs(tab[,"z value"]), lower.tail = FALSE)
    tab <- cbind(tab, confint(object))
    for (i in 2:ncol(tab)) {
        tab[,i] <- sprintf(fmt, tab[, i])
    }
    tab
}

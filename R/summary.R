#' @export
summary.margins <- 
function(object, digits = 4, ...){
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    tab <- data.frame(Factor = colnames(object$Effect), 
                      "dy/dx" = colMeans(object$Effect),
                      "Std.Err." = sqrt(object$Variance),
                      check.names = FALSE, stringsAsFactors = FALSE)
    tab[,"z value"] <- tab[,"dy/dx"]/tab[,"Std.Err."]
    tab[,"Pr(>|z|)"] <- 2 * pnorm(abs(tab[,"z value"]), lower.tail = FALSE)
    tab <- cbind(tab, confint(object))
    for(i in 2:ncol(tab)) {
        tab[,i] <- sprintf(fmt, tab[, i])
    }
    tab
}

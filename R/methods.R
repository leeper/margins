margins <- 
function(x, newdata = NULL, ...) {
    UseMethod("margins")
}

print.margins <- 
function(x, digits = 4, row.names = FALSE, ...){
    print(summary(x, digits = digits), row.names = row.names, ...)
    cat("\n")
    invisible(x)
}

print.marginslist <- function(x, ...) { for(i in 1:length(x)) print(x[[i]], ...)}

summary.margins <- 
function(object, digits = 4, ...){
    cat("Marginal Effects\n")
    v <- attributes(object)$Variables
    if(!is.null(v)) {
        cat(paste0("Variables set at: ", paste0(names(v), " = ", v, collapse = "; "), "\n\n"))
    }
    fmt <- paste0("%0.", ifelse(digits > 7, 7, digits), "f")
    tab <- data.frame(Factor = colnames(object$Effect), 
                      "dy/dx" = colMeans(object$Effect),
                      "Std.Err." = sqrt(object$Variance),
                      check.names = FALSE, stringsAsFactors = FALSE)
    tab[,"z value"] <- tab[,"dy/dx"]/tab[,"Std.Err."]
    tab[,"Pr(>|z|)"] <- 2 * pnorm(abs(tab[,"z value"]), lower.tail = FALSE)
    tab <- cbind(tab, confint(object))
    for(i in 2:ncol(tab)) tab[,i] <- sprintf(fmt, tab[, i])
    tab
}

confint.margins <- 
function(object, parm, level = 0.95, ...) {
    pnames <- colnames(object$Effect)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    cf <- colMeans(object$Effect)[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, a))
    ses <- sqrt(object$Variance)
    ci[] <- cf + ses %o% fac
    colnames(ci) <- sprintf("%0.2f%%", 100 * a)
    ci
}

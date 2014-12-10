print.margins <- 
function(x, digits = getOption('digits',4), row.names = FALSE, ...){
    print(summary(x), digits = digits, row.names = row.names, ...)
    invisible(x)
}

summary.margins <- 
function(object, ...){
    tab <- data.frame(Factor = colnames(object$Effect), 
                      "dy/dx" = colMeans(object$Effect),
                      "Std.Err." = sqrt(object$Variance),
                      check.names = FALSE, stringsAsFactors = FALSE)
    tab[,"z value"] <- tab[,"dy/dx"]/tab[,"Std.Err."]
    tab[,"Pr(>|z|)"] <- 2 * pnorm(abs(tab[,"z value"]), lower.tail = FALSE)
    tab
}

confint.margins <- 
function(object, parm, level = 0.95, ...) {
    s <- summary(object)
    pnames <- s$Factor
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    s <- s[parm,]
    cf <- s[,"dy/dx"]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, a))
    ses <- s[,"Std.Err."]
    ci[] <- cf + ses %o% fac
    ci
}

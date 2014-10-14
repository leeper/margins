plot.margins <- 
function(x, 
         which = x$Factor, 
         labels = which, 
         level = 0.95,
         bg = "black", 
         lwd = 2, 
         vline = NULL,
         ...) {
    x <- x[which,]
    if("(Intercept)" %in% x$Factor) {
        x <- x[!x$Factor == "(Intercept)",]
        labels <- labels[!labels == "(Intercept)"]
    }
    quantiles <- qnorm(cbind((1-sort(level))/2, 1-(1-sort(level))/2))
    maxl <- max(abs(quantiles))
    lb <- x$Effect - (maxl * x[["Std. Error"]])
    ub <- x$Effect + (maxl * x[["Std. Error"]])
    r <- max(ub) - min(lb)
    dotchart(x$Effect, labels = as.character(labels), bg = bg, xlim = c(min(lb)-0.04*r, max(ub)+0.04*r), ...)
    if(!is.null(vline))
        abline(v = vline, col = "gray")
    mapply(function(z, lwd) {
        segments(x$Effect + (quantiles[z,1] * x[["Std. Error"]]), 1:nrow(x), 
                 x$Effect + (quantiles[z,2] * x[["Std. Error"]]), 1:nrow(x), 
                 col = bg, lwd = lwd)
    }, 1:nrow(quantiles), seq(max(lwd), 0.25, length.out = nrow(quantiles)))
    invisible(x)
}

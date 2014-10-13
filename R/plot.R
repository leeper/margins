plot.margins <- 
function(x, 
         which = row.names(x), 
         include.intercept = FALSE, 
         labels = which, 
         bg = "black", 
         lwd = 2, 
         ...) {
    x <- x[which,]
    if(!include.intercept)
        x <- x[!row.names(x) == "(Intercept)",]
    lb <- x[,1] - (1.96 * x[,2])
    ub <- x[,1] + (1.96 * x[,2])
    r <- max(lb) - min(lb)
    dotchart(x[,1], labels = labels, bg = bg, xlim = c(min(lb)-0.04*r, max(ub)+0.04*r), ...)
    segments(lb, 1:nrow(x), ub, 1:nrow(x), col = bg, lwd = lwd)
    invisible(x)
}

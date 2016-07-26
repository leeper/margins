#' @export
plot.margins <- 
function(x, 
         at = 1:ncol(x$Effect),
         which = colnames(x$Effect), 
         labels = which,
         horiz = FALSE,
         xlab = "",
         ylab = "Marginal Effect",
         level = 0.95,
         pch = 21, 
         points.col = "black",
         points.bg = "black",
         las = 0,
         cex = 1,
         lwd = 2, 
         zeroline = TRUE,
         ...) {
    MEs <- colMeans(x$Effect[,which])
    quantiles <- qnorm(cbind((1-sort(level))/2, 1-(1-sort(level))/2))
    maxl <- max(abs(quantiles))
    lb <- MEs - (maxl * sqrt(x$Variance))
    ub <- MEs + (maxl * sqrt(x$Variance))
    r <- max(ub) - min(lb)
    if(horiz) {
        plot(NA, xlim = c(min(lb)-0.04*r, max(ub)+0.04*r),
                 ylim = c(min(at)-(0.04*min(at)), max(at) + (0.04*max(at))), 
                 yaxt = 'n', xlab = ylab, ylab = xlab, las = las, ...)
        if(zeroline)
            abline(v = 0, col = "gray")
        points(MEs, at, col = points.col, bg = points.bg, pch = pch)
        axis(2, at = at, labels = as.character(labels), las = las)
        mapply(function(z, lwd) {
            segments(MEs + (quantiles[z,1] * sqrt(x$Variance)), at, 
                     MEs + (quantiles[z,2] * sqrt(x$Variance)), at, 
                     col = points.col, lwd = lwd)
        }, 1:nrow(quantiles), seq(max(lwd), 0.25, length.out = nrow(quantiles)))
    } else {
        plot(NA, xlim = c(min(at)-(0.04*min(at)), max(at) + (0.04*max(at))), 
                 ylim = c(min(lb)-0.04*r, max(ub)+0.04*r), 
                 xaxt = 'n', xlab = xlab, ylab = ylab, las = las, ...)
        if(zeroline)
            abline(h = 0, col = "gray")
        points(at, MEs, col = points.col, bg = points.bg, pch = pch)
        axis(1, at = at, labels = as.character(labels), las = las)
        mapply(function(z, lwd) {
            segments(at, MEs + (quantiles[z,1] * sqrt(x$Variance)), 
                     at, MEs + (quantiles[z,2] * sqrt(x$Variance)), 
                     col = points.col, lwd = lwd)
        }, 1:nrow(quantiles), seq(max(lwd), 0.25, length.out = nrow(quantiles)))
    }
    invisible(x)
}

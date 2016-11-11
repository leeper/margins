# PLOTTING UTILITY FUNCTIONS FOR cplot()

# function to setup plot
setup_cplot <- 
function(plotdat, data, 
         xvals,
         xvar,
         yvar, 
         xlim, ylim, 
         x_is_factor,
         xlab,
         ylab,
         xaxs,
         yaxs,
         las,
         scatter,
         scatter.pch,
         scatter.col,
         ...) {
    if (is.null(xlim)) {
        if (isTRUE(x_is_factor)) {
            xlim <- c(0.75, length(xvals) + 0.25)
        } else {
            xlim <- xlim
        }
    }
    
    if (is.null(ylim)) {
        tmp <- range(c(plotdat[["upper"]], plotdat[["lower"]]), na.rm = TRUE)
        rng <- diff(tmp)
        ylim <- c(min(tmp, na.rm = TRUE) - (0.05 * rng), max(tmp, na.rm = TRUE) + (0.05 * rng))
        rm(tmp)
        rm(rng)
    }
    
    if (isTRUE(x_is_factor)) {
        plot(NA, xlab = xlab, ylab = ylab, xaxt = "n", xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
        axis(1, at = seq_along(xvals), labels = xvals)
    } else {
        plot(NA, xlab = xlab, ylab = ylab, xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
        if (isTRUE(scatter)) {
            points(data[, xvar], data[, yvar], pch = scatter.pch, col = scatter.col, bg = scatter.col)
        }
    }
}

# function to draw one set of lines
draw_one <- 
function(xvals, yvals, upper, lower, 
         x_is_factor,
         col,
         lty,
         lwd,
         se.type,
         factor.pch, 
         factor.fill, 
         factor.col, 
         factor.cex,
         se.lwd,
         se.fill,
         se.col,
         se.lty) {
    if (isTRUE(x_is_factor)) {
        xvals <- seq_along(xvals)
        # uncertainty
        for (i in seq_along(xvals)) {
            segments(xvals[i], upper[i], xvals[i], lower[i], col = col, lty = lty, lwd = lwd)
        }
        
        # prediction/effect line
        points(xvals, yvals, pch = factor.pch, bg = factor.fill, col = factor.col, cex = factor.cex)
    } else {
        # uncertainty
        if (se.type == "lines") {
            lines(xvals, upper, type = "l", lwd = se.lwd, col = se.col, lty = se.lty)
            lines(xvals, lower, type = "l", lwd = se.lwd, col = se.col, lty = se.lty)
        } else {
            polygon(c(xvals, rev(xvals)), c(upper, rev(lower)), col = se.fill, border = se.col, lty = se.lty)
        }
        
        # prediction/effect line
        lines(xvals, yvals, type = "l", lwd = lwd, col = col, lty = lty)
    }
}

draw_rug <- function(x, rug.size, rug.col) {
    rug(jitter(x), ticksize = rug.size, col = rug.col, quiet = TRUE)
}

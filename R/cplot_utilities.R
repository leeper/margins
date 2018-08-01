# function to check factors
check_factors <- function(object, data, xvar, dx) {
    # check factors
    classes <- attributes(terms(object))[["dataClasses"]][-1]
    classes <- classes[names(classes) != "(weights)"]
    classes[classes == "character"] <- "factor"
    
    varslist <- find_terms_in_model(model = object)
    
    c(list(classes = classes),
      varslist,
      x_is_factor = xvar %in% varslist$fnames,
      dx_is_factor = dx %in% varslist$fnames,
      list(data = data[, c(varslist$nnames, varslist$fnames), drop = FALSE]))
}

# PLOTTING UTILITY FUNCTIONS FOR cplot()

# function to setup plot
setup_cplot <- 
function(
  plotdat,
  data,
  xvar,
  yvar,
  xlim,
  ylim,
  x_is_factor,
  y_is_factor = FALSE,
  xlab,
  ylab,
  xaxs,
  yaxs,
  las,
  scatter,
  scatter.pch,
  scatter.col,
  ...
) {
    if (is.null(xlim)) {
        if (isTRUE(x_is_factor)) {
            xlim <- c(0.75, nrow(plotdat) + 0.25)
        } else {
            xlim <- range(plotdat[["xvals"]], na.rm = TRUE)
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
        if (isTRUE(y_is_factor)) {
            plot(NA, xlab = xlab, ylab = ylab, xaxt = "n", yaxt = "n", 
                 xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
            axis(2, at = seq_len(nlevels(plotdat[["yvals"]])), labels = levels(plotdat[["yvals"]]), las = las)
        } else {
            plot(NA, xlab = xlab, ylab = ylab, xaxt = "n", 
                 xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
        }
        axis(1, at = seq_along(plotdat[["xvals"]]), labels = plotdat[["xvals"]], las = las)
    } else {
        if (isTRUE(y_is_factor)) {
            plot(NA, xlab = xlab, ylab = ylab, yaxt = "n",
                 xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
            axis(2, at = seq_len(nlevels(plotdat[["yvals"]])), labels = levels(plotdat[["yvals"]]), las = las)
        } else {
            plot(NA, xlab = xlab, ylab = ylab,
                 xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
        }
        if (isTRUE(scatter)) {
            points(data[, xvar], data[, yvar], pch = scatter.pch, col = scatter.col, bg = scatter.col)
        }
    }
}

# function to draw one set of lines
draw_one <-
function(
  xvals,
  yvals,
  upper,
  lower,
  x_is_factor,
  y_is_factor = FALSE,
  col,
  lty,
  lwd,
  se.type,
  factor.lty = 0L,
  factor.pch, 
  factor.fill, 
  factor.col, 
  factor.cex,
  se.lwd,
  se.fill,
  se.col,
  se.lty
) {
    if (isTRUE(x_is_factor)) {
        xvals <- seq_along(xvals)
        # uncertainty
        if (!missing(upper) && !missing(lower)) {
            for (i in seq_along(xvals)) {
                segments(xvals[i], upper[i], xvals[i], lower[i], col = col, lty = lty, lwd = lwd)
            }
        }
        # prediction/effect line
        points(xvals, yvals, pch = factor.pch, bg = factor.fill, col = factor.col, cex = factor.cex)
        if (isTRUE(y_is_factor)) {
            factor.lty <- 0L
        }
        if ((factor.lty != 0L)) {
            lines(xvals, yvals, col = factor.col, lty = lty)
        }
    } else {
        # uncertainty
        if (!missing(upper) && !missing(lower)) {
            if (se.type == "lines") {
                lines(xvals, upper, type = "l", lwd = se.lwd, col = se.col, lty = se.lty)
                lines(xvals, lower, type = "l", lwd = se.lwd, col = se.col, lty = se.lty)
            } else if (se.type == "shade") {
                polygon(c(xvals, rev(xvals)), c(upper, rev(lower)), col = se.fill, border = se.col, lty = se.lty)
            } else {
                # do nothing
            }
        }
        # prediction/effect line
        if (isTRUE(y_is_factor)) {
            w <- which(diff(as.integer(yvals)) != 0)
            f <- stepfun(x = c(xvals[w], max(xvals, na.rm = TRUE)), 
                         y = c(yvals[1L], yvals[w+1L], yvals[length(yvals)]))
            plot(f, pch = factor.pch, col = factor.col, do.points = FALSE, col.vert = "white", add = TRUE)
        } else {
            lines(xvals, yvals, type = "l", lwd = lwd, col = col, lty = lty)
        }
    }
}

draw_rug <- function(x, rug.size, rug.col) {
    rug(jitter(x), ticksize = rug.size, col = rug.col, quiet = TRUE)
}

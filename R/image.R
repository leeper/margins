#' @rdname persp
#' @param xaxs A character string specifying the x-axis type (see \code{\link[graphics]{par}}).
#' @param yaxs A character string specifying the y-axis type (see \code{\link[graphics]{par}}).
#' @param bty A character string specifying the box type (see \code{\link[graphics]{par}}).
#' @param col A character vector specifying colors to use when coloring the contour plot.
#' @param contour For \code{image}, a logical specifying whether to overlay contour lines onto the heatmap using \code{\link[graphics]{contour}}.
#' @param contour.labels For \code{image}, if \code{contour = TRUE} a logical specifying whether to overlay contour lines onto the heatmap.
#' @param contour.drawlabels For \code{image}, if \code{contour = TRUE} a logical specifying whether to overlay contour lines onto the heatmap.
#' @param contour.cex For \code{image}, if \code{contour = TRUE} and \code{contour.drawlabels = TRUE} a numeric specifying the label size for contour line labels (see \code{\link[graphics]{par}}).
#' @param contour.col For \code{image}, if \code{contour = TRUE} a character string specifying a color for contour lines.
#' @param contour.lty For \code{image}, if \code{contour = TRUE} an integer specifying a line type for contour lines (see \code{\link[graphics]{par}}).
#' @param contour.lwd For \code{image}, if \code{contour = TRUE} an integer specifying a line width for contour lines (see \code{\link[graphics]{par}}).
#' @importFrom graphics image contour
#' @importFrom grDevices gray
#' @export
image.lm <-
function(x, 
         xvar = attributes(terms(x))[["term.labels"]][1],
         yvar = attributes(terms(x))[["term.labels"]][2], 
         what = c("prediction", "effect"), 
         type = c("response", "link"), 
         vcov = stats::vcov(x),
         nx = 25L,
         ny = nx,
         nz = 20,
         xlab = xvar, 
         ylab = yvar, 
         xaxs = "i",
         yaxs = xaxs,
         bty = "o",
         col = gray(seq(0.05,0.95, length.out = nz), alpha = 0.75),
         contour = TRUE,
         contour.labels = NULL,
         contour.drawlabels = TRUE,
         contour.cex = 0.6,
         contour.col = "black",
         contour.lty = 1,
         contour.lwd = 1,
         ...) {
    what <- match.arg(what)
    type <- match.arg(type)
    surface <- calculate_surface(x = x, xvar = xvar, yvar = yvar, nx = nx, ny = ny, type = type, vcov = vcov, what = what)
    outcome <- surface[["outcome"]]
    xvals <- surface[["xvals"]]
    yvals <- surface[["yvals"]]
    
    image(x = xvals, y = yvals, z = outcome, xaxs = xaxs, yaxs = yaxs,
          xlab = xlab, ylab = ylab, bty = bty, col = col, ...)
    
    if (isTRUE(contour)) {
        contour(x = xvals, y = yvals, z = outcome, nlevels = nz, labels = contour.labels, 
                drawlabels = contour.drawlabels, col = contour.col, lty = contour.lty, 
                cex = contour.cex, lwd = contour.lwd, add = TRUE)
    }
    
    invisible(NULL)
}

#' @rdname persp
#' @export
image.glm <- image.lm

#' @rdname persp
#' @export
image.loess <- image.lm


calculate_surface <- function(x, xvar, yvar, nx, ny, type, vcov = stats::vcov(x), what) {
    
    # internal function to calculate surface for `persp()` and `image()`
    
    dat <- x[["model"]]
    dat[] <- lapply(dat, as.numeric) # this probably isn't a good idea
    
    xvals <- seq(min(dat[[xvar]], na.rm = TRUE), 
                 max(dat[[xvar]], na.rm = TRUE), 
                 length.out = nx)
    yvals <- seq(min(dat[[yvar]], na.rm = TRUE), 
                 max(dat[[yvar]], na.rm = TRUE), 
                 length.out = ny)
    
    if (what == "prediction") {
        datmeans <- structure(lapply(colMeans(dat[, !names(dat) %in% c(xvar, yvar), drop = FALSE]), rep, length(xvals) * length(yvals)),
                              class = "data.frame", row.names = seq_len(length(xvals) * length(yvals)))
        outcome <- outer(xvals, yvals, FUN = function(a, b) {
            datmeans[, xvar] <- a
            datmeans[, yvar] <- b
            prediction(model = x, data = datmeans, type = type)[["fitted"]]
        })
    } else if (what == "effect") {
        dat2 <- expand.grid(xvals, yvals)
        names(dat2) <- c(xvar, yvar)
        cmeans <- colMeans(dat[, !names(dat) %in% c(xvar, yvar), drop = FALSE])
        for (i in seq_along(cmeans)) {
            dat2[[names(cmeans)[i]]] <- cmeans[i]
        }
        vals <- marginal_effects(data = dat2, model = x, type = type)[, xvar]
        outcome <- matrix(NA_real_, nrow = nx, ncol = ny)
        outcome[as.matrix(expand.grid(1:nx, 1:ny))] <- vals
    }
    
    return(list(outcome = outcome, xvals = xvals, yvals = yvals))
}

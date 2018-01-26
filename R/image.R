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
#' @importFrom prediction prediction
#' @export
image.lm <-
function(x, 
         xvar = attributes(terms(x))[["term.labels"]][1],
         yvar = attributes(terms(x))[["term.labels"]][2], 
         dx = xvar,
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
    surface <- calculate_surface(x = x, xvar = xvar, yvar = yvar, dx = dx, nx = nx, ny = ny, type = type, vcov = vcov, what = what)
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

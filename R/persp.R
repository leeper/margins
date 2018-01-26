#' @rdname persp
#' @title Perspective and heatmap/contour plots for models
#' @description Draw one or more perspectives plots reflecting predictions or marginal effects from a model, or the same using a flat heatmap or \dQuote{filled contour} (\code{\link[graphics]{image}}) representation. Currently methods exist for \dQuote{lm}, \dQuote{glm}, and \dQuote{loess} models.
#' @param x A model object.
#' @param xvar A character string specifying the name of variable to use as the \samp{x} dimension in the plot. See \code{\link[graphics]{persp}} for details.
#' @param yvar A character string specifying the name of variable to use as the \samp{y} dimension in the plot. See \code{\link[graphics]{persp}} for details.
#' @param dx A character string specifying the name of the variable for which the conditional average marginal effect is desired when \code{what = "effect"}. By default this is \code{xvar}.
#' @param what A character string specifying whether to draw \dQuote{prediction} (fitted values from the model, calculated using \code{\link[stats]{predict}}) or \dQuote{effect} (marginal effect of \code{dx}, using \code{\link{margins}}).
#' @param type A character string specifying whether to calculate predictions on the response scale (default) or link (only relevant for non-linear models).
#' @param vcov A matrix containing the variance-covariance matrix for estimated model coefficients, or a function to perform the estimation with \code{model} as its only argument.
#' @param nx An integer specifying the number of points across \code{x} at which to calculate the predicted value or marginal effect.
#' @param ny An integer specifying the number of points across \code{y} at which to calculate the predicted value or marginal effect.
#' @param nz An integer specifying, for \code{image}, the number of breakpoints to use when coloring the plot.
#' @param theta For \code{persp}, an integer vector specifying the value of \code{theta} in \code{\link[graphics]{persp}}. If length greater than 1, multiple subplots are drawn with different rotations.
#' @param phi For \code{persp}, an integer vector specifying the value of \code{phi} in \code{\link[graphics]{persp}}. If length greater than 1, multiple subplots are drawn with different rotations.
#' @param shade For \code{persp}, an integer vector specifying the value of \code{shade} in \code{\link[graphics]{persp}}. 
#' @param xlab A character string specifying the value of \code{xlab} in \code{\link[graphics]{persp}} or \code{\link[graphics]{image}}. 
#' @param ylab A character string specifying the value of \code{ylab} in \code{\link[graphics]{persp}} or \code{\link[graphics]{image}}. 
#' @param zlab A character string specifying the value of \code{zlab} (vertical axis label) in \code{\link[graphics]{persp}}. 
#' @param ticktype A character string specifying one of: \dQuote{detailed} (the default) or \dQuote{simple}. See \code{\link[graphics]{persp}}.
#' @param \dots Additional arguments passed to \code{\link[graphics]{persp}} or \code{\link[graphics]{image}}.
#' @examples
#' \dontrun{
#'   require('datasets')
#'   # prediction from several angles
#'   m <- lm(mpg ~ wt*drat, data = mtcars)
#'   persp(m, theta = c(45, 135, 225, 315))
#' 
#'   # flat/heatmap representation
#'   image(m)
#' 
#'   # marginal effect of 'drat' across drat and wt
#'   m <- lm(mpg ~ wt*drat*I(drat^2), data = mtcars)
#'   persp(m, xvar = "drat", yvar = "wt", what = "effect", 
#'         nx = 10, ny = 10, ticktype = "detailed")
#' 
#'   # a non-linear model
#'   m <- glm(am ~ wt*drat, data = mtcars, family = binomial)
#'   persp(m, theta = c(30, 60)) # prediction
#'   # flat/heatmap representation
#'   image(m)
#' 
#'   # effects on linear predictor and outcome
#'   persp(m, xvar = "drat", yvar = "wt", what = "effect", type = "link")
#'   persp(m, xvar = "drat", yvar = "wt", what = "effect", type = "response")
#' }
#' 
#' @seealso \code{\link{plot.margins}}, \code{\link{cplot}}
#' @keywords graphics hplot
#' @importFrom graphics persp layout
#' @importFrom grDevices n2mfrow
#' @export
persp.lm <- 
function(x, 
         xvar = attributes(terms(x))[["term.labels"]][1],
         yvar = attributes(terms(x))[["term.labels"]][2], 
         dx = xvar,
         what = c("prediction", "effect"), 
         type = c("response", "link"), 
         vcov = stats::vcov(x),
         nx = 25L,
         ny = nx,
         theta = 45, 
         phi = 10, 
         shade = 0.75, 
         xlab = xvar, 
         ylab = yvar, 
         zlab = if (match.arg(what) == "prediction") "Predicted value" else paste0("Marginal effect of ", dx),
         ticktype = c("detailed", "simple"),
         ...) {
    
    what <- match.arg(what)
    type <- match.arg(type)
    surface <- calculate_surface(x = x, xvar = xvar, yvar = yvar, dx = dx, nx = nx, ny = ny, type = type, vcov = vcov, what = what)
    outcome <- surface[["outcome"]]
    xvals <- surface[["xvals"]]
    yvals <- surface[["yvals"]]
    
    ticktype <- match.arg(ticktype)
    perspfun <- function(itheta, iphi, ...) {
        persp(xvals, yvals, outcome, theta = itheta, phi = iphi, 
              shade = 0.75, xlab = xlab, ylab = ylab, zlab = zlab, ticktype = ticktype, ...)
    }
    p <- par(mai = rep(0.5, 4))
    on.exit(par(p))
    if ((length(theta) == 1) && (length(phi) == 1)) {
        out <- list(perspfun(itheta = theta, iphi = phi, ...))
    } else {
        views <- expand.grid(theta = theta, phi = phi)
        if ((length(phi) == 1)) {
            d <- n2mfrow(length(theta))
            layout(matrix(seq_len(nrow(views)), nrow = d[1], ncol = d[2], byrow = TRUE))
        } else if ((length(theta) == 1)) {
            d <- n2mfrow(length(phi))
            layout(matrix(seq_len(nrow(views)), nrow = d[1], ncol = d[2], byrow = TRUE))
        } else {
            layout(matrix(seq_len(nrow(views)), ncol = length(theta), byrow = TRUE))
        }
        out <- mapply(perspfun, views[["theta"]], views[["phi"]], SIMPLIFY = FALSE)
    }
    invisible(out)
}

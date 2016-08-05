#' @title Perspective plots for models
#' @description Draw one or more perspectives plots reflecting predictions or marginal effects from a model. Currently methods exist for \dQuote{lm} and \dQuote{glm} models.
#' @param x A model object.
#' @param xvar A character string specifying the name of variable to use as the \samp{x} dimension in the plot. See \code{\link[graphics]{persp}} for details.
#' @param yvar A character string specifying the name of variable to use as the \samp{y} dimension in the plot. See \code{\link[graphics]{persp}} for details.
#' @param what A character string specifying whether to draw \dQuote{prediction} (fitted values from the model, calculated using \code{\link[stats]{predict}}) or \dQuote{effect} (marginal effect of \code{x}, using \code{\link{margins}}).
#' @param type A character string specifying whether to calculate predictions on the response scale (default) or link (only relevant for non-linear models).
#' @param nx An integer specifying the number of points across \code{x} at which to calculate the predicted value or marginal effect.
#' @param ny An integer specifying the number of points across \code{y} at which to calculate the predicted value or marginal effect.
#' @param theta An integer vector specifying the value of \code{theta} in \code{\link[graphics]{persp}}. If length greater than 1, multiple subplots are drawn with different rotations.
#' @param phi An integer vector specifying the value of \code{phi} in \code{\link[graphics]{persp}}. If length greater than 1, multiple subplots are drawn with different rotations.
#' @param shade An integer vector specifying the value of \code{shade} in \code{\link[graphics]{persp}}. 
#' @param xlab A character string specifying the value of \code{xlab} in \code{\link[graphics]{persp}}. 
#' @param ylab A character string specifying the value of \code{ylab} in \code{\link[graphics]{persp}}. 
#' @param zlab A character string specifying the value of \code{zlab} (vertical axis label) in \code{\link[graphics]{persp}}. 
#' @param ticktype A character string specifying one of: \dQuote{detailed} (the default) or \dQuote{simple}. See \code{\link[graphics]{persp}}.
#' @param \dots Additional arguments passed to \code{\link[graphics]{persp}}. 
#' @details Currently, this implements \dQuote{marginal effects at means} of all covariates.
#' @examples
#' \dontrun{
#' require('datasets')
#' # prediction from several angles
#' m <- lm(mpg ~ wt*drat, data = mtcars)
#' persp(m, theta = c(45, 135, 225, 315))
#' 
#' # marginal effect of 'drat' across drat and wt
#' m <- lm(mpg ~ wt*drat*I(drat^2), data = mtcars)
#' persp(m, xvar = "drat", yvar = "wt", what = "effect", nx = 10, ny = 10, ticktype = "detailed")
#' 
#' # a non-linear model
#' m <- glm(am ~ wt*drat, data = mtcars, family = binomial)
#' persp(m, theta = c(30, 60)) # prediction
#' 
#' # effects on linear predictor and outcome
#' persp(m, xvar = "drat", yvar = "wt", what = "effect", type = "link")
#' persp(m, xvar = "drat", yvar = "wt", what = "effect", type = "response")
#' }
#' @seealso \code{\link{plot.margins}}, \code{\link{cplot}}
#' @keywords graphics hplot
#' @importFrom graphics persp layout
#' @importFrom grDevices n2mfrow
#' @export
persp.lm <- 
function(x, 
         xvar = attributes(terms(x))[["term.labels"]][1],
         yvar = attributes(terms(x))[["term.labels"]][2], 
         what = c("prediction", "effect"), 
         type = c("response", "link"), 
         nx = 25L,
         ny = nx,
         theta = 45, 
         phi = 10, 
         shade = 0.75, 
         xlab = xvar, 
         ylab = yvar, 
         zlab = if (match.arg(what) == "prediction") "Predicted value" else paste0("Marginal effect of ", xvar),
         ticktype = c("detailed", "simple"),
         ...) {
    
    dat <- x[["model"]]
    dat[] <- lapply(dat, as.numeric) # this probably isn't a good idea
    
    xvals <- seq(min(dat[[xvar]], na.rm = TRUE), 
                 max(dat[[xvar]], na.rm = TRUE), 
                 length.out = nx)
    yvals <- seq(min(dat[[yvar]], na.rm = TRUE), 
                 max(dat[[yvar]], na.rm = TRUE), 
                 length.out = ny)
    
    what <- match.arg(what)
    type <- match.arg(type)
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
    
    ticktype <- match.arg(ticktype)
    perspfun <- function(itheta, iphi, ...) {
        persp(xvals, yvals, outcome, theta = itheta, phi = iphi, 
              shade = 0.75, xlab = xlab, ylab = ylab, zlab = zlab, ticktype = ticktype, ...)
    }
    p <- par(mai = rep(0.2, 4))
    on.exit(par(p))
    if ((length(theta) == 1) && (length(phi) == 1)) {
        out <- list(perspfun(itheta = theta, iphi = phi, ...))
    } else {
        views <- expand.grid(theta = theta, phi = phi)
        if ((length(phi) == 1)) {
            d <- n2mfrow(length(theta))
            layout(matrix(1:nrow(views), nrow = d[1], ncol = d[2], byrow = TRUE))
        } else if ((length(theta) == 1)) {
            d <- n2mfrow(length(phi))
            layout(matrix(1:nrow(views), nrow = d[1], ncol = d[2], byrow = TRUE))
        } else {
            layout(matrix(1:nrow(views), ncol = length(theta), byrow = TRUE))
        }
        out <- mapply(perspfun, views[["theta"]], views[["phi"]], SIMPLIFY = FALSE)
    }
    invisible(out)
}

#' @export
persp.glm <- persp.lm

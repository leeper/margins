#' @rdname cplot
#' @title Conditional predicted value and average marginal effect plots for models
#' @description Draw one or more conditioanl effects plots reflecting predictions or marginal effects from a model, conditional on a covariate. Currently methods exist for \dQuote{lm} and \dQuote{glm} models.
#' @param object A model object.
#' @param x A character string specifying the name of variable to use as the x-axis dimension in the plot.
#' @param dx If \code{what = "effect"}, the variable whose conditional marginal effect should be displayed. By default it is \code{x} (so the plot displays the marginal effect of \code{x} across values of \code{x}); ignored otherwise.
#' @param what A character string specifying whether to draw \dQuote{prediction} (fitted values from the model, calculated using \code{\link[stats]{predict}}) or \dQuote{effect} (average marginal effect of \code{dx} conditional on \code{x}, using \code{\link{margins}}).
#' @param type A character string specifying whether to calculate predictions on the response scale (default) or link (only relevant for non-linear models).
#' @param method If \code{what = "effect"}, a character string indicating the numeric derivative method to use when estimating marginal effects. \dQuote{simple} optimizes for speed; \dQuote{Richardson} optimizes for accuracy. See \code{\link[numDeriv]{grad}} for details.
#' @param n An integer specifying the number of points across \code{x} at which to calculate the predicted value or marginal effect.
#' @param level The confidence level required (used to draw uncertainty bounds).
#' @param xlab A character string specifying the value of \code{xlab} in \code{\link[graphics]{plot}}. 
#' @param ylab A character string specifying the value of \code{ylab} in \code{\link[graphics]{plot}}. 
#' @param xlim A two-element numeric vector specifying the x-axis limits. Set automatically if missing.
#' @param ylim A two-element numeric vector specifying the y-axis limits. Set automatically if missing.
#' @param lwd An integer specifying the width of the prediction or marginal effect line. See \code{\link[graphics]{lines}}.
#' @param col A character string specifying the color of the prediction or marginal effect line.
#' @param lty An integer specifying the \dQuote{line type} of the prediction or marginal effect line. See \code{\link[graphics]{par}}.
#' @param se.type A character string specifying whether to draw the confidence interval as \dQuote{lines} (the default, using \code{\link[graphics]{lines}}) or a \dQuote{shade} (using \code{\link[graphics]{polygon}}).
#' @param se.col If \code{se.type = "lines"}, a character string specifying the color of the confidence interval lines. If \code{se.type = "shade"}, the color of the shaded region border.
#' @param se.fill If \code{se.type = "shade"}, the color of the shaded region. Ignored otherwise.
#' @param se.lwd If \code{se.type = "lines"}, the width of the confidence interval lines. See \code{\link[graphics]{lines}}.
#' @param se.lty If \code{se.type = "lines"}, an integer specifying the \dQuote{line type} of the confidence interval lines; if \code{se.type = "shade"}, the line type of the shaded polygon border. See \code{\link[graphics]{par}}.
#' @param xaxs A character string specifying \code{xaxs}. See \code{\link[graphics]{par}}.
#' @param yaxs A character string specifying \code{xaxs}. See \code{\link[graphics]{par}}.
#' @param las An integer string specifying \code{las}. See \code{\link[graphics]{par}}.
#' @param rug A logical specifying whether to include an x-axis \dQuote{rug} (see \code{\link[graphics]{rug}}).
#' @param rug.col A character string specifying \code{col} to \code{\link[graphics]{rug}}.
#' @param rug.size A numeric value specifying \code{ticksize} to \code{\link[graphics]{rug}}.
#' @param \dots Additional arguments passed to \code{\link[graphics]{plot}}. 
#' @return A tidy data.frame containing the data used to draw the plot.
#' @examples
#' \dontrun{
#' require('datasets')
#' # prediction from several angles
#' m <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#' cplot(m)
#' 
#' # more complex model
#' m <- lm(Sepal.Length ~ Sepal.Width * Petal.Width * I(Petal.Width ^ 2), data = head(iris, 50))
#' ## marginal effect of 'Petal.Width' across 'Petal.Width'
#' cplot(m, x = "Petal.Width", what = "effect", n = 10)
#' ## marginal effect of 'Petal.Width' across 'Sepal.Width'
#' tmp <- cplot(m, x = "Sepal.Width", dx = "Petal.Width", what = "effect", n = 10)
#' 
#' # use ggplot2 instead of base graphics
#' \dontrun{
#' if (require("ggplot2")) {
#'     ggplot(tmp, aes(x = Petal.Width, y = "effect")) + geom_line(lwd = 2) + 
#'       geom_line(aes(y = effect + 1.96*se.effect)) + geom_line(aes(y = effect - 1.96*se.effect))
#' }
#' }
#' # a non-linear model
#' m <- glm(am ~ wt*drat, data = mtcars, family = binomial)
#' cplot(m, x = "wt") # prediction
#' 
#' # effects on linear predictor and outcome
#' cplot(m, x = "drat", dx = "wt", what = "effect", type = "link")
#' cplot(m, x = "drat", dx = "wt", what = "effect", type = "response")
#' }
#' @seealso \code{\link{plot.margins}}, \code{\link{persp.lm}}
#' @keywords graphics hplot
#' @importFrom graphics par plot lines rug polygon
#' @export
cplot <- function(object, ...) {
    UseMethod("cplot")
}

#' @rdname cplot
#' @export
cplot.lm <- 
function(object, 
         x = attributes(terms(object))[["term.labels"]][1],
         dx = x, 
         what = c("prediction", "effect"), 
         type = c("response", "link"), 
         method = c("simple", "Richardson", "complex"),
         n = 25L,
         level = 0.95,
         xlab = x, 
         ylab = if (match.arg(what) == "prediction") paste0("Predicted value") else paste0("Marginal effect of ", dx),
         xlim,
         ylim,
         lwd = 2,
         col = "black",
         lty = 1,
         se.type = c("lines", "shade"),
         se.col = "black",
         se.fill = grDevices::gray(.5,.5),
         se.lwd = 1,
         se.lty = if(match.arg(se.type) == "lines") 2 else 0,
         xaxs = "i",
         yaxs = xaxs,
         las = 1,
         rug = TRUE,
         rug.col = "black",
         rug.size = -0.02,
         ...) {
    
    dat <- object[["model"]]
    dat[] <- lapply(dat, as.numeric) # this probably isn't a good idea
    
    xvar <- x
    xvals <- seq(min(dat[[xvar]], na.rm = TRUE), 
                 max(dat[[xvar]], na.rm = TRUE), 
                 length.out = n)
    dxvar <- dx
    
    what <- match.arg(what)
    type <- match.arg(type)
    a <- (1 - level)/2
    fac <- qnorm(c(a, 1 - a))

    # setup `outdat` data
    if (what == "prediction") {
        # tmpdat <- build_datalist(dat, at = setNames(list(xvals), xvar))
        # outdat <- do.call("rbind", lapply(tmpdat, function(thisdat) {
            # s <- prediction(model = object, data = thisdat, type = type)
            # return(c(fitted = mean(s[["fitted"]], na.rm = TRUE),
                     # se = mean(s[["se.fitted"]], na.rm = TRUE)))
        # }))
        # outdat <- cbind(xvals, outdat)
        # colnames(outdat) <- c(xvar, "fitted", "se.fitted")
        # outdat <- as.data.frame(outdat)
        
        tmpdat <- structure(lapply(colMeans(dat[, names(dat) != xvar, drop = FALSE]), rep, length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, type = type, level = level)
        b1 <- outdat[["fitted"]] + (fac[1] * outdat[["se.fitted"]])
        b2 <- outdat[["fitted"]] + (fac[2] * outdat[["se.fitted"]])
    } else if (what == "effect") {
        tmpdat <- build_datalist(dat, at = setNames(list(xvals), xvar))
        outdat <- do.call("rbind", lapply(tmpdat, function(thisdat) {
            suppressMessages(s <- summary(margins(model = object, data = thisdat, type = type, method = method)[[1]]))
            return(c(effect = as.numeric(s[xvar, "dy/dx"]), se.effect = as.numeric(s[xvar, "Std.Err."])))
        }))
        outdat <- cbind(xvals, outdat)
        colnames(outdat) <- c(xvar, "effect", "se.effect")
        outdat <- as.data.frame(outdat)
        outdat[["se.effect"]] <- outdat[["se.effect"]]
        b1 <- outdat[["effect"]] + (fac[1] * outdat[["se.effect"]])
        b2 <- outdat[["effect"]] + (fac[2] * outdat[["se.effect"]])
    }

    # setup plot
    if (missing(xlim)) {
        xlim <- range(dat[[x]], na.rm = TRUE)
    }
    if (missing(ylim)) {
        tmp <- c(b1,b2)
        rng <- diff(range(tmp, na.rm = TRUE))
        ylim <- c(min(tmp, na.rm = TRUE) - (0.05 * rng), max(c(b1,b2), na.rm = TRUE) + (0.05 * rng))
        rm(tmp)
        rm(rng)
    }
    plot(NA, xlab = xlab, ylab = ylab, xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
    
    # uncertainty
    se.type <- match.arg(se.type)
    if (se.type == "lines") {
        lines(xvals, b1, type = "l", lwd = se.lwd, col = se.col, lty = se.lty)
        lines(xvals, b2, type = "l", lwd = se.lwd, col = se.col, lty = se.lty)
    } else {
        polygon(c(xvals, rev(xvals)), c(b1, rev(b2)), col = se.fill, border = se.col, lty = se.lty)
    }
    
    # prediction/effect line
    lines(xvals, if (what == "prediction") outdat[["fitted"]] else outdat[["effect"]], type = "l", lwd = lwd, col = col, lty = lty)

    #
    if (isTRUE(rug)) {
        rug(dat[[x]], ticksize = rug.size, col = rug.col)
    }
    
    # return data used in plot
    invisible(outdat)
}

#' @rdname cplot
#' @export
cplot.glm <- cplot.lm

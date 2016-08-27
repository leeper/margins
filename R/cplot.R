#' @rdname cplot
#' @title Conditional predicted value and average marginal effect plots for models
#' @description Draw one or more conditioanl effects plots reflecting predictions or marginal effects from a model, conditional on a covariate. Currently methods exist for \dQuote{lm} and \dQuote{glm} models.
#' @param object A model object.
#' @param x A character string specifying the name of variable to use as the x-axis dimension in the plot.
#' @param dx If \code{what = "effect"}, the variable whose conditional marginal effect should be displayed. By default it is \code{x} (so the plot displays the marginal effect of \code{x} across values of \code{x}); ignored otherwise.
#' @param what A character string specifying whether to draw a \dQuote{prediction} (fitted values from the model, calculated using \code{\link[stats]{predict}}) or an \dQuote{effect} (average marginal effect of \code{dx} conditional on \code{x}, using \code{\link{margins}}).
#' @param type A character string specifying whether to calculate predictions on the response scale (default) or link (only relevant for non-linear models).
#' @param data A data frame to override the default value offered in \code{object[["model"]]}.
#' @param at Currently ignored.
#' @param method If \code{what = "effect"}, a character string indicating the numeric derivative method to use when estimating marginal effects. \dQuote{simple} optimizes for speed; \dQuote{Richardson} optimizes for accuracy. See \code{\link[numDeriv]{grad}} for details.
#' @param n An integer specifying the number of points across \code{x} at which to calculate the predicted value or marginal effect.
#' @param level The confidence level required (used to draw uncertainty bounds).
#' @param draw A logical (default \code{TRUE}), specifying whether to draw the plot. If \code{FALSE}, the data used in drawing are returned as a list of data.frames. This might be useful if you want to plot using an alternative plotting package (e.g., ggplot2).
#' @param xlab A character string specifying the value of \code{xlab} in \code{\link[graphics]{plot}}. 
#' @param ylab A character string specifying the value of \code{ylab} in \code{\link[graphics]{plot}}. 
#' @param xlim A two-element numeric vector specifying the x-axis limits. Set automatically if missing.
#' @param ylim A two-element numeric vector specifying the y-axis limits. Set automatically if missing.
#' @param lwd An integer specifying the width of the prediction or marginal effect line. See \code{\link[graphics]{lines}}. If \code{x} is a factor variable in the model, this is used to set the line width of the error bars.
#' @param col A character string specifying the color of the prediction or marginal effect line. If \code{x} is a factor variable in the model, this is used to set the color of the error bars.
#' @param lty An integer specifying the \dQuote{line type} of the prediction or marginal effect line. See \code{\link[graphics]{par}}. If \code{x} is a factor variable in the model, this is used to set the line type of the error bars.
#' @param se.type A character string specifying whether to draw the confidence interval as \dQuote{lines} (the default, using \code{\link[graphics]{lines}}) or a \dQuote{shade} (using \code{\link[graphics]{polygon}}).
#' @param se.col If \code{se.type = "lines"}, a character string specifying the color of the confidence interval lines. If \code{se.type = "shade"}, the color of the shaded region border.
#' @param se.fill If \code{se.type = "shade"}, the color of the shaded region. Ignored otherwise.
#' @param se.lwd If \code{se.type = "lines"}, the width of the confidence interval lines. See \code{\link[graphics]{lines}}.
#' @param se.lty If \code{se.type = "lines"}, an integer specifying the \dQuote{line type} of the confidence interval lines; if \code{se.type = "shade"}, the line type of the shaded polygon border. See \code{\link[graphics]{par}}.
#' @param factor.pch If \code{x} is a factor variable in the model, the shape to use when drawing points. See \code{\link[graphics]{points}}.
#' @param factor.col If \code{x} is a factor variable in the model, the color to use for the border of the points. See \code{\link[graphics]{points}}.
#' @param factor.fill If \code{x} is a factor variable in the model, the color to use for the fill of the points. See \code{\link[graphics]{points}}.
#' @param factor.cex If \code{x} is a factor variable in the model, the \dQuote{expansion factor} to use for the point size. See \code{\link[graphics]{points}}.
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
#'
#' # marginal effect of 'Petal.Width' across 'Sepal.Width'
#' ## without drawing the plot
#' ## this might be useful for using, e.g., ggplot2 for plotting
#' tmp <- cplot(m, x = "Sepal.Width", dx = "Petal.Width", what = "effect", n = 10, draw = FALSE)
#' if (require("ggplot2")) {
#'     # use ggplot2 instead of base graphics
#'     ggplot(tmp, aes(x = Petal.Width, y = "effect")) + geom_line(lwd = 2) + 
#'       geom_line(aes(y = effect + 1.96*se.effect)) + geom_line(aes(y = effect - 1.96*se.effect))
#' }
#' # a non-linear model
#' m <- glm(am ~ wt*drat, data = mtcars, family = binomial)
#' cplot(m, x = "wt") # prediction
#' 
#' # effects on linear predictor and outcome
#' cplot(m, x = "drat", dx = "wt", what = "effect", type = "link")
#' cplot(m, x = "drat", dx = "wt", what = "effect", type = "response")
#' 
#' }
#' @seealso \code{\link{plot.margins}}, \code{\link{persp.lm}}
#' @keywords graphics hplot
#' @importFrom graphics par plot lines rug polygon segments points
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
         data = object[["model"]],
         at,
         method = c("simple", "Richardson", "complex"),
         n = 25L,
         level = 0.95,
         draw = TRUE,
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
         se.lwd = lwd,
         se.lty = if(match.arg(se.type) == "lines") 2 else 0,
         factor.pch = 19,
         factor.col = se.col,
         factor.fill = factor.col,
         factor.cex = 1,
         xaxs = "i",
         yaxs = xaxs,
         las = 1,
         rug = TRUE,
         rug.col = "black",
         rug.size = -0.02,
         ...) {
    
    xvar <- x
    
    # handle factors
    classes <- attributes(terms(object))[["dataClasses"]][-1]
    classes[classes == "character"] <- "factor"
    nnames <- clean_terms(names(classes)[classes != "factor"])
    fnames <- clean_terms(names(classes)[classes == "factor"])
    fnames2 <- names(classes)[classes == "factor"] # for checking stupid variable naming behavior by R
    x_is_factor <- (xvar %in% c(fnames, fnames2))
    
    # subset data
    dat <- data[, c(nnames, fnames2), drop = FALSE]
    names(dat)[names(dat) %in% fnames2] <- fnames
    
    # setup x (based on whether factor)
    if (isTRUE(x_is_factor)) {
        if (is.factor(dat[["xvar"]])) {
            xvals <- as.character(levels(dat[[clean_terms(xvar)]]))
        } else {
            xvals <- as.character(unique(dat[[clean_terms(xvar)]]))
        }
    } else {
        xvals <- seq(min(dat[[xvar]], na.rm = TRUE), 
                     max(dat[[xvar]], na.rm = TRUE), 
                     length.out = n)
    } 
    
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
        
        tmpdat <- structure(lapply(colMeans(dat[, names(dat) != xvar, drop = FALSE], na.rm = TRUE), rep, length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, type = type, level = level)
        out <- list(structure(list(xvals = xvals,
                                   yvals = outdat[["fitted"]],
                                   upper = outdat[["fitted"]] + (fac[1] * outdat[["se.fitted"]]),
                                   lower = outdat[["fitted"]] + (fac[2] * outdat[["se.fitted"]])),
                              class = "data.frame", row.names = seq_along(outdat[["fitted"]])))
    } else if (what == "effect") {
    
        dxvar <- dx
        
        suppressMessages(s <- summary(margins(model = object, data = data, at = setNames(list(xvals), xvar), type = type, method = method)))
        outdat <- do.call("rbind.data.frame", lapply(s, function(thismargin) {
            c(effect = as.numeric(thismargin[dx, "dy/dx"]), 
              se.effect = as.numeric(thismargin[dx, "Std.Err."]))
        }))
        out <- list(structure(list(xvals = xvals,
                                   yvals = outdat[[1]],
                                   upper = outdat[[1]] + (fac[2] * outdat[[2]]),
                                   lower = outdat[[1]] + (fac[1] * outdat[[2]])),
                              class = "data.frame", row.names = seq_len(nrow(outdat))))
    }
    
    # optionally draw the plot; if FALSE, just the data are returned
    if (isTRUE(draw)) {

        # setup plot
        if (missing(xlim)) {
            if (isTRUE(x_is_factor)) {
                xlim <- c(0.75, length(xvals) + 0.25)
            } else {
                xlim <- range(dat[[x]], na.rm = TRUE)
            }
        }
        if (missing(ylim)) {
            tmp <- unlist(lapply(out, function(one) {
                range(c(one[["upper"]], one[["lower"]]), na.rm = TRUE)
            }))
            rng <- diff(range(tmp, na.rm = TRUE))
            ylim <- c(min(tmp, na.rm = TRUE) - (0.05 * rng), max(tmp, na.rm = TRUE) + (0.05 * rng))
            rm(tmp)
            rm(rng)
        }
        if (isTRUE(x_is_factor)) {
            plot(NA, xlab = xlab, ylab = ylab, xaxt = "n", xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
            axis(1, at = seq_along(xvals), labels = xvals)
        } else {
            plot(NA, xlab = xlab, ylab = ylab, xaxs = xaxs, yaxs = yaxs, las = las, xlim = xlim, ylim = ylim, ...)
        }
        
        if (isTRUE(rug)) {
            rug(dat[[x]], ticksize = rug.size, col = rug.col)
        }
        
        se.type <- match.arg(se.type)
        
        # function to draw one set of lines
        draw_one <- function(xvals, yvals, upper, lower) {
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
        
        # draw
        lapply(out, function(linelist) {
            draw_one(xvals = linelist[["xvals"]], 
                     yvals = linelist[["yvals"]], 
                     upper = linelist[["upper"]], 
                     lower = linelist[["lower"]])
        })
    }
    # return data used in plot
    invisible(out)
}

#' @rdname cplot
#' @export
cplot.glm <- cplot.lm

#' @rdname cplot
#' @export
cplot.loess <- cplot.lm

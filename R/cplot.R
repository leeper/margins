#' @rdname cplot
#' @title Conditional predicted value and average marginal effect plots for models
#' @description Draw one or more conditioanl effects plots reflecting predictions or marginal effects from a model, conditional on a covariate. Currently methods exist for \dQuote{lm}, \dQuote{glm}, \dQuote{loess} class models.
#' @param object A model object.
#' @param x A character string specifying the name of variable to use as the x-axis dimension in the plot.
#' @param dx If \code{what = "effect"}, the variable whose conditional marginal effect should be displayed. By default it is \code{x} (so the plot displays the marginal effect of \code{x} across values of \code{x}); ignored otherwise. If \code{dx} is a factor with more than 2 levels, an error will be issued.
#' @param what A character string specifying whether to draw a \dQuote{prediction} (fitted values from the model, calculated using \code{\link[stats]{predict}}) or an \dQuote{effect} (average marginal effect of \code{dx} conditional on \code{x}, using \code{\link{margins}}).
#' @param data A data frame to override the default value offered in \code{object[["model"]]}.
#' @param type A character string specifying whether to calculate predictions on the response scale (default) or link (only relevant for non-linear models).
#' @param vcov A matrix containing the variance-covariance matrix for estimated model coefficients, or a function to perform the estimation with \code{model} as its only argument.
#' @param at Currently ignored.
#' @param n An integer specifying the number of points across \code{x} at which to calculate the predicted value or marginal effect.
#' @param xvals A numeric vector of values at which to calculate predictions or marginal effects. By default, it is calculated from the data using \code{\link{seq_range}}.
#' @param level The confidence level required (used to draw uncertainty bounds).
#' @param draw A logical (default \code{TRUE}), specifying whether to draw the plot. If \code{FALSE}, the data used in drawing are returned as a list of data.frames. This might be useful if you want to plot using an alternative plotting package (e.g., ggplot2). Also, if set to value \dQuote{add}, then the resulting data is added to the existing plot.
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
#' @param scatter A logical indicating whether to plot the observed data in \code{data} as a scatterplot.
#' @param scatter.pch If \code{scatter = TRUE}, an integer specifying a shape to use for plotting the data. See \code{\link[graphics]{points}}.
#' @param scatter.col If \code{scatter = TRUE}, a character string specifying a color to use for plotting the data. See \code{\link[graphics]{points}}.
#' @param scatter.bg If \code{scatter = TRUE}, a character string specifying a color to use for plotting the data. See \code{\link[graphics]{points}}.
#' @param scatter.cex If \code{scatter = TRUE}, an integer specifying the size of the points. See \code{\link[graphics]{points}}.
#' @param rug A logical specifying whether to include an x-axis \dQuote{rug} (see \code{\link[graphics]{rug}}).
#' @param rug.col A character string specifying \code{col} to \code{\link[graphics]{rug}}.
#' @param rug.size A numeric value specifying \code{ticksize} to \code{\link[graphics]{rug}}.
#' @param \dots Additional arguments passed to \code{\link[graphics]{plot}}. 
#' @details This is somewhat similar to to the output produced by the \code{marginalModelPlot()} function in the \bold{\href{https://cran.r-project.org/package=MTurkR}{car}} package.
#' @return A tidy data.frame containing the data used to draw the plot.
#' @examples
#' \dontrun{
#' require('datasets')
#' # prediction from several angles
#' m <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#' cplot(m)
#' 
#' # more complex model
#' m <- lm(Sepal.Length ~ Sepal.Width * Petal.Width * I(Petal.Width ^ 2), 
#'         data = head(iris, 50))
#' ## marginal effect of 'Petal.Width' across 'Petal.Width'
#' cplot(m, x = "Petal.Width", what = "effect", n = 10)
#'
#' # factor independent variables
#' mtcars[["am"]] <- factor(mtcars[["am"]])
#' m <- lm(mpg ~ am * wt, data = mtcars)
#' ## predicted values for each factor level
#' cplot(m, x = "am")
#' ## marginal effect of each factor level across numeric variable
#' cplot(m, x = "wt", dx = "am", what = "effect")
#' 
#' # marginal effect of 'Petal.Width' across 'Sepal.Width'
#' ## without drawing the plot
#' ## this might be useful for using, e.g., ggplot2 for plotting
#' tmp <- cplot(m, x = "Sepal.Width", dx = "Petal.Width", 
#'              what = "effect", n = 10, draw = FALSE)
#' if (require("ggplot2")) {
#'   # use ggplot2 instead of base graphics
#'   ggplot(tmp, aes(x = Petal.Width, y = "effect")) + 
#'          geom_line(lwd = 2) + 
#'          geom_line(aes(y = effect + 1.96*se.effect)) + 
#'          geom_line(aes(y = effect - 1.96*se.effect))
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
#' @importFrom prediction prediction find_data
#' @export
cplot <- function(object, ...) {
    UseMethod("cplot")
}

#' @rdname cplot
#' @export
cplot.lm <- 
function(object, 
         x = attributes(terms(object))[["term.labels"]][1L],
         dx = x, 
         what = c("prediction", "effect"), 
         data = prediction::find_data(object),
         type = c("response", "link"), 
         vcov = stats::vcov(object),
         at,
         n = 25L,
         xvals = seq_range(data[[x]], n = n),
         level = 0.95,
         draw = TRUE,
         xlab = x, 
         ylab = if (match.arg(what) == "prediction") paste0("Predicted value") else paste0("Marginal effect of ", dx),
         xlim = NULL,
         ylim = NULL,
         lwd = 1L,
         col = "black",
         lty = 1L,
         se.type = c("shade", "lines"),
         se.col = "black",
         se.fill = grDevices::gray(.5,.5),
         se.lwd = lwd,
         se.lty = if(match.arg(se.type) == "lines") 1L else 0L,
         factor.pch = 19L,
         factor.col = se.col,
         factor.fill = factor.col,
         factor.cex = 1L,
         xaxs = "i",
         yaxs = xaxs,
         las = 1L,
         scatter = FALSE,
         scatter.pch = 19L,
         scatter.col = se.col,
         scatter.bg = scatter.col,
         scatter.cex = 0.5,
         rug = TRUE,
         rug.col = col,
         rug.size = -0.02,
         ...) {
    
    xvar <- x
    yvar <- as.character(attributes(terms(object))[["variables"]][[2]])
    
    # handle factors
    classes <- attributes(terms(object))[["dataClasses"]][-1]
    classes[classes == "character"] <- "factor"
    nnames <- clean_terms(names(classes)[classes != "factor"])
    fnames <- clean_terms(names(classes)[classes == "factor"])
    fnames2 <- names(classes)[classes == "factor"] # for checking stupid variable naming behavior by R
    x_is_factor <- (xvar %in% c(fnames, fnames2))
    dx_is_factor <- (dx %in% c(fnames, fnames2))
    
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
        xvals <- xvals
    } 
    
    what <- match.arg(what)
    type <- match.arg(type)
    a <- (1 - level)/2
    fac <- qnorm(c(a, 1 - a))

    # setup `outdat` data
    if (what == "prediction") {
        mean_or_mode <- function(x) {
            if (is.factor(x)) {
                factor(names(sort(table(x), descending = TRUE))[1L], levels = levels(x))
            } else {
                mean(x, na.rm = TRUE)
            }
        }
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], mean_or_mode)
        tmpdat <- structure(lapply(tmpdat, rep, length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, type = type, level = level)
        out <- structure(list(xvals = xvals,
                              yvals = outdat[["fitted"]],
                              upper = outdat[["fitted"]] + (fac[2] * outdat[["se.fitted"]]),
                              lower = outdat[["fitted"]] + (fac[1] * outdat[["se.fitted"]])),
                         class = "data.frame", row.names = seq_along(outdat[["fitted"]]))
    } else if (what == "effect") {
        if (is.factor(dat[[dx]]) && nlevels(data[[dx]]) > 2L) {
            stop("Displaying effect of a factor variable with > 2 levels is not currently supported!")
        }
        summ <- summary(margins(model = object, data = data, at = setNames(list(xvals), xvar), type = type, vcov = vcov))
        outdat <- do.call("rbind.data.frame", lapply(summ, function(thismargin) {
            c(effect = as.numeric(thismargin[dx, "dy/dx"]), 
              se.effect = as.numeric(thismargin[dx, "Std.Err."]))
        }))
        out <- structure(list(xvals = xvals,
                              yvals = outdat[[1]],
                              upper = outdat[[1]] + (fac[2] * outdat[[2]]),
                              lower = outdat[[1]] + (fac[1] * outdat[[2]])),
                         class = "data.frame", row.names = seq_len(nrow(outdat)))
    }
    
    # optionally draw the plot; if FALSE, just the data are returned
    if (isTRUE(draw)) {
        setup_cplot(plotdat = out, data = data, xvals = xvals, xvar = xvar, yvar = yvar,
                    xlim = xlim, ylim = ylim, x_is_factor = x_is_factor,
                    xlab = xlab, ylab = ylab, xaxs = xaxs, yaxs = yaxs, las = las,
                    scatter = scatter, scatter.pch = scatter.pch, scatter.col = scatter.col, ...)
    }
    if (isTRUE(draw) || draw == "add") {
        draw_one(xvals = out[["xvals"]], 
                 yvals = out[["yvals"]], 
                 upper = out[["upper"]], 
                 lower = out[["lower"]],
                 x_is_factor = x_is_factor,
                 col = col, lty = lty, lwd = lwd,
                 se.type = match.arg(se.type),
                 factor.pch = factor.pch, factor.fill = factor.fill, 
                 factor.col = factor.col, factor.cex = factor.cex,
                 se.lwd = se.lwd, se.fill = se.fill, se.col = se.col, se.lty = se.lty)
        if (isTRUE(rug)) {
            draw_rug(data[[x]], rug.size = rug.size, rug.col = rug.col)
        }
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

#' @rdname cplot
#' @title Conditional predicted value and average marginal effect plots for models
#' @description Draw one or more conditional effects plots reflecting
#'   model coefficients, or a function to perform the estimation with
#'   \code{model} as its only argument.
#' @param at Currently ignored.
#' @param n An integer specifying the number of points across \code{x} at which
#'   to calculate the predicted value or marginal effect, when \code{x} is
#'   numeric. Ignored otherwise.
#' @param xvals A numeric vector of values at which to calculate predictions or
#'   marginal effects, if \code{x} is numeric. By default, it is calculated from
#'   the data using \code{\link{seq_range}}. If \code{x} is a factor, this is
#'   ignored, as is \code{n}.
#' @param level The confidence level required (used to draw uncertainty
#'   bounds).
#' @param draw A logical (default \code{TRUE}), specifying whether to draw the
#'   plot. If \code{FALSE}, the data used in drawing are returned as a list of
#'   data.frames. This might be useful if you want to plot using an alternative
#'   plotting package (e.g., ggplot2). Also, if set to value \dQuote{add}, then
#'   the resulting data is added to the existing plot.
#' @param size 
#' @param colour 
#' @param linetype 
#' @param rugplot logical include a rugplot at the bottom of the graph 
#' @param \dots Additional arguments passed to
#'   \code{\link[ggplot2]{geom_pointrange}} if `x` is a factor, or
#'   \code{\link[ggplot2]{geom_ribbon}} if `x` is continuous. For example,
#'   \code{shape}, \code{fill}, \code{alpha}.
#'
#' @details Note that when \code{what = "prediction"}, the plots show
#' predictions holding values of the data at their mean or mode, whereas when
#' \code{what = "effect"} average marginal effects (i.e., at observed values)
#' are shown.
#' 
#' When examining generalized linear models (e.g., logistic regression models),
#' confidence intervals for predictions can fall outside of the response scale
#' (again, for logistic regression this means confidence intervals can exceed
#' the (0,1) bounds). This is consistent with the behavior of
#' \code{\link[stats]{predict}} but may not be desired. The examples (below)
#' show ways of constraining confidence intervals to these bounds.
#' 
#' The overall aesthetic is somewhat similar to to the output produced by the
#' \code{marginalModelPlot()} function in the
#' \bold{\href{https://cran.r-project.org/package=car}{car}} package.
#' 
#' @return A tidy data frame containing the data used to draw the plot. Use
#' \code{draw = FALSE} to simply generate the data structure for use elsewhere.
#'
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
#' 
#' # a non-linear model
#' m <- glm(am ~ wt*drat, data = mtcars, family = binomial)
#' cplot(m, x = "wt") # prediction (response scale)
#' cplot(m, x = "wt") # prediction (link scale)
#' if (require("ggplot2")) {
#'   # prediction (response scale, constrained to [0,1])
#'   cplotdat <- cplot(m, x = "wt", type = "link", draw = FALSE)
#'   ggplot(cplotdat, aes(x = xvals, y = plogis(yvals))) + 
#'          geom_line(lwd = 1.5) + 
#'          geom_line(aes(y = plogis(upper))) + 
#'          geom_line(aes(y = plotis(lower)))
#' }
#' 
#' # effects on linear predictor and outcome
#' cplot(m, x = "drat", dx = "wt", what = "effect", type = "link")
#' cplot(m, x = "drat", dx = "wt", what = "effect", type = "response")
#' 
#' # plot conditional predictions across a third factor
#' local({
#'   iris$long <- rbinom(nrow(iris), 1, 0.6)
#'   x <- glm(long ~ Sepal.Width*Species, data = iris)
#'   cplot(x, x = "Sepal.Width", data = iris[iris$Species == "setosa", ], 
#'         ylim = c(0,1), col = "red", se.fill = rgb(1,0,0,.5), xlim = c(2,4.5))
#'   cplot(x, x = "Sepal.Width", data = iris[iris$Species == "versicolor", ], 
#'         draw = "add", col = "blue", se.fill = rgb(0,1,0,.5))
#'   cplot(x, x = "Sepal.Width", data = iris[iris$Species == "virginica", ], 
#'         draw = "add", col = "green", se.fill = rgb(0,0,1,.5))
#' })
#' 
#' # ordinal outcome
#' if (require("MASS")) {
#'   # x is a factor variable
#'   house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, 
#'                     data = housing)
#'   ## predicted probabilities
#'   cplot(house.plr)
#'   ## cumulative predicted probabilities
#'   cplot(house.plr, what = "stacked")
#'   ## ggplot2 example
#'   if (require("ggplot2")) {
#'     ggplot(cplot(house.plr), aes(x = xvals, y = yvals, group = level)) + 
#'       geom_line(aes(color = level))
#'   }
#'
#'   # x is continuous
#'   cyl.plr <- polr(factor(cyl) ~ wt, data = mtcars)
#'   cplot(cyl.plr, col = c("red", "purple", "blue"), what = "stacked")
#'   cplot(cyl.plr, what = "class")
#' }
#' 
#' }
#' @seealso \code{\link{plot.margins}}, \code{\link{persp.lm}}
#' @keywords graphics
#' @importFrom ggplot2 ggplot geom_line geom_ribbon geom_pointrange geom_rug xlab ylab theme_minimal
#' @importFrom utils head
#' @importFrom graphics par plot lines rug polygon segments points
#' @importFrom prediction prediction find_data seq_range mean_or_mode
#' @export
cplot <- function(object, 
                  x = NULL,
                  dx = NULL, 
                  data = NULL,
                  what = c("prediction", "effect", "classprediction", "stackedprediction"), 
                  type = c("response", "link"), 
                  vcov = stats::vcov(object),
                  level = 0.95,
                  draw = TRUE,
                  colour = 'black',
                  linetype = 1,
                  size = 0.5,
                  xvals = NULL,
                  n = 25,
                  rugplot = TRUE,
                  at = NULL,
                  ...) {
                
    # input checks

    # default values
    if (is.null(data)) {
        data <- prediction::find_data(object)
    }
    if (is.null(x)) {
        x <- attributes(terms(object))[["term.labels"]][1L]
    }
    xvar <- x
    if (is.null(dx)) {
        dx <- x
    }
    what <- match.arg(what)
    if (what == 'prediction') {
        ylabel <- 'Predicted value'
    } else if (what == 'effect') {
        ylabel <- paste0('Marginal effect of ', dx)
    } else if (what == 'stackedprediction') {
        ylabel <- 'Predicted value'
    } else if (what == 'classprediction') {
        ylabel <- 'Predicted class'
    }

    xlabel <- xvar
    type <- match.arg(type)

    # prepare data for plotting
    out <- cplot_extract(object = object, 
                         data = data, 
                         xvar = xvar, 
                         dx = dx, 
                         what = what, 
                         type = type, 
                         xvals = xvals,
                         vcov = vcov,
                         at = at,
                         n = n,
                         level = level)

    # plot
    if (isTRUE(draw)) {

        # save for future queries (e.g., levels)
        outdat <- out 

        out <- ggplot(outdat, aes(x = xvals, y = yvals))

        # x is numeric -> geom_line + geom_ribbon
        if (is.numeric(outdat$xvals)) {

            # geom_line for estimates
            out  <- out +
                    geom_line(size = size, colour = colour, linetype = linetype)

            # rugplot
            if (rugplot) {
                rugdat <- data.frame('x' = data[[xvar]])
                out <- out +
                       geom_rug(data = rugdat, aes(x = x), inherit.aes=FALSE)
            }

            # confidence intervals are available
            if (all(c('lower', 'upper') %in% names(outdat))) {

                # geom_ribbon for confidence intervals (user-supplied alpha fill)
                extra_args <- list(...)
                if (all(c('alpha', 'fill') %in% names(extra_args))) {
                    out <- out +
                           geom_ribbon(aes(ymin = lower, ymax = upper), 
                                       ...) 
                } else if ('alpha' %in% names(extra_args)) {
                    out <- out +
                           geom_ribbon(aes(ymin = lower, ymax = upper), 
                                       fill = 'grey', ...) 
                } else if ('fill' %in% names(extra_args)) {
                    out <- out +
                           geom_ribbon(aes(ymin = lower, ymax = upper), 
                                       alpha = .3, ...) 
                } else {
                    out <- out +
                           geom_ribbon(aes(ymin = lower, ymax = upper), 
                                       alpha = .3, fill = 'grey', ...) 
                }

            }


        # x is not numeric -> geom_pointrange or geom_point
        } else {

            # confidence intervals are available
            if (all(c('lower', 'upper') %in% names(outdat))) {
                out <- out +
                       geom_pointrange(aes(ymin = lower, ymax = upper),
                                       size = size, colour = colour, linetype = linetype, ...)
            } else {
                out <- out +
                       geom_point(size = size, colour = colour, ...)
            }

        }

        # finish plot 
        out <- out +
               xlab(xlabel) + 
               ylab(ylabel) +
               theme_minimal()

        # facet_wrap if `level` is in the extracted data
        if ('level' %in% names(outdat)) {
            out <- out +
                   facet_wrap(~ level)
        }

    }

    # output
    return(out)
}

#' Generic extracts model information for use by `cplot`
#'
#' @export
cplot_extract <- function(object, ...) {
    UseMethod("cplot_extract")
}

#' Internal function to extract data for `cplot`
#'
#' @inheritParams cplot
cplot_extract.default <- function(object, 
                                  data, 
                                  dx, 
                                  level, 
                                  xvar, 
                                  at,
                                  n,
                                  type, 
                                  xvals,
                                  vcov,
                                  what,
                                  ...) {

    # handle factors and subset data
    data <- force(data)
    f <- check_factors(object, data, xvar = xvar, dx = dx)
    x_is_factor <- f[["x_is_factor"]]
    dx_is_factor <- f[["dx_is_factor"]]
    dat <- f[["data"]]

    # setup xvals (based on whether factor)
    if (is.null(xvals)) {
        if (isTRUE(x_is_factor)) {
            if (is.factor(dat[[xvar]])) {
                xvals <- as.character(levels(dat[[clean_terms(xvar)]]))
            } else {
                xvals <- as.character(unique(dat[[clean_terms(xvar)]]))
            }
        } else {
            xvals <- prediction::seq_range(data[[xvar]], n = n)
        } 
    }

    at <- setNames(list(xvals), xvar)
   
    a <- (1 - level)/2
    fac <- qnorm(c(a, 1 - a))
    
    if (what == "prediction") {

        # generates predictions as mean/mode of all variables rather than average prediction!
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], prediction::mean_or_mode)
        tmpdat <- structure(lapply(tmpdat, rep, length.out = length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, type = type, level = level, vcov = vcov)
        out <- structure(list(xvals = xvals,
                              yvals = outdat[["fitted"]],
                              upper = outdat[["fitted"]] + (fac[2] * outdat[["se.fitted"]]),
                              lower = outdat[["fitted"]] + (fac[1] * outdat[["se.fitted"]])),
                         class = "data.frame", row.names = seq_along(outdat[["fitted"]]))
    } else if (what == "effect") {
        if (is.factor(dat[[dx]]) && nlevels(data[[dx]]) > 2L) {
            stop("Displaying effect of a factor variable with > 2 levels is not currently supported!")
        }
        marg <- margins(model = object, data = data, at = at, type = type, vcov = vcov)
        out <- summary(marg)[ , c(xvar, "AME", "upper", "lower", "factor"), drop = FALSE]
        out <- setNames(out[out[["factor"]] == dx, , drop = FALSE], c("xvals", "yvals", "upper", "lower", "factor"))
    }

    return(out)
}

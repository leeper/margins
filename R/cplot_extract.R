#' Generic extracts model information for use by `cplot`
#'
#' @rdname cplot_extract
#' @inheritParams cplot
#' @param xvar The name of the variable to show on the x-axis
#' @param zvar name of the third dimension variable over which quantities should
#'   be plotted (as facets).
#' @export
cplot_extract <- function(object, ...) {
    UseMethod("cplot_extract")
}

#' Internal function to extract data for `cplot`
#'
#' @rdname cplot_extract
#' @inheritParams cplot 
#' @inheritParams cplot_extract
cplot_extract.default <- function(object, 
                                  data, 
                                  dx, 
                                  level, 
                                  xvar, 
                                  zvar,
                                  xvals,
                                  zvals,
                                  at,
                                  n,
                                  type, 
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

    # at argument
    at <- setNames(list(xvals), xvar)
    z_valid <- !is.null(zvar) & !is.null(zvals)
    if (z_valid) {
        at[[zvar]] <- zvals
    }
   
    # confidence level
    a <- (1 - level)/2
    fac <- qnorm(c(a, 1 - a))
    
    if (what == "prediction") {

        # generates predictions as mean/mode of all variables rather than average prediction!
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], prediction::mean_or_mode)

        # data.frame with all combinations of xvals, zvals, and mean/mode values 
        tmpdat[[xvar]] <- xvals
        if (z_valid) {
            tmpdat[[zvar]] <- zvals
        }
        tmpdat <- expand.grid(tmpdat, stringsAsFactors = FALSE)

        # predicted values
        outdat <- prediction(model = object, data = tmpdat, type = type, level = level, vcov = vcov)

        # output
        out <- structure(list(xvals = outdat[[xvar]],
                              yvals = outdat[["fitted"]],
                              upper = outdat[["fitted"]] + (fac[2] * outdat[["se.fitted"]]),
                              lower = outdat[["fitted"]] + (fac[1] * outdat[["se.fitted"]])),
                         class = "data.frame", row.names = seq_along(outdat[["fitted"]]))
        if (z_valid) {
            out[['zvals']] <- outdat[[zvar]]
        }

    } else if (what == "effect") {

        if (is.factor(dat[[dx]]) && nlevels(data[[dx]]) > 2L) {
            stop("Displaying effect of a factor variable with > 2 levels is not currently supported!")
        }

        marg <- margins(model = object, data = data, at = at, type = type, vcov = vcov)

        if (!z_valid) {
            out <- summary(marg, level = level)[ , c(xvar, "AME", "upper", "lower", "factor"), drop = FALSE]
            out <- setNames(out[out[["factor"]] == dx, , drop = FALSE], 
                            c("xvals", "yvals", "upper", "lower", "factor"))
        } else {
            out <- summary(marg, level = level)[ , c(xvar, zvar, "AME", "upper", "lower", "factor"), drop = FALSE]
            out <- setNames(out[out[["factor"]] == dx, , drop = FALSE], 
                            c("xvals", "zvals", "yvals", "upper", "lower", "factor"))
        }

    }

    return(out)
}

#' @inheritParams cplot
#' @export
cplot_extract.polr <-
function(object, 
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
    f <- check_factors(object = object, data = data, xvar = xvar, dx = dx)
    x_is_factor <- f[["x_is_factor"]]
    dx_is_factor <- f[["dx_is_factor"]]
    dat <- f[["data"]]
    
    # setup x (based on whether factor)
    if (is.null(xvals)) {
        if (isTRUE(x_is_factor)) {
            if (is.factor(dat[["xvar"]])) {
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


    if (what %in% c("prediction", "classprediction", "stackedprediction")) {

        # generates predictions as mean/mode of all variables rather than average prediction!
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], mean_or_mode)
        tmpdat <- structure(lapply(tmpdat, rep, length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, level = level)

        if (what == 'prediction') {
            out <- structure(list(xvals = xvals,
                                  yvals = outdat[["fitted"]]),
                             class = "data.frame", 
                             row.names = seq_along(outdat[["fitted"]]))

        } else if (what == "classprediction") {
            out <- structure(list(xvals = xvals,
                                  yvals = outdat[["fitted.class"]]),
                             class = "data.frame", 
                             row.names = seq_along(outdat[["fitted.class"]]))

        } else if (what == "stackedprediction"){ 
            out <- list()
            for (i in grep('^Pr\\(', names(outdat))) {
                out[[i]] <- data.frame(xvals = xvals,
                                       yvals = outdat[[i]],
                                       level = names(outdat)[i],
                                       stringsAsFactors = FALSE)
            }
            out <- do.call('rbind', out)
        }

    } else if (what == "effect") {
        stop("Displaying marginal effects is not currently supported for 'polr' models.")
    }

    return(out)
}

#' @rdname cplot
#' @export
cplot_extract.multinom <- cplot_extract.polr

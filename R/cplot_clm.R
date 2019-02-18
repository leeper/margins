#' @inheritParams cplot
#' @export
cplot_extract.clm <-
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
    f <- check_factors(object = object, data = data, xvar = xvar, dx = dx) #ok
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

    # setup `outdat` data
    if (what %in% c("prediction", "classprediction", "stackedprediction")) {
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], mean_or_mode)
        tmpdat <- structure(lapply(tmpdat, rep, length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, level = level)
        if (what == "classprediction") {
            out <- structure(list(xvals = xvals,
                                  yvals = outdat[["fitted.class"]]),
                             class = "data.frame", 
                             row.names = seq_along(outdat[["fitted.class"]]))
        } else {
            out <- list()
            preds <- grep("Pr", names(outdat))
            for (i in preds) {
                if (what == "stackedprediction" && i != preds[1L]) {
                    outdat[[i]] <- outdat[[i]] + outdat[[i - 1L]]
                }
                out[[i - preds[1] + 1]] <- structure(list(xvals = xvals,
                                              yvals = outdat[[i]],
                                              level = names(outdat)[i]),
                                      class = "data.frame", 
                                      row.names = seq_along(outdat[["fitted"]]))
            }
            out <- do.call("rbind", out)
        }
    } else if (what == "effect") {
        stop("Displaying marginal effects is not currently supported for 'clm' models!")
    }
    
    # return data used in plot
    return(out)

}

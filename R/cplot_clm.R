#' @rdname cplot
#' @importFrom stats stepfun
#' @export
cplot.clm <-
function(object, 
         x = attributes(terms(object))[["term.labels"]][1L], #ok
         dx = x,                                              #ok
         what = c("prediction", "classprediction", "stackedprediction", "effect"), 
         data = prediction::find_data(object), #ok
         type = c("response", "link"), 
         vcov = stats::vcov(object), # different order to polr
         at,
         n = 25L,
         xvals = seq_range(data[[x]], n = n),  
         level = 0.95,
         draw = TRUE,
         xlab = x, 
         ylab = if (match.arg(what) == "effect") paste0("Marginal effect of ", dx) else paste0("Predicted value"),
         xlim = NULL,
         ylim = if (match.arg(what) %in% c("prediction", "stackedprediction")) c(0,1.04) else NULL,
         lwd = 1L,
         col = "black",
         lty = 1L,
         factor.lty = 1L,
         factor.pch = 19L,
         factor.col = col,
         factor.fill = factor.col,
         factor.cex = 1L,
         xaxs = "i",
         yaxs = xaxs,
         las = 1L,
         scatter = FALSE,
         scatter.pch = 19L,
         scatter.col = factor.col,
         scatter.bg = scatter.col,
         scatter.cex = 0.5,
         rug = TRUE,
         rug.col = col,
         rug.size = -0.02,
         ...) {
    
    xvar <- x
    yvar <- as.character(attributes(terms(object))[["variables"]][[2]]) #ok
    
    # handle factors and subset data
    f <- margins:::check_factors(object = object, data = data, xvar = xvar, dx = dx) #ok
    x_is_factor <- f[["x_is_factor"]]
    dx_is_factor <- f[["dx_is_factor"]]
    dat <- f[["data"]]
    
    # setup x (based on whether factor)
    if (isTRUE(x_is_factor)) {
        if (is.factor(dat[["xvar"]])) {
            xvals <- as.character(levels(dat[[margins:::clean_terms(xvar)]]))
        } else {
            xvals <- as.character(unique(dat[[margins:::clean_terms(xvar)]]))
        }
    } else {
        xvals <- xvals
    } 
    
    what <- match.arg(what)
    type <- match.arg(type)
    a <- (1 - level)/2
    fac <- qnorm(c(a, 1 - a))

    # setup `outdat` data
    if (what %in% c("prediction", "classprediction", "stackedprediction")) {
        tmpdat <- lapply(dat[, names(dat) != xvar, drop = FALSE], prediction:::mean_or_mode)
        tmpdat <- structure(lapply(tmpdat, rep, length(xvals)),
                            class = "data.frame", row.names = seq_len(length(xvals)))
        tmpdat[[xvar]] <- xvals
        outdat <- prediction(model = object, data = tmpdat, level = level)
        if (what == "classprediction") {
            out <- structure(list(xvals = xvals,
                                  yvals = outdat[["fitted.class"]]),
                             class = "data.frame", 
                             row.names = seq_along(outdat[["fitted.class"]]))
            out <- list(out)
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
        }
    } else if (what == "effect") {
        stop("Displaying marginal effects is not currently supported for 'clm' models!")
    }
    
    # optionally draw the plot; if FALSE, just the data are returned
    if (isTRUE(draw)) {
        if (is.null(ylim) && (what == "classprediction")) {
            ylim <- c(0.5, nlevels(out[[1]][["yvals"]]) + 0.5)
            y_is_factor <- TRUE
        } else {
            y_is_factor <- FALSE
        }
      
        margins:::setup_cplot(plotdat = out[[1L]], data = data, xvals = xvals, xvar = xvar, yvar = yvar,
                    xlim = xlim, ylim = ylim, x_is_factor = x_is_factor, y_is_factor = y_is_factor,
                    xlab = xlab, ylab = ylab, xaxs = xaxs, yaxs = yaxs, las = las,
                    scatter = scatter, scatter.pch = scatter.pch, scatter.col = scatter.col)
    }
    if (isTRUE(draw) || draw == "add") {
        if (length(lty) != length(out)) {
            lty <- rep(lty, length(out))
        }
        if (length(lwd) != length(out)) {
            lwd <- rep(lwd, length(out))
        }
        if (length(col) != length(out)) {
            col <- rep(col, length(out))
        }
        if (length(factor.col) != length(out)) {
            factor.col <- rep(factor.col, length(out))
        }
        if (length(factor.fill) != length(out)) {
            factor.fill <- rep(factor.fill, length(out))
        }
        for (i in seq_along(out)) {
            margins:::draw_one(xvals = out[[i]][["xvals"]], 
                     yvals = out[[i]][["yvals"]], 
                     x_is_factor = x_is_factor,
                     y_is_factor = y_is_factor,
                     col = col[i], lty = lty[i], lwd = lwd,
                     se.type = "none",
                     factor.lty = factor.lty, factor.pch = factor.pch, 
                     factor.fill = factor.fill[i], 
                     factor.col = factor.col[i], factor.cex = factor.cex)
          
        }
        if (isTRUE(rug) && is.numeric(data[[x]])) {
            draw_rug(data[[x]], rug.size = rug.size, rug.col = rug.col)
        }
    }
    
    # return data used in plot
    invisible(do.call("rbind", out))
#    invisible(out)
}

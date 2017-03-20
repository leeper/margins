calculate_surface <- function(x, xvar, yvar, nx, ny, type, vcov = stats::vcov(x), what) {
    
    # internal function to calculate surface for `persp()` and `image()`
    
    dat <- x[["model"]]
    dat[] <- lapply(dat, as.numeric) # this probably isn't a good idea
    
    xvals <- seq(min(dat[[xvar]], na.rm = TRUE), 
                 max(dat[[xvar]], na.rm = TRUE), 
                 length.out = nx)
    yvals <- seq(min(dat[[yvar]], na.rm = TRUE), 
                 max(dat[[yvar]], na.rm = TRUE), 
                 length.out = ny)
    
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
        vals <- marginal_effects(data = dat2, model = x, type = type)[, paste0("dydx_", xvar)]
        outcome <- matrix(NA_real_, nrow = nx, ncol = ny)
        outcome[as.matrix(expand.grid(1:nx, 1:ny))] <- vals
    }
    
    return(list(outcome = outcome, xvals = xvals, yvals = yvals))
}

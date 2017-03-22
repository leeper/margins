calculate_surface <- function(x, xvar, yvar, dx, nx, ny, type, vcov = stats::vcov(x), what) {
    
    # internal function to calculate surface for `persp()` and `image()`
    
    dat <- x[["model"]]
    dat[] <- lapply(dat, as.numeric) # this probably isn't a good idea
    
    xvals <- seq_range(dat[[xvar]], nx)
    yvals <- seq_range(dat[[yvar]], ny)
    
    if (what == "prediction") {
        datmeans <- structure(lapply(colMeans(dat[, !names(dat) %in% c(xvar, yvar), drop = FALSE]), rep, length(xvals) * length(yvals)),
                              class = "data.frame", row.names = seq_len(length(xvals) * length(yvals)))
        outcome <- outer(xvals, yvals, FUN = function(a, b) {
            datmeans[, xvar] <- a
            datmeans[, yvar] <- b
            prediction(model = x, data = datmeans, type = type)[["fitted"]]
        })
    } else if (what == "effect") {
        mar <- summary(margins(x, at = setNames(list(xvals, yvals), c(xvar, yvar)), vce = "none", type = type))
        vals <- mar[mar[["factor"]] == dx, "AME"]
        outcome <- matrix(NA_real_, nrow = nx, ncol = ny)
        outcome[as.matrix(expand.grid(seq_len(nx), seq_len(ny)))] <- vals
    }
    
    return(list(outcome = outcome, xvals = xvals, yvals = yvals))
}

#' @importFrom utils capture.output
calculate_surface <- function(x, xvar, yvar, dx, nx, ny, type, vcov = stats::vcov(x), what) {
    
    # internal function to calculate surface for `persp()` and `image()`
    
    dat <- prediction::find_data(x, env = .GlobalEnv)
    
    xvals <- seq_range(dat[[xvar]], nx)
    yvals <- seq_range(dat[[yvar]], ny)
    
    if (what == "prediction") {
        # mean predictions
        datmeans <- structure(lapply(lapply(dat[, !names(dat) %in% c(xvar, yvar), drop = FALSE],
                                            prediction::mean_or_mode),
                                     rep, length(xvals) * length(yvals)),
                              class = "data.frame", row.names = seq_len(length(xvals) * length(yvals)))
        outcome <- outer(xvals, yvals, FUN = function(a, b) {
            datmeans[, xvar] <- a
            datmeans[, yvar] <- b
            prediction(model = x, data = datmeans, type = type)[["fitted"]]
        })
    } else if (what == "effect") {
        # average marginal effects
        mar <- summary(margins(x, at = setNames(list(xvals, yvals), c(xvar, yvar)), vce = "none", type = type))
        vals <- mar[mar[["factor"]] == dx, "AME"]
        outcome <- matrix(NA_real_, nrow = nx, ncol = ny)
        outcome[as.matrix(expand.grid(seq_len(nx), seq_len(ny)))] <- vals
    }
    
    return(list(outcome = outcome, xvals = xvals, yvals = yvals))
}

warn_for_weights <- function(model) {
    wghts <- unname(model[["weights"]])
    if (!isTRUE(all.equal(wghts, rep(wghts[1], length(wghts))))) {
        warning("'weights' used in model estimation are currently ignored!", call. = FALSE)
    }
    NULL
}

#' @rdname marginal_effects
#' @export
marginal_effects.margins <- function(model, data, ...) {
    w <- which(sapply(model, inherits, what = "marginaleffect"))
    out <- model[, w, drop = FALSE]
    attributes(out) <- attributes(model)[names(attributes(model)) != "names"]
    names(out) <- names(model)[w]
    out
}

#' @rdname extract_marginal_effects
#' @export
marginal_effects.marginslist <- function(model, data, ...) {
    lapply(model, marginal_effects, ...)
}

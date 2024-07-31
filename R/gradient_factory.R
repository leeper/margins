# a factory function that returns a function to give the gradient (as a vector)
## used as first argument to jacobian()
gradient_factory <- function(data, model, variables = NULL, type = "response", weights = NULL, eps = 1e-7, varslist = NULL, ...) {
    UseMethod("gradient_factory", model)
}

#' @export
gradient_factory.default <- function(data, model, variables = NULL, type = "response", weights = NULL, eps = 1e-7, varslist = NULL, ...) {
    
    # identify classes of terms in `model`
    if (is.null(varslist)) {
        varslist <- find_terms_in_model(model, variables = variables)
    }
    
    # factory function to return marginal effects holding data constant but varying coefficients
    FUN <- function(coefs, weights = NULL) {
        model <- reset_coefs(model, coefs)
        if (is.null(weights)) {
            # build matrix of unit-specific marginal effects
            if (is.null(type)) {
                me_tmp <- marginal_effects(model = model, data = data, variables = variables, eps = eps, as.data.frame = FALSE, varslist = varslist, ...)
            } else {
                me_tmp <- marginal_effects(model = model, data = data, variables = variables, type = type, eps = eps, as.data.frame = FALSE, varslist = varslist, ...)
            }
            # apply colMeans to get average marginal effects
            means <- stats::setNames(.colMeans(me_tmp, nrow(me_tmp), ncol(me_tmp), na.rm = TRUE), colnames(me_tmp))
        } else {
            # build matrix of unit-specific marginal effects
            if (is.null(type)) {
                me_tmp <- marginal_effects(model = model, data = data, variables = variables, eps = eps, as.data.frame = FALSE, varslist = varslist, ...)
            } else {
                me_tmp <- marginal_effects(model = model, data = data, variables = variables, type = type, eps = eps, as.data.frame = FALSE, varslist = varslist, ...)
            }
            # apply colMeans to get average marginal effects
            means <- apply(me_tmp, 2L, stats::weighted.mean, w = weights, na.rm = TRUE)
        }
        means
    }
    return(FUN)
}

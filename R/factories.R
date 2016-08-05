.build_predict_fun <- function(data, model, type = "response") {
    # factory function to return prediction as a function of a named variable `x`
    #
    # returns a one-argument function that expresses `predict(model)` with respect to named vector x (variable names and values)
    # this allows the passing of model `predict()` functions to `numDeriv::grad()`
    FUN <- function(x) {
        tmpdat <- data
        if (!missing(x)) {
            for (i in seq_along(x)) {
                # this assumes numeric data, so factors will be problematic
                tmpdat[[names(x)[i]]] <- x[i]
            }
        }
        unname(prediction(model = model, data = tmpdat, type = type)[["fitted"]])
    }
    return(compiler::cmpfun(FUN))
}

.build_grad_fun <- function(data, model, which_me, type = "response", method = c("Richardson", "simple", "complex")) {
    # factory function to return prediction holding data constant but varying coefficients
    FUN <- function(coefs) {
        for (i in seq_along(coefs)) {
            model[["coefficients"]][names(coefs)[i]] <- coefs[i]
        }
        colMeans(marginal_effects(model = model, data = data, type = type, method = method)[, which_me, drop = FALSE], na.rm = TRUE)
    }
    return(compiler::cmpfun(FUN))
}

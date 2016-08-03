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
        unname(predict(model, newdata = tmpdat, type = type))
    }
    return(cmpfun(FUN))
}

.build_grad_fun <- function(data, model, which_me, atmeans = TRUE, type = "response", method = c("Richardson", "simple", "complex")) {
    # factory function to return prediction holding data constant but varying coefficients
    FUN <- function(coefs) {
        for (i in seq_along(coefs)) {
            model[["coefficients"]][names(coefs)[i]] <- coefs[i]
        }
        if (isTRUE(atmeans)) {
            get_slopes(data = as.data.frame(t(colMeans(data))), model = model, type = type, method = method)[,which_me]
        } else {
            colMeans(get_slopes(data = data, model = model, type = type, method = method)[, which_me, drop = FALSE], na.rm = TRUE)
        }
    }
    return(cmpfun(FUN))
}

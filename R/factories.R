.build_grad_fun <- function(data, model, type = "response", method = c("simple", "Richardson", "complex")) {
    method <- match.arg(method)
    # factory function to return prediction holding data constant but varying coefficients
    FUN <- function(coefs) {
        for (i in seq_along(coefs)) {
            model[["coefficients"]][names(coefs)[i]] <- coefs[i]
        }
        colMeans(marginal_effects(model = model, data = data, type = type), na.rm = TRUE)
    }
    return(compiler::cmpfun(FUN))
}

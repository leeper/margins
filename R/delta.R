delta_once <- 
function(data, 
         model, 
         type = c("response", "link", "terms"), 
         vcov = vcov(model),
         eps = 1e-7,
         ...) {
    # take the derivative of each marginal effect from a model with respect to each model coefficient
    
    type <- match.arg(type)
    if (is.function(vcov)) {
        vcov <- vcov(model)
    }
    
    # express each marginal effect as a function of estimated coefficients
    # holding data constant (using `.build_grad_fun()`)
    # use `jacobian(.build_grad_fun(), model$coef)`
    # to get `jacobian`, an ME-by-beta matrix,
    # such that jacobian %*% V %*% t(jacobian)
    # gives the variance of each marginal effect
    # http://www.soderbom.net/lecture10notes.pdf
    # http://stats.stackexchange.com/questions/122066/how-to-use-delta-method-for-standard-errors-of-marginal-effects
    
    FUN <- .build_grad_fun(data = data, model = model, type = type, ...)
    #jacobian <- numDeriv::jacobian(FUN, model[["coefficients"]], method = "simple")
    jacobian <- jacobian(FUN, model[["coefficients"]], eps = eps)
    vout <- diag(jacobian %*% vcov %*% t(jacobian))
    return(vout)
}

.build_grad_fun <- function(data, model, type = "response", eps = 1e-7, ...) {
    
    # factory function to return prediction holding data constant but varying coefficients
    FUN <- function(coefs) {
        model[["coefficients"]] <- coefs
        colMeans(marginal_effects(model = model, data = data, type = type, eps = eps, ...), na.rm = TRUE)
    }
    return(compiler::cmpfun(FUN))
}

jacobian <- function(FUN, coefficients, eps = 1e-4) {
    F0 <- FUN(coefficients)
    out <- matrix(NA_real_, nrow = length(F0), ncol = length(coefficients))
    for (i in seq_along(coefficients)) {
        coeftemp <- coefficients
        coeftemp[i] <- coeftemp[i] + eps
        out[, i] <- (FUN(coeftemp) - F0) / eps
    }
    out
}

delta_once <- 
function(data, 
         model, 
         variables = NULL, # which mes do we need variances of
         type = c("response", "link", "terms"), 
         vcov = vcov(model),
         weights = NULL,
         eps = 1e-7,
         varslist = NULL,
         ...) {
    # take the derivative of each marginal effect from a model with respect to each model coefficient
    
    type <- match.arg(type)
    
    
    # identify classes of terms in `model`
    if (is.null(varslist)) {
        varslist <- find_terms_in_model(model, variables = variables)
    }
    
    # setup vcov
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
    
    FUN <- .build_grad_fun(data = data,
                           model = model,
                           variables = variables,
                           type = type,
                           weights = weights,
                           eps = eps,
                           varslist = varslist,
                           ...)
    jacobian <- jacobian(FUN, coef(model), eps = eps)
    vout <- diag(jacobian %*% vcov %*% t(jacobian))
    return(vout)
}

.build_grad_fun <- function(data, model, variables = NULL, type = "response", weights = NULL, eps = 1e-7, varslist = NULL, ...) {
    
    # identify classes of terms in `model`
    if (is.null(varslist)) {
        varslist <- find_terms_in_model(model, variables = variables)
    }
    
    # factory function to return prediction holding data constant but varying coefficients
    FUN <- function(coefs) {
        model[["coefficients"]] <- coefs
        if (is.null(weights)) {
            means <- colMeans(marginal_effects(model = model, data = data, variables = variables, type = type, eps = eps, varslist = varslist, ...), na.rm = TRUE)
        } else {
            me_tmp <- marginal_effects(model = model, data = data, variables = variables, type = type, eps = eps, varslist = varslist, ...)
            means <- unlist(stats::setNames(lapply(me_tmp, stats::weighted.mean, w = weights, na.rm = TRUE), names(me_tmp)))
        }
        means
    }
    return(FUN)
}

jacobian <- function(FUN, coefficients, eps = 1e-4) {
    F0 <- FUN(coefficients)
    out <- matrix(NA_real_, nrow = length(F0), ncol = length(coefficients))
    colnames(out) <- names(coefficients)
    rownames(out) <- names(F0)
    for (i in seq_along(coefficients)) {
        coeftemp <- coefficients
        coeftemp[i] <- coeftemp[i] + eps
        out[, i] <- (FUN(coeftemp) - F0) / eps
    }
    out
}

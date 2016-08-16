delta_once <- 
function(data, 
         model, 
         type = c("response", "link", "terms"), 
         vcov = vcov(model),
         method = c("simple", "Richardson", "complex")) {
    # take the derivative of each marginal effect from a model with respect to each model coefficient
    
    type <- match.arg(type)
    method <- match.arg(method)
    if (is.function(vcov)) {
        vcov <- vcov(model)
    }
    
    # express each marginal effect as a function of all coefficients
    # holding data constant
    # this is what .build_grad_fun() will do
    # then:  numDeriv::grad(.build_grad_fun(), model$coef)
    # gives `gradmat`, such that v %*% V %*% t(v)
    # gives the variance of each marginal effect
    # `gradmat` should be an ME-by-beta matrix
    
    # Some references:
    # http://www.soderbom.net/lecture10notes.pdf
    # http://stats.stackexchange.com/questions/122066/how-to-use-delta-method-for-standard-errors-of-marginal-effects
    
    FUN <- .build_grad_fun(data = data, model = model, type = type, method = method)
    gradmat <- numDeriv::jacobian(FUN, model[["coefficients"]], method = method)
    vout <- diag(gradmat %*% vcov %*% t(gradmat))
    return(vout)
}

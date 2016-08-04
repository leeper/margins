get_effect_variances <- 
function(data = data, 
         model = model, 
         allvars = all.vars(model[["terms"]])[-1],
         type = c("response", "link", "terms"),
         vce = c("delta", "simulation", "bootstrap"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         method = c("simple", "Richardson", "complex"), # passed to marginal_effects()
         ...) {
    
    # march.arg() for arguments
    type <- match.arg(type)
    method <- match.arg(method)
    vce <- match.arg(vce)
    
    if (vce == "bootstrap") {
        # function to calculate AME for one bootstrap subsample
        bootfun <- function() {
            s <- sample(1:nrow(data), nrow(data), TRUE)
            colMeans(marginal_effects(model = model, data = data[s,], type = type, method = method)[, allvars, drop = FALSE], na.rm = TRUE)
        }
        # bootstrap the data and take the variance of bootstrapped AMEs
        variances <- apply(replicate(iterations, bootfun()), 1, var, na.rm = TRUE)
    } else if (vce == "delta") {
        
        # express each marginal effect as a function of all coefficients
        # holding data constant
        # this is what .build_grad_fun() will do
        # then:  numDeriv::grad(.build_grad_fun(), model$coef)
        # gives `gradmat`, such that v %*% V %*% t(v)
        # gives the variance of each marginal effect
        # `gradmat` should be an ME-by-beta matrix
        
        # http://www.soderbom.net/lecture10notes.pdf
        # http://stats.stackexchange.com/questions/122066/how-to-use-delta-method-for-standard-errors-of-marginal-effects
        
        # TO GET UNIT-SPECIFIC VARIANCES, NEED TO TAKE DERIVATIVE OF `.build_grad_fun()` FOR EVERY ROW SEPARATELY
        
        gradmat <- do.call("rbind", lapply(allvars, function(thisme) {
            FUN <- .build_grad_fun(data = data, model = model, which_me = thisme, type = type, method = method)
            numDeriv::grad(FUN, model[["coefficients"]])
        }))
        variances <- diag(gradmat %*% vcov(model) %*% t(gradmat))
    } else if (vce == "simulation") {
        
        # copy model for quick use in estimation
        tmpmodel <- model
        tmpmodel$model <- NULL # remove data from model for memory
        
        # simulate from multivariate normal
        coefmat <- MASS::mvrnorm(iterations, coef(model), vcov(model))
        
        # estimate AME from from each simulated coefficient vector
        effectmat <- apply(coefmat, 1, function(coefrow) {
            tmpmodel[["coefficients"]] <- coefrow
            colMeans(marginal_effects(data, model = tmpmodel, type = type, method = method)[, allvars, drop = FALSE])
        })
        # calculate the variance of the simulated AMEs
        variances <- apply(effectmat, 1, var, na.rm = TRUE)
    }
    return(variances)
}

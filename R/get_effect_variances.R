get_effect_variances <- 
function(data = data, 
         model = model, 
         which = all.vars(model[["terms"]])[-1], # which mes do we need variances of
         type = c("response", "link", "terms"),
         vcov = vcov(model),
         vce = c("delta", "simulation", "bootstrap"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         method = c("simple", "Richardson", "complex"), # passed to marginal_effects()
         ...) {
    
    # march.arg() for arguments
    type <- match.arg(type)
    method <- match.arg(method)
    vce <- match.arg(vce)
    if (is.function(vcov)) {
        vcov <- vcov(model)
    }
    
    if (vce == "delta") {
        
        # default method
        variances <- delta_once(data = data, model = model, type = type, vcov = vcov, method = method)
        
    } else if (vce == "simulation") {
        
        # copy model for quick use in estimation
        tmpmodel <- model
        tmpmodel$model <- NULL # remove data from model for memory
        
        # simulate from multivariate normal
        coefmat <- MASS::mvrnorm(iterations, coef(model), vcov)
        
        # estimate AME from from each simulated coefficient vector
        effectmat <- apply(coefmat, 1, function(coefrow) {
            tmpmodel[["coefficients"]] <- coefrow
            colMeans(marginal_effects(data, model = tmpmodel, type = type))
        })
        # calculate the variance of the simulated AMEs
        variances <- apply(effectmat, 1, var, na.rm = TRUE)
        
    } else if (vce == "bootstrap") {
    
        # function to calculate AME for one bootstrap subsample
        bootfun <- function() {
            s <- sample(seq_len(nrow(data)), nrow(data), TRUE)
            colMeans(marginal_effects(model = model, data = data[s,], type = type), na.rm = TRUE)
        }
        # bootstrap the data and take the variance of bootstrapped AMEs
        variances <- apply(replicate(iterations, bootfun()), 1, var, na.rm = TRUE)
        
    } 
    return(variances)
}

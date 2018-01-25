get_effect_variances <- 
function(data, 
         model = model, 
         variables = NULL, # which mes do we need variances of
         type = c("response", "link", "terms"),
         vcov = stats::vcov(model),
         vce = c("delta", "simulation", "bootstrap", "none"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         weights = NULL,
         eps = 1e-7,
         varslist = NULL,
         ...) {
    
    # march.arg() for arguments
    type <- match.arg(type)
    vce <- match.arg(vce)
    if (is.function(vcov)) {
        vcov <- vcov(model)
    }
    
    # identify classes of terms in `model`
    if (is.null(varslist)) {
        varslist <- find_terms_in_model(model, variables = variables)
    }
    
    if (vce == "none") {
        
        return(NULL)
        
    } else if (vce == "delta") {
        
        # default method
        variances <- delta_once(data = data,
                                model = model,
                                variables = variables,
                                type = type,
                                vcov = vcov,
                                weights = weights,
                                eps = eps,
                                varslist = varslist,
                                ...)
    
    } else if (vce == "simulation") {
        
        # copy model for quick use in estimation
        tmpmodel <- model
        tmpmodel[["model"]] <- NULL # remove data from model for memory
        
        # simulate from multivariate normal
        coefmat <- MASS::mvrnorm(iterations, coef(model), vcov)
        
        # estimate AME from from each simulated coefficient vector
        effectmat <- apply(coefmat, 1, function(coefrow) {
            tmpmodel[["coefficients"]] <- coefrow
            if (is.null(weights)) {
                means <- colMeans(marginal_effects(model = tmpmodel, data = data, variables = variables, type = type, eps = eps, varslist = varslist, ...), na.rm = TRUE)
            } else {
                me_tmp <- marginal_effects(model = tmpmodel, data = data, variables = variables, type = type, eps = eps, varslist = varslist, ...)
                means <- unlist(stats::setNames(lapply(me_tmp, stats::weighted.mean, w = weights, na.rm = TRUE), names(me_tmp)))
            }
            if (!is.matrix(means)) {
                matrix(means, ncol = 1L)
            }
            return(means)
        })
        # calculate the variance of the simulated AMEs
        variances <- apply(effectmat, 1, var, na.rm = TRUE)
        
    } else if (vce == "bootstrap") {
    
        # function to calculate AME for one bootstrap subsample
        bootfun <- function() {
            samp <- sample(seq_len(nrow(data)), nrow(data), TRUE)
            tmpmodel <- model
            tmpmodel[["call"]][["data"]] <- data[samp,]
            tmpmodel <- eval(tmpmodel[["call"]])
            if (is.null(weights)) {
                means <- colMeans(marginal_effects(model = tmpmodel,
                                                   data = data[samp,],
                                                   variables = variables,
                                                   type = type,
                                                   eps = eps,
                                                   varslist = varslist,
                                                   ...), na.rm = TRUE)
            } else {
                me_tmp <- marginal_effects(model = tmpmodel, data = data[samp,], variables = variables, type = type, eps = eps, varslist = varslist, ...)
                means <- unlist(stats::setNames(lapply(me_tmp, stats::weighted.mean, w = weights, na.rm = TRUE), names(me_tmp)))
            }
            means
        }
        # bootstrap the data and take the variance of bootstrapped AMEs
        variances <- apply(replicate(iterations, bootfun()), 1, var, na.rm = TRUE)
        
    }
    
    # replicate to nrow(data)
    variances <- setNames(lapply(variances, rep, nrow(data)), paste0("Var_", names(variances)))
    
    return(variances)
}

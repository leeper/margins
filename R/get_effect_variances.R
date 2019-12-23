# function to get effect variances using specified vce method
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
    if (!is.null(type)) {
        type <- match.arg(type)
    }
    vce <- match.arg(vce)
    
    # deploy appropriate vce procedure
    if (vce == "none") {
        return(list(variances = NULL, vcov = NULL, jacobian = NULL))
    }
    
    # setup vcov
    if (is.function(vcov)) {
        vcov <- vcov(model)
    }
    
    # identify classes of terms in `model`
    if (is.null(varslist)) {
        varslist <- find_terms_in_model(model, variables = variables)
    }
    
    if (vce == "delta") {
        
        # default method
        
        # express each marginal effect as a function of estimated coefficients
        # holding data constant (using `gradient_factory()`)
        # use `jacobian(gradient_factory(), model$coef)`
        # to get `jacobian`, an ME-by-beta matrix,
        # such that jacobian %*% V %*% t(jacobian)
        # gives the variance of each marginal effect
        # http://www.soderbom.net/lecture10notes.pdf
        # http://stats.stackexchange.com/questions/122066/how-to-use-delta-method-for-standard-errors-of-marginal-effects
        
        # build gradient function
        FUN <- gradient_factory(data = data,
                                model = model,
                                variables = variables,
                                type = type,
                                weights = weights,
                                eps = eps,
                                varslist = varslist,
                                ...)
        # get jacobian
        if (inherits(model, "merMod")) {
            requireNamespace("lme4")
            jacobian <- jacobian(FUN, lme4::fixef(model)[names(lme4::fixef(model)) %in% c("(Intercept)", colnames(vcov))], weights = weights, eps = eps)
            # sandwich
            vc <- as.matrix(jacobian %*% vcov %*% t(jacobian))
        } else {
            # check that vcov() only contains coefficients from model
            if (nrow(vcov) != length(coef(model))) {
                vcov <- vcov[intersect(rownames(vcov), names(coef(model))), intersect(rownames(vcov), names(coef(model)))]
            }
            
            jacobian <- jacobian(FUN, coef(model)[names(coef(model)) %in% c("(Intercept)", colnames(vcov))], weights = weights, eps = eps)
            
            # sandwich
            vc <- jacobian %*% vcov %*% t(jacobian)
        }
        # extract variances from diagonal
        variances <- diag(vc)
        
    } else if (vce == "simulation") {
        
        # copy model for quick use in estimation
        tmpmodel <- model
        if (inherits(model, "merMod")) {
            coefs <- lme4::fixef(model)
            # Removing data from model for memory, but S4 class requires "frame"
            # to be data.frame class --- hacky way of "removing" it
            model@frame <- model@frame[NULL] 
        } else {
            coefs <- coef(model)
            tmpmodel[["model"]] <- NULL # remove data from model for memory
        }
        
        # check that vcov() only contains coefficients from model
        if (nrow(vcov) != length(coefs)) {
            vcov <- vcov[intersect(rownames(vcov), names(coef(model))), intersect(rownames(vcov), names(coef(model)))]
        }
        
        # simulate from multivariate normal
        coefmat <- MASS::mvrnorm(iterations, coefs, vcov)
        
        # estimate AME from from each simulated coefficient vector
        effectmat <- apply(coefmat, 1, function(coefrow) {
            if (inherits(model, "merMod")) {
                tmpmodel@beta <- coefrow
            } else {
                tmpmodel[["coefficients"]] <- coefrow
            }
            if (is.null(weights)) {
                if (is.null(type)) {
                    means <- colMeans(marginal_effects(model = tmpmodel, data = data, variables = variables, eps = eps, varslist = varslist, ...), na.rm = TRUE)
                } else {
                    means <- colMeans(marginal_effects(model = tmpmodel, data = data, variables = variables, type = type, eps = eps, varslist = varslist, ...), na.rm = TRUE)
                }
            } else {
                if (is.null(type)) {
                    me_tmp <- marginal_effects(model = tmpmodel, data = data, variables = variables, eps = eps, varslist = varslist, ...)
                } else {
                    me_tmp <- marginal_effects(model = tmpmodel, data = data, variables = variables, type = type, eps = eps, varslist = varslist, ...)
                }
                means <- unlist(stats::setNames(lapply(me_tmp, stats::weighted.mean, w = weights, na.rm = TRUE), names(me_tmp)))
            }
            if (!is.matrix(means)) {
                matrix(means, ncol = 1L)
            }
            return(means)
        })
        # When length(variables) == 1, effectmat is a vector
        if (!is.matrix(effectmat)) {
            # Coerce to 1 row matrix
            effectmat <- matrix(effectmat, nrow = 1)
            # Rownames are lost in these cases
            rownames(effectmat) <- paste0("dydx_", variables)
        }
        # calculate the variance of the simulated AMEs
        vc <- var(t(effectmat))
        variances <- diag(vc)
        jacobian <- NULL
        
    } else if (vce == "bootstrap") {
    
        # function to calculate AME for one bootstrap subsample
        bootfun <- function() {
            samp <- sample(seq_len(nrow(data)), nrow(data), TRUE)
            tmp_call <- getCall(model)
            tmp_call[["data"]] <- data[samp,]
            tmpmodel <- eval(tmp_call)
            if (is.null(weights)) {
                if (is.null(type)) {
                    means <- colMeans(marginal_effects(model = tmpmodel,
                                                       data = data[samp,],
                                                       variables = variables,
                                                       eps = eps,
                                                       varslist = varslist,
                                                       ...), na.rm = TRUE)
                } else {
                    means <- colMeans(marginal_effects(model = tmpmodel,
                                                       data = data[samp,],
                                                       variables = variables,
                                                       type = type,
                                                       eps = eps,
                                                       varslist = varslist,
                                                       ...), na.rm = TRUE)
                }
            } else {
                if (is.null(type)) {
                    me_tmp <- marginal_effects(model = tmpmodel, data = data[samp,], variables = variables, eps = eps, varslist = varslist, ...)
                } else {
                    me_tmp <- marginal_effects(model = tmpmodel, data = data[samp,], variables = variables, type = type, eps = eps, varslist = varslist, ...)
                }
                means <- unlist(stats::setNames(lapply(me_tmp, stats::weighted.mean, w = weights, na.rm = TRUE), names(me_tmp)))
            }
            means
        }
        # bootstrap the data and take the variance of bootstrapped AMEs
        vc <- if (length(variables) > 1) {
            var(t(replicate(iterations, bootfun())))
        } else { # Take the variance of the vector
            # Need to coerce to 1 x 1 matrix with appropriate dimnames
            matrix(var(replicate(iterations, bootfun())), nrow = 1L, 
                   dimnames = list(nms <- paste0("dydx_", variables), nms))
        }
        variances <- diag(vc)
        jacobian <- NULL
    }
    
    # replicate to nrow(data)
    variances <- setNames(lapply(variances, rep, nrow(data)), paste0("Var_", names(variances)))
    
    return(list(variances = variances, vcov = vc, jacobian = jacobian))
}

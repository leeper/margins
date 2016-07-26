.margins <- 
function(x, 
         data = eval(x$call$data, parent.frame()), 
         ..., 
         at = NULL, 
         atmeans = FALSE, 
         factors = c("discrete", "continuous"), # Not implemented (should be diff between two predictions)
         type = c("response", "terms", "link")) {
    
    factors <- match.arg(factors)
    type <- match.arg(type)
    
    # variables in the model
    allvars <- attributes(terms(x))$term.labels[attributes(terms(x))$order == 1]
    allvars <- sort(.cleanterms(allvars))
    
    # create a data.frame and pass to .data
    dat <- .setdata(data[, allvars, drop = FALSE], at = at)
    
    # iterate over data.frames and pass each to .slope
    out <- lapply(dat, function(d) {
        # optionally pass to .atmeans
        if(atmeans) {
            # need to be able to tell .atmeans which vars to set to means
            d <- .atmeans(d, vars = names(d), na.rm = TRUE)
        }
        
        # obtain gradient with respect to each variable in `allvars`
        thisgrad <- lapply(allvars, function(variable) {
            # use sapply to evaluate gradient at each value of variable from `allvars`
            slope <- numeric(nrow(d))
            for (i in seq_along(d[,variable])) {
                slope[i] <- .slope(value = d[i,variable], varname = variable, model = x, data = d[i,,drop=FALSE], type = type)
            }
            slope
        })
        setNames(thisgrad, allvars)
    })
    
    
    # AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAH!!!
    
    # Apply chain rule: Jacobian %*% V(beta) %*% t(Jacobian)
    #chain <- grad
    #chain <- grad * mean(predicted) * mean(dpredicted)
    #chain <- grad * mean(predicted) # close
    #chain <- grad * mean(dpredicted)
    #chain <- apply(grad, 2, `*`, t(MEs))
    #colnames(chain) <- betas
    grad <- setNames(out, names(dat))
    
    # model variance-covariance matrix
    vc <- vcov(x)
    
    # calculate variances
    for (i in seq_along(grad)) {
        jac <- t(colMeans(do.call("cbind", grad[[i]])))
        colnames(jac) <- allvars
        Variances <- diag(jac %*% vc[allvars, allvars] %*% t(jac))
    }
    
    structure(grad, class = "margins", atmeans = atmeans)
}


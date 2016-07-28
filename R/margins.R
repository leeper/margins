#' @importFrom stats setNames
#' @importFrom MASS mvrnorm
.margins <- 
function(x, 
         data = eval(x$call$data, parent.frame()), 
         atmeans = FALSE, 
         type = c("response", "terms", "link"),
         vce = c("delta", "bootstrap", "simulation"), # sloooooow....
         iterations = 500L, # if vce == "bootstrap" or "simulation"
         method = c("Richardson", "simple", "complex"), # passed to .slope()
         ...) {
    
    # variables in the model
    allvars <- attributes(terms(x))$term.labels[attributes(terms(x))$order == 1]
    allvars <- sort(.cleanterms(allvars))
    
    # optionally pass to .atmeans
    if(atmeans) {
        # need to be able to tell .atmeans which vars to set to means
        dat <- .atmeans(data, vars = names(data), na.rm = TRUE)
    } else {
        dat <- data
    }
    
    type <- match.arg(type)
    # obtain gradient with respect to each variable in data
    ## THIS DOES NOT HANDLE DISCRETE FACTORS
    grad <- .slope(dat, model = x, type = type, method = method)[, allvars, drop = FALSE]
    
    # variance estimation technique
    vce <- match.arg(vce)
    # model variance-covariance matrix
    vc <- vcov(x)
    
    if (vce == "bootstrap") {
        # function to calculate AME for one bootstrap subsample
        bootfun <- function() {
            s <- sample(1:nrow(dat), nrow(dat), TRUE)
            colMeans(.slope(dat[s,], model = x, type = type, method = method)[, allvars, drop = FALSE], na.rm = TRUE)
        }
        # bootstrap the data and take the variance of bootstrapped AMEs
        variances <- apply(replicate(iterations, bootfun()), 2, var, na.rm = TRUE)
    } else if (vce == "delta") {
        # AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAH!!!
    
        # Apply chain rule: Jacobian %*% V(beta) %*% t(Jacobian)
        #chain <- grad
        #chain <- grad * mean(predicted) * mean(dpredicted)
        #chain <- grad * mean(predicted) # close
        #chain <- grad * mean(dpredicted)
        #chain <- apply(grad, 2, `*`, t(MEs))
        #colnames(chain) <- betas
        
        #var <- t(colMeans(grad)) %*% vc[allvars, allvars] %*% colMeans(grad)
        
        # calculate variances
        # variances <- numeric(length(allvars))
        # for (i in seq_along(grad)) {
            # jac <- t(colMeans(out))
            # colnames(jac) <- allvars
            # variances[i] <- diag(jac %*% vc[allvars, allvars] %*% t(jac))
        # }
        variances <- diag(vc[allvars, allvars, drop = FALSE])
    } else if (vce == "simulation") {
        
        # copy model for quick use in estimation
        tmpmodel <- x
        tmpmodel$model <- NULL
        
        # simulate from multivariate normal
        coefmat <- MASS::mvrnorm(iterations, coef(x), vcov(x))
        
        # estimate AME from from each simulated coefficient vector
        effectmat <- apply(coefmat, 1, function(coefrow) {
            tmpmodel[["coefficients"]] <- coefrow
            colMeans(.slope(dat, model = tmpmodel, type = type, method = method)[, allvars, drop = FALSE])
        })
        # calculate the variance of the simulated AMEs
        variances <- apply(effectmat, 1, var, na.rm = TRUE)
    }
    
    structure(list(Effects = grad, Variances = variances), 
              class = "margins", 
              type = type,
              atmeans = atmeans, 
              vce = vce, 
              iterations = iterations)
}


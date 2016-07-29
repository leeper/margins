#' @title Marginal Effects Calculator
#' @description This is the low-level marginal effects calculator called by \code{\link{margins}}.
#' @importFrom stats setNames
#' @importFrom MASS mvrnorm
#' @export
.margins <- 
function(x, 
         data,
         atmeans = FALSE, 
         type = c("response", "terms", "link"),
         vce = c("delta", "bootstrap", "simulation"), # sloooooow....
         iterations = 500L, # if vce == "bootstrap" or "simulation"
         method = c("Richardson", "simple", "complex"), # passed to .slope()
         ...) {
    
    # variables in the model
    allvars <- attributes(terms(x))$term.labels[attributes(terms(x))$order == 1]
    allvars <- sort(.cleanterms(allvars))
    
    # setup data
    if (missing(data)) {
        d <- eval(x$call$data, parent.frame())
        dat <- if (!is.null(d)) d else x$model
        rm(d)
    } else {
        dat <- data
    }
    if (atmeans) {
        # optionally pass to .atmeans
        # need to be able to tell .atmeans which vars to set to means
        dat <- .atmeans(dat, vars = names(dat), na.rm = TRUE)
    }
    
    type <- match.arg(type)
    # obtain gradient with respect to each variable in data
    ## THIS DOES NOT HANDLE DISCRETE FACTORS
    grad <- .slope(dat, model = x, type = type, method = method)[, allvars, drop = FALSE]
    if (inherits(x, "glm") && type == "response") {
        pred <- .pred(dat, model = x, type = "link")
        tmp <- apply(grad, 2, `*`, pred)
        rm(pred)
        rownames(tmp) <- rownames(grad)
        grad <- tmp
        rm(tmp)
    }
    
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
        
        # express each marginal effect as a function of all coefficients
        # holding data constant (maybe just atmeans to start)
        # this is what .grad_factory() will do
        # then:  numDeriv::grad(.grad_factory(), x$coef)
        # gives `gradmat`, such that v %*% V %*% t(v)
        # gives the variance of each marginal effect
        # `gradmat` should be an ME-by-beta matrix
        
        # http://www.soderbom.net/lecture10notes.pdf
        # http://stats.stackexchange.com/questions/122066/how-to-use-delta-method-for-standard-errors-of-marginal-effects
        
        gradmat <- do.call("rbind", lapply(allvars, function(thisme) {
            FUN <- .grad_factory(data = dat, model = x, which_me = thisme, atmeans = TRUE, type = type, method = method)
            numDeriv::grad(FUN, x$coef)
        }))
        variances <- diag(gradmat %*% vc %*% t(gradmat))
    } else if (vce == "simulation") {
        
        # copy model for quick use in estimation
        tmpmodel <- x
        tmpmodel$model <- NULL # remove data from model for memory
        
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
              df.residual = x[["df.residual"]],
              vce = vce, 
              iterations = iterations)
}


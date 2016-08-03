#' @title Marginal Effects Calculator
#' @description This is the low-level marginal effects calculator called by \code{\link{margins}}.
#' @param x A model object.
#' @param data A data.frame. If missing, data are extracted from the \code{x}.
#' @param atmeans A logical indicating whether to estimate \dQuote{marginal effects at means} or \dQuote{average marginal effects} (the default).
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param vce A character string indicating the type of estimation procedure to use for estimating variances. The default (\dQuote{delta}) uses the delta method. Alternatives are \dQuote{bootstrap}, which uses bootstrap estimation, or \dQuote{simulation}, which averages across simulations drawn from the joint sampling distribution of model coefficients. The latter two are extremely time intensive.
#' @param iterations If \code{vce = "bootstrap"}, the number of bootstrap iterations. If \code{vce = "simulation"}, the number of simulated effects to draw. Ignored otherwise.
#' @param method A character string indicating the numeric derivative method to use when estimating marginal effects. See \code{\link[numDeriv]{grad}} for details.
#' @param \dots Ignored.
#' @details Generally, it is not necessary to call this function directly because \code{\link{margins}} provides a simpler interface.
#' 
#' This is the internal, workhorse function that estimates marginal effects from model \code{x} for a data.frame, \code{data}, optionally estimating either \dQuote{average marginal effects} (when \code{atmeans = FALSE}) or \dQuote{marginal effects at means} (when \code{atmeans = TRUE}). In the former case, the marginal effects are estimated for each observation in the dataset and returned in full. In the latter, column means are taken for \code{data} and estimation is performed only these \dQuote{averaged} cases. The former is generally preferred because the latter may estimate marginal effects for cases that are unintuitive or not covered by the observed data (e.g., the effect when a binary variable in \code{data} is averaged to 0.6 rather than at 0 and 1, respectively).
#' 
#' To estimate marginal effects at specified values of \code{x} (other than means), use \code{\link{margins}} with the \code{at} parameter to specify values of covariates at which to estimate effects.
#' 
#' The choice of \code{vce} may be important. The default variance-covariance estimation procedure (\code{vce = "delta"}) uses the delta method to estimate marginal effect variances. This is the fastest method. When \code{vce = "simulation"}, coefficient estimates are repeatedly drawn from the asymptotic (multivariate normal) distribution of the model coefficients and each draw is used to estimate marginal effects, with the variance based upon the dispersion of those simulated effects. The number of interations used is given by \code{iterations}. For \code{vce = "bootstrap"}, the bootstrap is used to repeatedly subsample \code{data} and the variance of marginal effects is estimated from the variance of the bootstrap distribution. This method is markedly slower than the other two procedures and, obviously, it will probably fail if \code{atmeans = TRUE}. Again, \code{iterations} regulates the number of boostrap subsamples to draw.
#' 
#'
#' @return A data.frame of class \dQuote{margins} containing the contents of \code{data}, fitted values for \code{x}, and any estimated marginal effects. Attributes containing additional information, including the marginal effect variances and additional details.
#' @import stats
#' @importFrom compiler cmpfun
#' @importFrom numDeriv grad
#' @importFrom MASS mvrnorm
#' @export
marginal_effect <- 
function(x, 
         data,
         atmeans = FALSE, 
         type = c("response", "link", "terms"),
         vce = c("delta", "simulation", "bootstrap"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         method = c("simple", "Richardson", "complex"), # passed to get_slopes()
         ...) {
    
    # variables in the model
    allvars <- attributes(terms(x))$term.labels[attributes(terms(x))$order == 1]
    allvars <- sort(clean_terms(allvars))
    
    # setup data
    if (missing(data)) {
        d <- eval(x[["call"]][["data"]], parent.frame())
        dat <- if (!is.null(d)) d else x[["model"]]
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
    method <- match.arg(method)
    
    # obtain gradient with respect to each variable in data
    ## THIS DOES NOT HANDLE DISCRETE FACTORS
    grad <- get_slopes(dat, model = x, type = type, method = method)[, allvars, drop = FALSE]
    
    # variance estimation technique
    vce <- match.arg(vce)
    # model variance-covariance matrix
    vc <- vcov(x)
    
    if (vce == "bootstrap") {
        # function to calculate AME for one bootstrap subsample
        bootfun <- function() {
            s <- sample(1:nrow(dat), nrow(dat), TRUE)
            colMeans(get_slopes(dat[s,], model = x, type = type, method = method)[, allvars, drop = FALSE], na.rm = TRUE)
        }
        # bootstrap the data and take the variance of bootstrapped AMEs
        variances <- apply(replicate(iterations, bootfun()), 1, var, na.rm = TRUE)
    } else if (vce == "delta") {
        
        # express each marginal effect as a function of all coefficients
        # holding data constant (maybe just atmeans to start)
        # this is what .build_grad_fun() will do
        # then:  numDeriv::grad(.build_grad_fun(), x$coef)
        # gives `gradmat`, such that v %*% V %*% t(v)
        # gives the variance of each marginal effect
        # `gradmat` should be an ME-by-beta matrix
        
        # http://www.soderbom.net/lecture10notes.pdf
        # http://stats.stackexchange.com/questions/122066/how-to-use-delta-method-for-standard-errors-of-marginal-effects
        
        gradmat <- do.call("rbind", lapply(allvars, function(thisme) {
            # THIS NEEDS TO BE SET TO `atmeans = atmeans` BUT IT IS SUPER, SUPER SLOW
            FUN <- .build_grad_fun(data = dat, model = x, which_me = thisme, atmeans = atmeans, type = type, method = method)
            numDeriv::grad(FUN, x[["coefficients"]])
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
            colMeans(get_slopes(dat, model = tmpmodel, type = type, method = method)[, allvars, drop = FALSE])
        })
        # calculate the variance of the simulated AMEs
        variances <- apply(effectmat, 1, var, na.rm = TRUE)
    }
    
    # obtain predicted values and standard errors
    pred <- stats::predict(x, newdata = data, type = type, se.fit = TRUE)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # set output classes
    for (i in seq_along(grad)) {
        class(grad[[i]]) <- c("marginaleffect", "numeric")
    }
    
    structure(cbind(dat, fit = pred[["fit"]], se.fit = pred[["se.fit"]], grad), 
              class = c("margins", "data.frame"), 
              Variances = setNames(variances, names(grad)),
              type = type,
              atmeans = atmeans, 
              call = x[["call"]],
              df.residual = x[["df.residual"]],
              vce = vce, 
              call = call,
              iterations = iterations)
}

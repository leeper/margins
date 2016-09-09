#' @title \dQuote{margins} Object Builder
#' @description This is the lower-level function called by \code{\link{margins}} that assembles a \dQuote{margins} object from calls to \code{\link{prediction}}, \code{\link{marginal_effects}}, and \code{get_effect_variances} (not exported).
#' @param model A model object.
#' @param data A data.frame over which to calculate marginal effects.
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param vcov A matrix containing the variance-covariance matrix for estimated model coefficients, a function to perform the estimation with \code{model} as its only argument
#' @param vce A character string indicating the type of estimation procedure to use for estimating variances. The default (\dQuote{delta}) uses the delta method. Alternatives are \dQuote{bootstrap}, which uses bootstrap estimation, or \dQuote{simulation}, which averages across simulations drawn from the joint sampling distribution of model coefficients. The latter two are extremely time intensive.
#' @param iterations If \code{vce = "bootstrap"}, the number of bootstrap iterations. If \code{vce = "simulation"}, the number of simulated effects to draw. Ignored otherwise.
#' @param unit_ses If \code{vce = "delta"}, a logical specifying whether to calculate and return unit-specific marginal effect variances. This calculation is time consuming and the information is often not needed, so this is set to \code{FALSE} by default.
#' @param eps A numeric value specifying the \dQuote{step} to use when calculating numerical derivatives.
#' @param \dots Ignored.
#' @details Generally, it is not necessary to call this function directly because \code{\link{margins}} provides a simpler interface. To just get marginal effects without building a \dQuote{margins} object, call \code{\link{marginal_effects}} instead, which handles the effect estimation of a model object without building a \dQuote{margins} object.
#' 
#' This is the package's core function that assembles a \dQuote{margins} object, through sequential calls to \code{\link{prediction}}, \code{\link{marginal_effects}}, and an internal function (\code{get_effect_variances()}) to calculate variances. See documentation pages for those functions for details on implementation and return values.
#' 
#' The choice of \code{vce} may be important. The default variance-covariance estimation procedure (\code{vce = "delta"}) uses the delta method to estimate marginal effect variances. This is the fastest method. When \code{vce = "simulation"}, coefficient estimates are repeatedly drawn from the asymptotic (multivariate normal) distribution of the model coefficients and each draw is used to estimate marginal effects, with the variance based upon the dispersion of those simulated effects. The number of interations used is given by \code{iterations}. For \code{vce = "bootstrap"}, the bootstrap is used to repeatedly subsample \code{data} and the variance of marginal effects is estimated from the variance of the bootstrap distribution. This method is markedly slower than the other two procedures. Again, \code{iterations} regulates the number of boostrap subsamples to draw.
#'
#' @return A data.frame of class \dQuote{margins} containing the contents of \code{data}, fitted values for \code{model}, the standard errors of the fitted values, and any estimated marginal effects. This data.frame may have repeated column names (for the original variables and the margginal effects thereof). Marginal effects columns are distinguished by their class (\dQuote{marginaleffect}) and can be extracted using \code{\link{extract_marginal_effects}}. Attributes containing additional information, including the marginal effect variances and additional details.
#' @seealso \code{\link{margins}}, \code{\link{marginal_effects}}
#' @keywords models
#' @import stats
#' @importFrom compiler cmpfun
#' @importFrom MASS mvrnorm
#' @export
build_margins <- 
function(model, 
         data,
         type = c("response", "link", "terms"),
         vcov = stats::vcov(model),
         vce = c("delta", "simulation", "bootstrap", "none"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         unit_ses = FALSE,
         eps = 1e-7,
         ...) {
    
    # variables in the model
    allvars <- all.vars(model[["terms"]])[-1]
    
    # march.arg() for arguments
    type <- match.arg(type)
    vce <- match.arg(vce)
    if (is.function(vcov)) {
        vcov <- vcov(model)
    }
    
    # obtain gradient with respect to each variable in data
    mes <- marginal_effects(model = model, data = data, type = type, eps = eps)
    
    # variance estimation technique
    variances <- get_effect_variances(data = data, model = model, allvars = names(mes), 
                                      type = type, vcov = vcov, vce = vce, 
                                      iterations = iterations, eps = eps)
    
    # get unit-specific effect variances (take derivative of `.build_grad_fun()` for every row separately)
    if ((vce == "delta") && (isTRUE(unit_ses))) {
        vmat <- do.call("rbind", lapply(seq_len(nrow(data)), function(datarow) {
            delta_once(data = data[datarow,], model = model, type = type, vcov = vcov, eps = eps)
        }))
        colnames(vmat) <- paste0("se.", names(mes))
        vmat <- as.data.frame(vmat)
        vmat[] <- lapply(vmat, function(x) {
            structure(sqrt(x), class = c("se.marginaleffect", "numeric"))
        })
    }
    
    # obtain predicted values and standard errors
    pred <- prediction(model = model, data = data, type = type)
    
    # setup output structure
    structure(if ((vce == "delta") && (isTRUE(unit_ses))) {
                  cbind(data, pred, mes, vmat)
              } else { 
                  cbind(data, pred, mes)
              }, 
              class = c("margins", "data.frame"), 
              variances = if (is.null(variances)) variances else setNames(variances, names(mes)),
              type = type,
              call = model[["call"]],
              df.residual = model[["df.residual"]],
              vce = vce, 
              iterations = iterations)
}

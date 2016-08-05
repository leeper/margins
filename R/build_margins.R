#' @title \dQuote{margins} Object Builder
#' @description This is the low-level marginal effects calculator called by \code{\link{margins}} that assembles a \dQuote{margins} object.
#' @param model A model object.
#' @param data A data.frame over which to calculate marginal effects.
#' @param type A character string indicating the type of marginal effects to estimate. Mostly relevant for non-linear models, where the reasonable options are \dQuote{response} (the default) or \dQuote{link} (i.e., on the scale of the linear predictor in a GLM).
#' @param vce A character string indicating the type of estimation procedure to use for estimating variances. The default (\dQuote{delta}) uses the delta method. Alternatives are \dQuote{bootstrap}, which uses bootstrap estimation, or \dQuote{simulation}, which averages across simulations drawn from the joint sampling distribution of model coefficients. The latter two are extremely time intensive.
#' @param iterations If \code{vce = "bootstrap"}, the number of bootstrap iterations. If \code{vce = "simulation"}, the number of simulated effects to draw. Ignored otherwise.
#' @param method A character string indicating the numeric derivative method to use when estimating marginal effects. \dQuote{simple} optimizes for speed; \dQuote{Richardson} optimizes for accuracy. See \code{\link[numDeriv]{grad}} for details.
#' @param \dots Ignored.
#' @details Generally, it is not necessary to call this function directly because \code{\link{margins}} provides a simpler interface. To just get marginal effects without building a \dQuote{margins} object, call \code{\link{marginal_effects}}, which handles the actual differentiation of a model object.
#' 
#' This is the internal, workhorse function that estimates marginal effects from model \code{model} for a data.frame, \code{data}, optionally estimating either \dQuote{average marginal effects} (when \code{atmeans = FALSE}) or \dQuote{marginal effects at means} (when \code{atmeans = TRUE}). In the former case, the marginal effects are estimated for each observation in the dataset and returned in full. In the latter, column means are taken for \code{data} and estimation is performed only these \dQuote{averaged} cases. The former is generally preferred because the latter may estimate marginal effects for cases that are unintuitive or not covered by the observed data (e.g., the effect when a binary variable in \code{data} is averaged to 0.6 rather than at 0 and 1, respectively).
#' 
#' To estimate marginal effects at specified values of \code{model} (other than means), use \code{\link{margins}} with the \code{at} parameter to specify values of covariates at which to estimate effects.
#' 
#' The choice of \code{vce} may be important. The default variance-covariance estimation procedure (\code{vce = "delta"}) uses the delta method to estimate marginal effect variances. This is the fastest method. When \code{vce = "simulation"}, coefficient estimates are repeatedly drawn from the asymptotic (multivariate normal) distribution of the model coefficients and each draw is used to estimate marginal effects, with the variance based upon the dispersion of those simulated effects. The number of interations used is given by \code{iterations}. For \code{vce = "bootstrap"}, the bootstrap is used to repeatedly subsample \code{data} and the variance of marginal effects is estimated from the variance of the bootstrap distribution. This method is markedly slower than the other two procedures and, obviously, it will probably fail if \code{atmeans = TRUE}. Again, \code{iterations} regulates the number of boostrap subsamples to draw.
#' 
#'
#' @return A data.frame of class \dQuote{margins} containing the contents of \code{data}, fitted values for \code{model}, the standard errors of the fitted values, and any estimated marginal effects. This data.frame may have repeated column names (for the original variables and the margginal effects thereof). Marginal effects columns are distinguished by their class (\dQuote{marginaleffect}) and can be extracted using \code{\link{extract_marginal_effects}}. Attributes containing additional information, including the marginal effect variances and additional details.
#' @seealso \code{\link{margins}}, \code{\link{marginal_effects}}
#' @keywords models
#' @import stats
#' @importFrom compiler cmpfun
#' @importFrom numDeriv grad
#' @importFrom MASS mvrnorm
#' @export
build_margins <- 
function(model, 
         data,
         type = c("response", "link", "terms"),
         vce = c("delta", "simulation", "bootstrap"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         method = c("simple", "Richardson", "complex"), # passed to marginal_effects()
         ...) {
    
    # variables in the model
    allvars <- all.vars(model[["terms"]])[-1]
    
    # march.arg() for arguments
    type <- match.arg(type)
    method <- match.arg(method)
    vce <- match.arg(vce)
    
    # obtain gradient with respect to each variable in data
    mes <- marginal_effects(model = model, data = data, type = type, method = method)
    mes_out <- mes[, grep(allvars, names(mes)), drop = FALSE]
    
    # variance estimation technique
    variances <- get_effect_variances(data = data, model = model, allvars = allvars, 
                                      type = type, vce = vce, iterations = iterations, method = method)
    
    # obtain predicted values and standard errors
    pred <- prediction(model = model, data = data, type = type)
    
    # setup output structure
    structure(cbind(data, pred, mes_out), 
              class = c("margins", "data.frame"), 
              Variances = setNames(variances, names(mes)),
              type = type,
              call = model[["call"]],
              df.residual = model[["df.residual"]],
              vce = vce, 
              iterations = iterations)
}

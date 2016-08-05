#' @rdname margins
#' @title Marginal Effects Estimation
#' @description \code{margins} is an S3 generic function for building a \dQuote{margins} object from a model object. Methods are currently implemented for \dQuote{lm} (and, implicitly, \dQuote{glm}) class objects.
#' @param model A model object of class \dQuote{lm}.
#' @param data A data.frame containing the data at which to evaluate the marginal effects, as in \code{\link[stats]{predict}}.
#' @param at A list of one or more named vectors, specifically values at which to calculate the marginal effects. See \code{\link{build_datalist}} for details on use.
#' @param atmeans A logical indicating whether to calculate marginal effects at the means (i.e., partial effects at the average of all covariates), as opposed to the default average marginal effects (i.e., average partial effects), which is the default.
#' @param \dots Arguments passed to methods, and in turn to \code{\link{build_margins}}.
#' @details Methods for this generic return a \dQuote{marginslist} list of one or more \dQuote{margins} objects. A \dQuote{margins} object is a data.frame consisting of the original data, predicted values and standard errors thereof, estimated marginal effects from the model \code{model}, with attributes describing various features of the marginal effects estimates.
#' 
#' See \code{\link{marginal_effects}} for details on estimation of marginal effects.
#'
#' The \code{atmeans} option controls what types of effects are returned. Estimates are either \dQuote{average marginal effects} (when \code{atmeans = FALSE}, the default) or \dQuote{marginal effects at means} (when \code{atmeans = TRUE}). In the former case, the marginal effects are estimated for each observation in the dataset and returned in full. In the latter, column means are taken for \code{data} and estimation is performed only these \dQuote{averaged} cases. The former is generally preferred because the latter may estimate marginal effects for cases that are unintuitive or not covered by the observed data (e.g., the effect when a binary variable in \code{data} is averaged to 0.6 rather than at 0 and 1, respectively).
#' 
#' The \code{margins} method for objects of class \dQuote{lm} or \dQuote{glm} simply constructs a list of data.frames (using \code{\link{build_datalist}}) and calls \code{\link{build_margins}} on each. You can call \code{build_margins} directly, but it requires the explicit specification of a dataset over which to estimate the quantities of interest.
#' 
#' Alternatively, you can use \code{\link{marginal_effects}} to retrieve a data.frame of marginal effects without constructing a \dQuote{margins} object. That can be efficient for plotting, etc., given the time-consuming nature of variance estimation. Use \code{\link{extract_marginal_effects}} to retrieve a data.frame of marginal effects from a \dQuote{margins} object.
#' 
#' @return An object of class \dQuote{marginslist}, composed of one or more objects of class \dQuote{margins}.
#' @author Thomas J. Leeper
#' @references
#' Greene, W.H. 2012. Econometric Analysis, 7th Ed. Boston: Pearson.
#' 
#' Stata manual: \code{margins}. Retrieved 2014-12-15 from \url{http://www.stata.com/manuals13/rmargins.pdf}.
#' @examples
#' # linear model
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
#' margins(x)
#'
#' # generalized linear model
#' x <- glm(am ~ hp, data = head(mtcars), family = binomial)
#' margins(x, type = "response")
#' margins(x, type = "link")
#' 
#' @seealso \code{\link{marginal_effects}}, \code{\link{build_margins}}
#' @keywords models
#' @export
margins <- 
function(model, ...) {
    UseMethod("margins")
}

#' @rdname margins
#' @export
margins.lm <- 
function(model, 
         data, 
         at = NULL, 
         atmeans = FALSE, 
         ...){
    
    # setup data
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    data_list <- build_datalist(data, at = at, atmeans = atmeans)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # calculate marginal effects
    out <- lapply(data_list, function(thisdata) {
        m <- build_margins(model = model, data = thisdata, atmeans = atmeans, ...)
        attr(m, "at") <- attributes(thisdata)[["at"]]
        attr(m, "atmeans") <- atmeans
        m
    })
    
    # return value
    structure(out, class = "marginslist")
}

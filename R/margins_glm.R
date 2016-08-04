#' @title Marginal Effects for Generalized Linear Models
#' @description Calculate marginal effects from estimated generalized linear models
#' @param model A model object of class \dQuote{glm}.
#' @param data A data.frame containing the data at which to evaluate the marginal effects, as in \code{\link[stats]{predict}}.
#' @param at A list of one or more named vectors, specifically values at which to calculate the marginal effects. See \code{\link{build_datalist}} for details on use.
#' @param atmeans A logical indicating whether to calculate marginal effects at the means (i.e., partial effects at the average of all covariates), as opposed to the default average marginal effects (i.e., average partial effects), which is the default.
#' @param \dots Arguments passed to \code{\link{marginal_effects}}. One of particular relevance for GLMs is \code{type}.
#' @details Calculates marginal effects for a linear model object using \code{\link{marginal_effects}} and returns a \dQuote{marginslist} list of one or more \dQuote{margins} objects.
#' @return An object of class \dQuote{marginslist}, composed of one or more objects of class \dQuote{margins}.
#' @author Thomas J. Leeper
#' @references
#' Greene, W.H. 2012. Econometric Analysis, 7th Ed. Boston: Pearson.
#' 
#' Stata manual: \code{margins}. Retrieved 2014-12-15 from \url{http://www.stata.com/manuals13/rmargins.pdf}.
#' @seealso \code{\link{margins.glm}}, \code{\link{plot.margins}}, \code{\link{extract_marginal_effects}}
#' @examples
#' # basic examples
#' require("datasets")
#' x <- glm(mpg ~ cyl * hp + wt, data = mtcars)
#' margins(x, type = "response")
#' margins(x, type = "link")
#'
#' @export
margins.glm <- 
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

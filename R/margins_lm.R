#' @title Marginal Effects for OLS Regression
#' @description Calculate marginal effects from estimated linear models
#' @param x A model object of class \dQuote{lm}.
#' @param newdata A data.frame containing the data at which to evaluate the marginal effects, as in \code{\link[stats]{predict}}.
#' @param at A list of one or more named vectors, specifically values at which to calculate the marginal effects. See \code{\link{build_datalist}} for details on use.
#' @param atmeans A logical indicating whether to calculate marginal effects at the means (i.e., partial effects at the average of all covariates), as opposed to the default average marginal effects (i.e., average partial effects), which is the default.
#' @param \dots Arguments passed to \code{\link{marginal_effect}}.
#' @details Calculate marginal effects for a linear model object.
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
#' x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
#' margins(x)
#'
#' @export
margins.lm <- 
function(x, 
         newdata, 
         at = NULL, 
         atmeans = FALSE, 
         ...){
    
    # setup data
    if (missing(newdata)) {
        if (!is.null(x[["call"]][["data"]])) {
            newdata <- eval(x[["call"]][["data"]], parent.frame()) 
        } else { 
            newdata <- get_all_vars(x[["terms"]], data = x[["model"]])
        }
    }
    data_list <- build_datalist(newdata, at = at, atmeans = atmeans)
    
    # reduce memory profile
    x[["model"]] <- NULL
    attr(x[["terms"]], ".Environment") <- NULL
    
    # calculate marginal effects
    out <- lapply(data_list, function(thisdata) {
        m <- marginal_effect(x = x, data = thisdata, atmeans = atmeans, ...)
        attr(m, "Variables") <- attributes(thisdata)[["Variables"]]
        attr(m, "at") <- attributes(thisdata)[["at"]]
        m
    })
    
    # return value
    structure(out, class = "marginslist")
}

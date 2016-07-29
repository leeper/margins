#' @rdname margins.plm
#' @title Marginal Effects for Panel Regression Models
#' @description Calculate marginal effects from estimated panel linear and panel generalized linear models
#' @param x A model object of class \dQuote{plm} or \dQuote{pglm}, from the \pkg{plm} package.
#' @param newdata A data.frame containing the data at which to evaluate the marginal effects, as in \code{\link[stats]{predict}}.
#' @param at A list of one or more named vectors, specifically values at which to calculate the marginal effects. See examples.
#' @param atmeans A logical indicating whether to calculate marginal effects at the means (i.e., partial effects at the average of all covariates), as opposed to the default average marginal effects (i.e., average partial effects), which is the default.
#' @param \dots Arguments passed to \code{\link{marginal_effect}}. One of particular relevance for GLMs is \code{type}.
#' @seealso \code{\link{margins.lm}}, \code{\link{margins.glm}}, \code{\link{plot.margins}}
#' @export
margins.plm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         ...) {
    # calculate marginal effects
    if(is.null(newdata)) {
        newdata <- if(!is.null(x[["call"]][["data"]])) eval(x[["call"]][["data"]]) else x[["model"]]
    }
    data_list <- at_builder(newdata, terms = terms(x[["formula"]]), at = at, atmeans = atmeans)
    
    # FOR SOME REASON THIS ISN'T CAPTURING INTERACTION TERMS
    if(x[["args"]][["model"]] != 'pooling') {
        warning("marginal effects not likely to be correct")
        out <- lapply(data_list, marginal_effect, x = x, ...)
        class(out) <- "marginslist"
        #mm <- cbind(model.matrix(x), model.matrix(~0+attributes(x$model)$index[,1]))
    } else {
        out <- lapply(data_list, marginal_effect, x = x, ...)
        class(out) <- "marginslist"
    }
}

#' @rdname margins.plm
#' @export
margins.pglm <- 
function(x, 
         newdata = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         ...){
    # configure link function
    g <- getlink(x[["family"]][["link"]])
    dfun <- g[["dfun"]]
    
    if(is.null(newdata)) {
        newdata <- if(!is.null(x[["call"]][["data"]])) eval(x[["call"]][["data"]]) else x[["model"]]
    }
    data_list <- at_builder(newdata, terms = terms(x[["formula"]]), at = at, atmeans = atmeans)
    out <- marginal_effect(x, mm = newdata, ...)

    out
}

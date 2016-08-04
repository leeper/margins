#' @rdname margins.plm
#' @title Marginal Effects for Panel Regression Models
#' @description Calculate marginal effects from estimated panel linear and panel generalized linear models
#' @param model A model object of class \dQuote{plm} or \dQuote{pglm}, from the \pkg{plm} package.
#' @param data A data.frame containing the data at which to evaluate the marginal effects, as in \code{\link[stats]{predict}}.
#' @param at A list of one or more named vectors, specifically values at which to calculate the marginal effects. See \code{\link{build_datalist}} for details on use.
#' @param atmeans A logical indicating whether to calculate marginal effects at the means (i.e., partial effects at the average of all covariates), as opposed to the default average marginal effects (i.e., average partial effects), which is the default.
#' @param \dots Arguments passed to \code{\link{marginal_effect}}. One of particular relevance for GLMs is \code{type}.
#' @seealso \code{\link{margins.lm}}, \code{\link{margins.glm}}, \code{\link{plot.margins}}, \code{\link{extract_marginal_effects}}
#' @export
margins.plm <- 
function(model, 
         data = NULL, 
         at = NULL, 
         atmeans = FALSE, 
         ...) {
    
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
    
    # calculate marginal effects
    warning("Marginal effects not likely to be correct")
    out <- lapply(data_list, function(thisdata) {
        m <- build_margins(model = model, data = thisdata, atmeans = atmeans, ...)
        attr(m, "at") <- attributes(thisdata)[["at"]]
        attr(m, "atmeans") <- atmeans
        m
    })
    
    # return value
    structure(out, class = "marginslist")
}

#' @rdname margins.plm
#' @export
margins.pglm <- 
function(model, 
         data = NULL, 
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
    
    # calculate marginal effects
    warning("Marginal effects not likely to be correct")
    out <- lapply(data_list, function(thisdata) {
        m <- build_margins(model = model, data = thisdata, atmeans = atmeans, ...)
        attr(m, "at") <- attributes(thisdata)[["at"]]
        attr(m, "atmeans") <- atmeans
        m
    })
    
    # return value
    structure(out, class = "marginslist")
}

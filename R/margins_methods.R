#' @rdname margins
#' @importFrom prediction find_data
#' @export
margins.default <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         ...){
    
    # setup data
    data_list <- build_datalist(data, at = at)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # warn about weights
    warn_for_weights(model)
    
    # calculate marginal effects
    out <- lapply(data_list, function(thisdata) {
        m <- build_margins(model = model, data = thisdata, ...)
        attr(m, "at") <- attributes(thisdata)[["at"]]
        m
    })
    
    # return value
    structure(out, class = "marginslist")
}

#' @rdname margins
#' @export
margins.lm <- margins.default

#' @rdname margins
#' @export
margins.glm <- margins.default

#' @rdname margins
#' @export
margins.loess <- function(model, 
         data, 
         at = NULL, 
         ...){
    
    # setup data
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    data_list <- build_datalist(data, at = at)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # warn about weights
    warn_for_weights(model)
    
    # calculate marginal effects
    out <- lapply(data_list, function(thisdata) {
        m <- build_margins(model = model, data = thisdata, vcov = NULL, vce = "none", ...)
        attr(m, "at") <- attributes(thisdata)[["at"]]
        m
    })
    
    # return value
    structure(out, class = "marginslist")
}

warn_for_weights <- function(model) {
    wghts <- unname(model[["weights"]])
    if (!isTRUE(all.equal(wghts, rep(wghts[1], length(wghts))))) {
        warning("'weights' used in model estimation are currently ignored!")
    }
    NULL
}

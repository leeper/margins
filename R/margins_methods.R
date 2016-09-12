#' @rdname margins
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
    
    # calculate marginal effects
    out <- lapply(data_list, function(thisdata) {
        m <- build_margins(model = model, data = thisdata, vcov = NULL, vce = "none", ...)
        attr(m, "at") <- attributes(thisdata)[["at"]]
        m
    })
    
    # return value
    structure(out, class = "marginslist")
}

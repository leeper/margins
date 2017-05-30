# @rdname margins
# @export
margins.merMod <- function(model, 
         data = find_data(model), 
         variables = NULL,
         at = NULL, 
         ...){
    
    # setup data
    data_list <- build_datalist(data, at = at)
    
    # calculate marginal effects
    out <- lapply(data_list, function(thisdata) {
        m <- build_margins(model = model, variables = variables, data = thisdata, vce = "none", ...)
        attr(m, "at") <- attributes(thisdata)[["at"]]
        m
    })
    
    # return value
    structure(do.call("rbind", out), 
              class = c("margins", "data.frame"),
              at = if (is.null(at)) at else names(at),
              type = NULL,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              vce = "none", 
              iterations = NULL)
}

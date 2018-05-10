# @rdname margins
# @export
margins.lme <- function(model, 
         data = find_data(model), 
         variables = NULL,
         at = NULL, 
         ...){
    
    # setup data
    data_list <- build_datalist(data, at = at)
    at_specification <- attr(data_list, "at_specification")
    
    # calculate marginal effects
    out <- list()
    for (i in seq_along(data_list)) {
        out[[i]] <- build_margins(model = model, variables = variables, data = data_list[[i]], vce = "none", ...)
        out[[i]][["_at_number"]] <- i
    }
    
    # return value
    structure(do.call("rbind", out), 
              class = c("margins", "data.frame"),
              at = if (is.null(at)) NULL else at_specification,
              type = NULL,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              vce = "none", 
              vcov = NULL,
              jacobian = NULL,
              weighted = FALSE,
              iterations = NULL)
}

# @rdname margins
# @export
margins.nlme <- margins.lme

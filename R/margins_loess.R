#' @rdname margins
#' @export
margins.loess <- 
function(model, 
         data, 
         variables,
         at = NULL, 
         eps = 1e-7,
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
    out <- list()
    for (i in seq_along(data_list)) {
        out[[i]] <- build_margins(model = model, 
                                  data = data_list[[i]], 
                                  variables = variables,
                                  type = "response", 
                                  vcov = NULL, 
                                  vce = "none", 
                                  unit_ses = FALSE, 
                                  eps = eps, 
                                  ...)
    }
    
    # return value
    structure(do.call("rbind", out), 
              class = c("margins", "data.frame"), 
              at = if (is.null(at)) at else names(at),
              type = "response",
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              vce = "none", 
              iterations = NULL)
}

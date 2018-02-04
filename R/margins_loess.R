#' @rdname margins
#' @export
margins.loess <- 
function(model, 
         data, 
         variables = NULL,
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
    
    # identify classes of terms in `model`
    varslist <- find_terms_in_model(model, variables = variables)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
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
                                  varslist = varslist,
                                  ...)
    }
    
    # return value
    structure(do.call("rbind", out), 
              class = c("margins", "data.frame"), 
              at = if (is.null(at)) at else names(at),
              type = "response",
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              vce = "none", 
              vcov = stats::setNames(lapply(out, attr, "vcov"), names(data_list)),
              weighted = FALSE,
              iterations = NULL)
}

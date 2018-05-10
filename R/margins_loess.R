#' @rdname margins
#' @export
margins.loess <- 
function(model, 
         data, 
         variables = NULL,
         at = NULL, 
         vce = "none", 
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
    at_specification <- attr(data_list, "at_specification")
    
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
        out[[i]][["_at_number"]] <- i
    }
    
    # return value
    structure(do.call("rbind", out), 
              class = c("margins", "data.frame"), 
              at = if (is.null(at)) NULL else at_specification,
              type = "response",
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              vce = "none", 
              vcov = NULL,
              jacobian = NULL,
              weighted = FALSE,
              iterations = NULL)
}

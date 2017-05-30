#' @rdname margins
#' @export
margins.nnet <- 
function(model, 
         data = find_data(model, parent.frame()), 
         variables = NULL,
         at = NULL, 
         vce = "none",
         eps = 1e-7,
         ...) {
    
    # match.arg()
    vce <- match.arg(vce)
    
    # setup data
    data_list <- build_datalist(data, at = at)
    if (is.null(names(data_list))) {
        names(data_list) <- NA_character_
    }
    
    # warn about weights
    warn_for_weights(model)
    
    # calculate marginal effects
    out <- list()
    for (i in seq_along(data_list)) {
        out[[i]] <- build_margins(model = model, 
                                  data = data_list[[i]], 
                                  variables = variables,
                                  type = NULL, 
                                  vce = vce, 
                                  vcov = NULL,
                                  unit_ses = FALSE, 
                                  eps = eps, 
                                  ...)
    }
    
    # return value
    structure(do.call("rbind", out), 
              class = c("margins", "data.frame"), 
              at = if (is.null(at)) at else names(at),
              type = NULL,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              vce = vce, 
              iterations = NULL)
}

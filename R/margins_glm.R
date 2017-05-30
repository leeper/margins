#' @rdname margins
#' @export
margins.glm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         variables = NULL,
         at = NULL, 
         type = c("response", "link", "terms"),
         vcov = stats::vcov(model),
         vce = c("delta", "simulation", "bootstrap", "none"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         unit_ses = FALSE,
         eps = 1e-7,
         ...) {
    
    # match.arg()
    type <- match.arg(type)
    vce <- match.arg(vce)
    
    # setup data
    data_list <- build_datalist(data, at = at)
    if (is.null(names(data_list))) {
        names(data_list) <- NA_character_
    }
    
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
                                  type = type, 
                                  vcov = vcov, 
                                  vce = vce, 
                                  iterations = iterations, 
                                  unit_ses = unit_ses, 
                                  eps = eps, 
                                  ...)
    }
    
    # return value
    structure(do.call("rbind", out), 
              class = c("margins", "data.frame"), 
              at = if (is.null(at)) at else names(at),
              type = type,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              vce = vce, 
              iterations = if (vce == "bootstrap") iterations else NULL)
}

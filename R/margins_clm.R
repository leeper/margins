#' @rdname margins
#' @export
margins.clm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         variables = NULL,
         at = NULL, 
         type = c("response", "link"),
         vce = "none",
         eps = 1e-7,
         ...) {
    
    # match.arg()
    type <- match.arg(type)
    if (type == "link") {
        type <- "linear.predictor"
    }
    vce <- match.arg(vce)
    
    # setup data
    data_list <- build_datalist(data, at = at)
    if (is.null(names(data_list))) {
        names(data_list) <- NA_character_
    }
    at_specification <- attr(data_list, "at_specification")
    
    # identify classes of terms in `model`
    varslist <- find_terms_in_model(model, variables = variables)
    
    # calculate marginal effects
    out <- list()
    for (i in seq_along(data_list)) {
        out[[i]] <- build_margins(model = model, 
                                  data = data_list[[i]], 
                                  variables = variables,
                                  type = type,
                                  vce = vce, 
                                  vcov = NULL,
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
              type = NULL,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              vce = vce, 
              vcov = NULL,
              jacobian = NULL,
              weighted = FALSE,
              iterations = NULL)
}

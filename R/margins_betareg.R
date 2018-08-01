#' @rdname margins
#' @export
margins.betareg <- 
function(model, 
         data = find_data(model, parent.frame()), 
         variables = NULL,
         at = NULL, 
         type = c("response", "link"),
         vcov = stats::vcov(model, phi = FALSE),
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
                                  vcov = vcov,
                                  vce = vce,
                                  iterations = iterations,
                                  unit_ses = unit_ses,
                                  eps = eps,
                                  varslist = varslist,
                                  ...)
        out[[i]][["_at_number"]] <- i
    }
    if (vce == "delta") {
        jac <- do.call("rbind", lapply(out, attr, "jacobian"))
        rownames(jac) <- paste0(rownames(jac), ".", rep(seq_len(length(out)), each = length(unique(rownames(jac)))))
        vc <- jac %*% vcov %*% t(jac)
    } else {
        jac <- NULL
        vc <- NULL
    }
    
    # return value
    structure(do.call("rbind", out), 
              class = c("margins", "data.frame"), 
              at = if (is.null(at)) NULL else at_specification,
              type = type,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              vce = vce, 
              vcov = vc,
              jacobian = jac,
              weighted = FALSE,
              iterations = if (vce == "bootstrap") iterations else NULL)
}

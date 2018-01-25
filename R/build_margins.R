build_margins <- 
function(model, 
         data,
         variables = NULL, 
         type = c("response", "link", "terms"),
         vcov = stats::vcov(model),
         vce = c("delta", "simulation", "bootstrap", "none"),
         iterations = 50L, # if vce == "bootstrap" or "simulation"
         unit_ses = FALSE,
         weights = NULL,
         eps = 1e-7,
         varslist = NULL,
         ...) {
    
    # variables in the model
    allvars <- all.vars(terms(model))[-1]
    
    # march.arg() for arguments
    if (!is.null(type)) {
        type <- type[1L]
    }
    vce <- match.arg(vce)
    if (is.function(vcov)) {
        vcov <- vcov(model)
    }
    
    # identify classes of terms in `model`
    if (is.null(varslist)) {
        varslist <- find_terms_in_model(model, variables = variables)
    }
    
    # obtain gradient with respect to each variable in data
    if (!is.null(type)) {
        mes <- marginal_effects(model = model, data = data, variables = variables, type = type, eps = eps, varslist = varslist, ...)
    } else {
        mes <- marginal_effects(model = model, data = data, variables = variables, eps = eps, varslist = varslist, ...)
    }
    
    variables <- gsub("^dydx_", "", names(mes))
    
    # variance estimation technique
    if (vce != "none") {
        variances <- get_effect_variances(data = data, model = model, variables = variables,
                                          type = type, vcov = vcov, vce = vce,
                                          iterations = iterations, weights = weights, eps = eps,
                                          varslist = varslist, ...)
    }
    
    # get unit-specific effect variances (take derivative of `.build_grad_fun()` for every row separately)
    if ((vce == "delta") && (isTRUE(unit_ses))) {
        vmat <- do.call("rbind", lapply(seq_len(nrow(data)), function(datarow) {
            delta_once(data = data[datarow,], model = model, variables = variables,
                       type = type, vcov = vcov, vce = vce, weights = weights,
                       eps = eps, varslist = varslist, ...)
        }))
        colnames(vmat) <- paste0("SE_", names(mes))
        vmat <- as.data.frame(vmat)
        vmat[] <- lapply(vmat, sqrt)
    }
    
    # obtain predicted values and standard errors
    if (!is.null(type)) {
        pred <- prediction(model = model, data = data, type = type, ...)
    } else {
        pred <- prediction(model = model, data = data, ...)
    }
    
    # setup output structure
    if ((vce == "delta") && (isTRUE(unit_ses))) {
        out <- cbind(pred, mes, variances, vmat)
    } else if (vce == "none") { 
        out <- cbind(pred, mes)
    } else { 
        out <- cbind(pred, mes, variances)
    }
    
    if (is.null(weights)) {
        out[["_weights"]] <- NA_real_
    } else {
        out[["_weights"]] <- weights
    }
    
    structure(out, 
              class = "data.frame", 
              row.names = seq_len(nrow(pred)))
}

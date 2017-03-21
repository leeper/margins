#' @rdname margins
#' @export
margins.default <- 
function(model, 
         data = find_data(model, parent.frame()), 
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

#' @rdname margins
#' @export
margins.lm <- margins.default

#' @rdname margins
#' @export
margins.glm <- margins.default

#' @rdname margins
#' @export
margins.loess <- 
function(model, 
         data, 
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

# @rdname margins
# @export
margins.merMod <- function(model, 
         data = find_data(model), 
         at = NULL, 
         ...){
    
    # setup data
    data_list <- build_datalist(data, at = at)
    
    # calculate marginal effects
    out <- lapply(data_list, function(thisdata) {
        m <- build_margins(model = model, data = thisdata, vce = "none", ...)
        attr(m, "at") <- attributes(thisdata)[["at"]]
        m
    })
    
    # return value
    structure(do.call("rbind", out), class = c("margins", "data.frame"))
}

warn_for_weights <- function(model) {
    wghts <- unname(model[["weights"]])
    if (!isTRUE(all.equal(wghts, rep(wghts[1], length(wghts))))) {
        warning("'weights' used in model estimation are currently ignored!")
    }
    NULL
}

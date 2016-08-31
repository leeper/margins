#' @rdname marginal_effects
#' @export
marginal_effects.lm <- function(model, data, type = c("response", "link"), eps = 1e-4, ...) {
    
    # setup data, if missing
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    
    type <- match.arg(type)
    
    # extract term names from `model`
    term_names <- all.vars(terms(model))
    
    # identify classes of terms in `model`
    classes <- attributes(terms(model))[["dataClasses"]][-1]
    classes[classes == "character"] <- "factor"
    ## cleanup names of terms
    terms2 <- sapply(names(classes), function(x) all.vars(parse(text = x)))
    names(classes)[names(classes) %in% names(terms2)] <- terms2[names(classes) %in% names(terms2)]
    
    # identify factors versus numeric terms in `model`
    nnames <- clean_terms(names(classes)[classes != "factor"])
    fnames <- clean_terms(names(classes)[classes == "factor"])
    fnames2 <- names(classes)[classes == "factor"] # for checking stupid variable naming behavior by R
    
    # setup obs-x-term data.frame of obs-specific marginal effects
    out <- structure(matrix(NA_real_, nrow = nrow(data), ncol = length(nnames)), 
                     rownames = seq_len(nrow(data)))
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    for (i in seq_along(nnames)) {
        out[, i] <- get_instant_pdiff(data = data, model = model, variable = nnames[i], type = type, eps = eps)
    }
    out <- setNames(as.data.frame(out, optional = TRUE), nnames)
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    if (any(classes == "factor")) {
        for (i in seq_along(fnames)) {
            out <- cbind(out, get_factor_pdiff(data = data, model = model, fnames[i], type = type, fwrap = (fnames != fnames2)[i]))
        }
    }
    for (i in seq_along(out)) {
        class(out[[i]]) <- c("marginaleffect", "numeric")
    }
    return(out)
}

#' @rdname marginal_effects
#' @export
marginal_effects.glm <- marginal_effects.lm

#' @rdname marginal_effects
#' @export
marginal_effects.loess <- marginal_effects.lm

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
    nnames <- clean_terms(names(classes)[!classes %in% c("factor", "logical")])
    lnames <- clean_terms(names(classes)[classes == "logical"])
    fnames <- clean_terms(names(classes)[classes == "factor"])
    fnames2 <- names(classes)[classes == "factor"] # for checking stupid variable naming behavior by R
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    # add discrete differences for logical terms
    out1 <- lapply(c(nnames, lnames), mfx, data = data, model = model, type = type, eps = eps)
    
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    out2 <- list()
    for (i in seq_along(fnames)) {
        out2[[i]] <- mfx.factor(data = data, model = model, fnames[i], type = type, fwrap = (fnames != fnames2)[i])
    }
    
    out <- c(out1, out2)
    out <- do.call("cbind.data.frame", out[vapply(out, function(x) length(x) > 0, FUN.VALUE = logical(1))])
    return(out)
}

#' @rdname marginal_effects
#' @export
marginal_effects.glm <- marginal_effects.lm

#' @rdname marginal_effects
#' @export
marginal_effects.loess <- marginal_effects.lm

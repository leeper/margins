#' @rdname marginal_effects
#' @importFrom prediction find_data
#' @export
marginal_effects.default <- 
function(model, 
         data = find_data(model, parent.frame()), 
         type = c("response", "link"), 
         eps = 1e-7, 
         ...) {
    
    type <- match.arg(type)
    
    # extract term names from `model`
    term_names <- all.vars(terms(model))
    
    # identify classes of terms in `model`
    classes <- attributes(terms(model))[["dataClasses"]][-1]
    # drop specially named "(weights)" variables
    if (!is.null(model[["weights"]])) {
        classes <- classes[!names(classes) %in% "(weights)"]
    }
    # handle character variables as factors
    classes[classes == "character"] <- "factor"
    ## cleanup names of terms
    terms2 <- sapply(names(classes), function(x) all.vars(parse(text = x)))
    names(classes)[names(classes) %in% names(terms2)] <- terms2[names(classes) %in% names(terms2)]
    
    # identify factors versus numeric terms in `model`
    nnames <- clean_terms(names(classes)[!classes %in% c("factor", "ordered", "logical")])
    lnames <- clean_terms(names(classes)[classes == "logical"])
    fnames <- clean_terms(names(classes)[classes %in% c("factor", "ordered")])
    fnames2 <- names(classes)[classes %in% c("factor", "ordered")] # for checking stupid variable naming behavior by R
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    # add discrete differences for logical terms
    out1 <- lapply(c(nnames, lnames), dydx, data = data, model = model, type = type, eps = eps, ...)
    
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    out2 <- list()
    for (i in seq_along(fnames)) {
        out2[[i]] <- dydx.factor(data = data, model = model, fnames[i], type = type, fwrap = (fnames != fnames2)[i], ...)
    }
    
    out <- c(out1, out2)
    out <- do.call("cbind.data.frame", out[vapply(out, function(x) length(x) > 0, FUN.VALUE = logical(1))])
    return(out)
}

#' @rdname marginal_effects
#' @export
marginal_effects.margins <- function(model, data, ...) {
    w <- which(sapply(model, inherits, what = "marginaleffect"))
    out <- model[, w, drop = FALSE]
    attributes(out) <- attributes(model)[names(attributes(model)) != "names"]
    names(out) <- names(model)[w]
    out
}

#' @rdname marginal_effects
#' @export
marginal_effects.marginslist <- function(model, data, ...) {
    lapply(model, marginal_effects, ...)
}


#' @rdname marginal_effects
#' @export
marginal_effects.lm <- marginal_effects.default

#' @rdname marginal_effects
#' @export
marginal_effects.glm <- marginal_effects.default

#' @rdname marginal_effects
#' @export
marginal_effects.loess <- marginal_effects.default

# @rdname marginal_effects
# @importFrom prediction find_data
# @export
marginal_effects.merMod <- 
function(model, 
         data = find_data(model), 
         type = c("response", "link"), 
         eps = 1e-7, 
         ...) {
    
    type <- match.arg(type)
    
    # extract term names from `model`
    term_names <- all.vars(terms(model))
    
    # identify classes of terms in `model`
    nnames <- colnames(attributes(terms(model))[["factors"]])
    lnames <- NULL
    fnames <- NULL
    classes <- rep("numeric", length(nnames))
    warning("factor variables are not handled as factor for models of class 'merMod'")
    
    # estimate numerical derivatives with respect to each variable (for numeric terms in the model)
    # add discrete differences for logical terms
    out1 <- lapply(c(nnames, lnames), dydx, data = data, model = model, type = type, eps = eps, ...)
    
    # add discrete differences for factor terms
    ## exact number depends on number of factor levels
    out2 <- list()
    for (i in seq_along(fnames)) {
        out2[[i]] <- dydx.factor(data = data, model = model, fnames[i], type = type, fwrap = (fnames != fnames2)[i], ...)
    }
    
    out <- c(out1, out2)
    out <- do.call("cbind.data.frame", out[vapply(out, function(x) length(x) > 0, FUN.VALUE = logical(1))])
    return(out)
}

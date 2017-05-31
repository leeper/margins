find_terms_in_model <- function(model, variables = NULL) {
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
    vars <- list(
      nnames = clean_terms(names(classes)[!classes %in% c("factor", "ordered", "logical")]),
      lnames = clean_terms(names(classes)[classes == "logical"]),
      fnames = clean_terms(names(classes)[classes %in% c("factor", "ordered")]),
      fnames2 = names(classes)[classes %in% c("factor", "ordered")] # for checking stupid variable naming behavior by R
    )
    
    # subset of variables for which to compute the marginal effects
    if (!is.null(variables)) {
        tmp <- c(vars$nnames, vars$lnames, vars$fnames, vars$fnames2)
        if (any(!variables %in% tmp)) {
            stop("Some values in 'variables' are not in the model terms.")
        }
        vars$nnames <- vars$nnames[vars$nnames %in% variables]
        vars$lnames <- vars$lnames[vars$lnames %in% variables]
        vars$fnames <- vars$fnames[vars$fnames %in% variables]
        vars$fnames2 <- vars$fnames2[vars$fnames2 %in% variables]
    }
    
    return(vars)
}
